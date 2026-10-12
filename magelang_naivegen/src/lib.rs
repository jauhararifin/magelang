//! Unoptimized Apple ARM64 code generation from Magelang's typed IR.
//!
//! Values are flattened into scalar components, each occupying a 16-byte expression
//! stack slot. Locals use native memory layout in a fixed stack frame. Mage calls
//! pass arguments and results on the stack; native imports bridge to Apple's C ABI.
//! At callee entry, SP points to the last argument component. Higher addresses hold
//! earlier arguments, the callee address, then caller-reserved result slots. This
//! keeps aggregate returns and nested calls independent of register allocation.

mod asm;
mod code;
mod expr;
mod layout;

use asm::Assembly;
use code::FunctionBuilder;
use layout::Layout;
use magelang_syntax::ErrorManager;
use magelang_typecheck::{DefId, Func, Module, Statement, TypeArgs, TypeRepr};
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Stdio};

type FuncId<'a> = (DefId<'a>, Option<&'a TypeArgs<'a>>);

pub(crate) struct Context<'a> {
    functions: HashMap<FuncId<'a>, String>,
    globals: HashMap<DefId<'a>, String>,
    data: Vec<Vec<u8>>,
}

impl Context<'_> {
    fn bytes(&mut self, bytes: &[u8]) -> String {
        let symbol = format!("Lmage_data_{}", self.data.len());
        self.data.push(bytes.to_vec());
        symbol
    }
}

/// Emit assembly for an arm64 macOS executable, including global initialization
/// and a C `main` wrapper around the optional `@main()` function.
/// Target-specific annotations unsupported by this backend are diagnosed.
pub fn generate<'a>(errors: &ErrorManager, module: &'a Module<'a>) -> Option<String> {
    if !module.is_valid || !errors.is_empty() {
        return None;
    }
    let functions: Vec<_> = module.packages.iter().flat_map(|package| &package.functions).collect();
    let globals: Vec<_> = module.packages.iter().flat_map(|package| &package.globals).collect();
    let mut ctx = Context {
        functions: functions.iter().enumerate().map(|(i, f)| ((f.name, f.typeargs), format!("Lmage_fn_{i}"))).collect(),
        globals: globals.iter().enumerate().map(|(i, g)| (g.name, format!("Lmage_global_{i}"))).collect(),
        data: vec![],
    };
    let mut assembly = Assembly::default();
    assembly.emit(".section __TEXT,__text,regular,pure_instructions");
    let mut main = None;
    for func in functions {
        let mut import = None;
        let mut intrinsic = None;
        let mut seen = HashSet::new();
        for annotation in func.annotations.iter() {
            if !seen.insert(&annotation.name) {
                errors.report(annotation.pos, format!("Duplicated annotation @{}", annotation.name));
                continue;
            }
            let arity = match annotation.name.as_str() {
                "main" => 0,
                "native_import" | "intrinsic" => 1,
                _ => {
                    errors.report(
                        annotation.pos,
                        format!("Unsupported annotation @{} in naive backend", annotation.name),
                    );
                    continue;
                }
            };
            if annotation.arguments.len() != arity {
                errors.report(annotation.pos, format!("@{} expects {arity} arguments", annotation.name));
                continue;
            }
            match annotation.name.as_str() {
                "main" => {
                    let ty = func.ty.as_func().unwrap();
                    if func.typeargs.is_some() || !ty.params.is_empty() || !ty.return_type.is_void() {
                        errors.report(func.pos, "@main requires a non-generic fn() returning void".into());
                    }
                    if main.is_some() {
                        errors.report(func.pos, "Multiple @main functions".into());
                    }
                    main = Some(ctx.functions[&(func.name, func.typeargs)].clone());
                }
                "native_import" => import = Some(annotation.arguments[0].as_str()),
                "intrinsic" => intrinsic = Some(annotation.arguments[0].as_str()),
                _ => unreachable!(),
            }
        }
        let native = matches!(func.statement, Statement::Native);
        if (import.is_some() as u8 + intrinsic.is_some() as u8) != u8::from(native) {
            errors.report(func.pos, "A bodyless function requires exactly one @native_import or @intrinsic".into());
            continue;
        }
        if let Some(symbol) = import {
            let valid_symbol = !symbol.is_empty()
                && symbol
                    .bytes()
                    .enumerate()
                    .all(|(i, b)| b == b'_' || b.is_ascii_alphabetic() || (i > 0 && b.is_ascii_digit()));
            let ty = func.ty.as_func().unwrap();
            let scalar = |ty: &magelang_typecheck::Type<'_>| {
                matches!(
                    ty.repr,
                    TypeRepr::Int(..)
                        | TypeRepr::Float(..)
                        | TypeRepr::Bool
                        | TypeRepr::Ptr(..)
                        | TypeRepr::ArrayPtr(..)
                )
            };
            if !valid_symbol
                || func.typeargs.is_some()
                || !ty.params.iter().all(|ty| scalar(ty))
                || !(ty.return_type.is_void() || scalar(ty.return_type))
            {
                errors.report(func.pos, "@native_import requires a C symbol and a non-generic scalar/pointer signature (no aggregates or callbacks)".into());
                continue;
            }
        }
        if let Some(name) = intrinsic {
            if let Err(message) = validate_intrinsic(func, name) {
                errors.report(func.pos, message);
                continue;
            }
        }
        let symbol = ctx.functions[&(func.name, func.typeargs)].clone();
        let mut builder = FunctionBuilder::new(&mut ctx, symbol, func.ty.as_func().unwrap());
        if let Some(symbol) = import {
            builder.native_import(symbol);
        } else if let Some(name) = intrinsic {
            builder.intrinsic(func, name);
        } else {
            builder.parameters();
            builder.statement(func.statement);
        }
        assembly.emit(builder.finish());
    }

    let mut embedded = HashMap::new();
    for global in &globals {
        for annotation in global.annotations.iter() {
            if annotation.name != "embed_file"
                || annotation.arguments.len() != 1
                || !global.ty.is_byte_slice()
                || !matches!(global.value.kind, magelang_typecheck::ExprKind::Zero)
                || embedded.contains_key(&global.name)
            {
                errors.report(
                    annotation.pos,
                    "Native globals only support one @embed_file(path) on an uninitialized *[u8]".into(),
                );
                continue;
            }
            match std::fs::read(&annotation.arguments[0]) {
                Ok(mut bytes) => {
                    let len = bytes.len();
                    bytes.push(0);
                    let symbol = ctx.bytes(&bytes);
                    embedded.insert(global.name, (symbol, len));
                }
                Err(error) => errors.report(annotation.pos, format!("Cannot embed file: {error}")),
            }
        }
    }
    if !errors.is_empty() {
        return None;
    }

    let global_map: HashMap<_, _> = globals.iter().map(|g| (g.name, *g)).collect();
    let mut init = FunctionBuilder::entry(&mut ctx);
    for id in &module.global_init_order {
        let global = global_map[id];
        if matches!(global.value.kind, magelang_typecheck::ExprKind::Zero) && !embedded.contains_key(id) {
            continue;
        }
        init.global_initializer(global, embedded.get(id).map(|(symbol, len)| (symbol.as_str(), *len)));
    }
    if let Some(main) = main {
        // Even a zero-argument Mage call has a slot for the function pointer.
        init.asm.symbol("x9", &main);
        init.asm.push("x9");
        init.asm.emit("    blr x9");
        init.asm.adjust_stack(16);
    }
    assembly.emit(init.finish());
    assembly.emit(".section __DATA,__data");
    for global in globals {
        assembly.emit(format!(
            ".p2align 3\n{}:\n    .space {}",
            ctx.globals[&global.name],
            Layout::of(global.ty).size.max(1)
        ));
    }
    for (index, bytes) in ctx.data.iter().enumerate() {
        assembly.emit(format!("Lmage_data_{index}:"));
        for chunk in bytes.chunks(16) {
            assembly.emit(format!("    .byte {}", chunk.iter().map(u8::to_string).collect::<Vec<_>>().join(", ")));
        }
    }
    assembly.emit(".subsections_via_symbols");
    Some(assembly.0)
}

fn validate_intrinsic(func: &Func<'_>, name: &str) -> Result<(), String> {
    let ty = func.ty.as_func().unwrap();
    let valid = match name {
        "size_of" | "align_of" => {
            func.typeargs.is_some_and(|args| args.len() == 1) && ty.params.is_empty() && ty.return_type.is_usize()
        }
        "unreachable" => func.typeargs.is_none() && ty.params.is_empty() && ty.return_type.is_void(),
        "f32.floor" | "f32.ceil" => {
            func.typeargs.is_none() && ty.params.len() == 1 && ty.params[0].is_f32() && ty.return_type.is_f32()
        }
        "f64.floor" | "f64.ceil" => {
            func.typeargs.is_none() && ty.params.len() == 1 && ty.params[0].is_f64() && ty.return_type.is_f64()
        }
        _ => return Err(format!("Unsupported intrinsic {name:?} in naive backend")),
    };
    if valid {
        Ok(())
    } else {
        Err(format!("Invalid signature for intrinsic {name:?}"))
    }
}

/// Assemble and link emitted text into a Mach-O executable using Apple's clang.
/// No optimization pass or intermediate source file is involved.
pub fn compile(assembly: &str, output: impl AsRef<Path>) -> io::Result<()> {
    let mut child = Command::new("clang")
        .args(["-target", "arm64-apple-macos11", "-x", "assembler", "-", "-o"])
        .arg(output.as_ref())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    let written = child.stdin.take().unwrap().write_all(assembly.as_bytes());
    let result = child.wait_with_output()?;
    if !result.status.success() {
        return Err(io::Error::other(format!(
            "clang failed ({}):\n{}",
            result.status,
            String::from_utf8_lossy(&result.stderr)
        )));
    }
    written
}
