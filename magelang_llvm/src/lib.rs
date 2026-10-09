mod code;
mod expr;
mod layout;

use anyhow::{bail, ensure, Context, Result};
use magelang_typecheck::{DefId, Func, Module, Statement, TypeArgs};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use code::Codegen;
use layout::{check_ffi, layout, signature, Value};

type FunctionKey<'a> = (DefId<'a>, Option<&'a TypeArgs<'a>>);

struct Backend<'a> {
    functions: HashMap<FunctionKey<'a>, Value>,
    globals: HashMap<DefId<'a>, Value>,
    strings: HashMap<Vec<u8>, Value>,
    data: String,
}

/// The LLVM backend currently uses the 64-bit host's native data layout and C ABI.
pub fn host_triple() -> Result<&'static str> {
    match (std::env::consts::ARCH, std::env::consts::OS) {
        ("aarch64", "macos") => Ok("arm64-apple-darwin"),
        ("x86_64", "macos") => Ok("x86_64-apple-darwin"),
        ("aarch64", "linux") => {
            Ok(if cfg!(target_env = "musl") { "aarch64-unknown-linux-musl" } else { "aarch64-unknown-linux-gnu" })
        }
        ("x86_64", "linux") => {
            Ok(if cfg!(target_env = "musl") { "x86_64-unknown-linux-musl" } else { "x86_64-unknown-linux-gnu" })
        }
        _ => bail!("LLVM codegen currently supports x86-64 and ARM64 hosts on Linux and macOS"),
    }
}

/// Lower the typed module directly to LLVM IR, including a C `main` entry point.
/// The IR uses opaque pointers and can be compiled with Clang/LLVM 15 or newer.
pub fn generate<'a>(module: &'a Module<'a>) -> Result<String> {
    ensure!(module.is_valid, "cannot generate LLVM code for an invalid module");
    let triple = host_triple()?;
    let mut backend =
        Backend { functions: HashMap::new(), globals: HashMap::new(), strings: HashMap::new(), data: String::new() };
    let mut main = None;
    let mut definitions = Vec::new();
    let mut symbols = HashSet::new();
    let mut declarations = String::from("declare void @llvm.trap() cold noreturn nounwind\n");
    for bits in [32, 64] {
        let ty = layout::Scalar::Float(bits);
        for operation in ["floor", "ceil", "trunc"] {
            writeln!(declarations, "declare {ty} @llvm.{operation}.f{bits}({ty})").unwrap();
        }
    }
    for func in module.packages.iter().flat_map(|package| &package.functions) {
        let (import, intrinsic, is_main) = annotations(func).with_context(|| format!("in {}", func.name))?;
        let ty = func.ty.as_func().unwrap();
        if is_main {
            ensure!(main.is_none(), "multiple functions annotated with @main()");
            ensure!(
                func.typeargs.is_none() && ty.params.is_empty() && ty.return_type.is_void(),
                "@main() requires a non-generic function with no parameters or return value: {}",
                func.name
            );
        }
        let signature = signature(ty).with_context(|| format!("in {}", func.name))?;
        let name = if let Some(import) = import {
            check_ffi(ty).with_context(|| format!("in native import {}", func.name))?;
            ensure!(
                import != "main" && !import.starts_with("__magelang_") && !import.starts_with("llvm."),
                "reserved native symbol: {import}"
            );
            ensure!(!import.is_empty() && !import.contains('\0'), "invalid native import symbol");
            ensure!(symbols.insert(import), "duplicate native import: {import}");
            let name = symbol(import);
            let params = signature.params.iter().map(|ty| ty.parameter("")).collect::<Vec<_>>().join(", ");
            writeln!(declarations, "declare {} {name}({params})", signature.result()).unwrap();
            name
        } else {
            format!("@__magelang_fn_{}", definitions.len())
        };
        backend.functions.insert((func.name, func.typeargs), Value::pointer(name.clone()));
        if is_main {
            main = Some(name.clone());
        }
        definitions.push((func, name, import.is_some(), intrinsic));
    }
    let main = main.context("native executables require a function annotated with @main()")?;
    for (index, global) in module.packages.iter().flat_map(|package| &package.globals).enumerate() {
        ensure!(
            global.annotations.is_empty(),
            "global annotations are not yet supported by the native backend: {}",
            global.name
        );
        let layout = layout(global.ty).with_context(|| format!("in global {}", global.name))?;
        let name = format!("@__magelang_global_{index}");
        writeln!(
            backend.data,
            "{name} = internal global [{} x i8] zeroinitializer, align {}",
            layout.size.max(1),
            layout.align
        )
        .unwrap();
        backend.globals.insert(global.name, Value::pointer(name));
    }
    let mut functions = String::new();
    for (func, name, imported, intrinsic) in definitions {
        if imported {
            continue;
        }
        let mut code = Codegen::new(&mut backend);
        code.function(func, &name, intrinsic).with_context(|| format!("in {}", func.name))?;
        functions.push_str(&code.finish());
    }
    let globals: HashMap<_, _> =
        module.packages.iter().flat_map(|package| &package.globals).map(|g| (g.name, g)).collect();
    let mut code = Codegen::new(&mut backend);
    code.header = "define i32 @main()".into();
    for name in &module.global_init_order {
        let global = globals.get(name).context("missing global initializer")?;
        let value = code.expr(&global.value).with_context(|| format!("initializing {}", global.name))?;
        let address = code.backend.globals[name].clone();
        code.store(global.ty, &address, &value)?;
    }
    code.emit(format!("call void {main}()"));
    code.emit("ret i32 0");
    functions.push_str(&code.finish());
    Ok(format!("; Magelang LLVM backend\nsource_filename = \"magelang\"\ntarget triple = \"{triple}\"\n\n{declarations}\n{}\n{functions}", backend.data))
}

fn symbol(name: &str) -> String {
    let mut result = String::from("@\"");
    for byte in name.bytes() {
        if (32..=126).contains(&byte) && byte != b'"' && byte != b'\\' {
            result.push(byte as char);
        } else {
            write!(result, "\\{byte:02X}").unwrap();
        }
    }
    result.push('"');
    result
}

fn annotations<'a>(func: &'a Func<'_>) -> Result<(Option<&'a str>, Option<&'a str>, bool)> {
    let mut import = None;
    let mut intrinsic = None;
    let mut main = false;
    for annotation in func.annotations.iter() {
        let args = &annotation.arguments;
        match annotation.name.as_str() {
            "native_import" => {
                ensure!(args.len() == 1, "@native_import expects one symbol name");
                ensure!(import.is_none(), "duplicate @native_import");
                ensure!(func.typeargs.is_none(), "native imports cannot be generic");
                import = Some(args[0].as_str());
            }
            "intrinsic" => {
                ensure!(args.len() == 1, "@intrinsic expects one name");
                ensure!(intrinsic.is_none(), "duplicate @intrinsic");
                intrinsic = Some(args[0].as_str());
            }
            "main" => {
                ensure!(args.is_empty() && !main, "invalid or duplicate @main()");
                main = true;
            }
            "wasm_export" => ensure!(args.len() == 1, "@wasm_export expects one name"),
            "wasm_import" => bail!("@wasm_import is not supported by the native backend; use @native_import(\"symbol\") and native APIs instead of WASI"),
            name => bail!("unsupported native annotation @{name}"),
        }
    }
    let native = matches!(func.statement, Statement::Native);
    ensure!(
        (native && (import.is_some() ^ intrinsic.is_some())) || (!native && import.is_none() && intrinsic.is_none()),
        "a function needs a body, @native_import, or a supported @intrinsic"
    );
    ensure!(!main || !native, "@main() requires a function body");
    Ok((import, intrinsic, main))
}
