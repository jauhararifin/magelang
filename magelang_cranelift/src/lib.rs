mod code;
mod expr;
mod layout;

use anyhow::{bail, ensure, Context, Result};
use cranelift_codegen::ir::{types, AbiParam, InstBuilder};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module as _};
use cranelift_object::{ObjectBuilder, ObjectModule};
use magelang_typecheck::{DefId, Func, Module, Statement, TypeArgs};
use std::collections::HashMap;

use code::Codegen;
use layout::Layouts;

type FunctionKey<'a> = (DefId<'a>, Option<&'a TypeArgs<'a>>);

struct Backend<'a> {
    object: ObjectModule,
    layouts: Layouts,
    functions: HashMap<FunctionKey<'a>, FuncId>,
    globals: HashMap<DefId<'a>, DataId>,
    strings: HashMap<Vec<u8>, DataId>,
}

/// Compile the typed module directly to a host-native object containing a C `main` entry point.
pub fn generate<'a>(module: &'a Module<'a>, optimize: bool) -> Result<Vec<u8>> {
    ensure!(module.is_valid, "cannot generate native code for an invalid module");
    ensure!(cfg!(any(target_os = "linux", target_os = "macos")), "native codegen currently supports Linux and macOS");
    let mut settings = settings::builder();
    settings.set("opt_level", if optimize { "speed" } else { "none" })?;
    settings.set("is_pic", "true")?;
    let isa = cranelift_native::builder().map_err(anyhow::Error::msg)?.finish(settings::Flags::new(settings))?;
    let object = ObjectModule::new(ObjectBuilder::new(isa, "magelang", cranelift_module::default_libcall_names())?);
    let mut backend = Backend {
        layouts: Layouts { pointer: object.target_config().pointer_type() },
        object,
        functions: HashMap::new(),
        globals: HashMap::new(),
        strings: HashMap::new(),
    };
    let mut main = None;
    let mut definitions = Vec::new();
    let mut symbols = HashMap::new();
    for func in module.packages.iter().flat_map(|package| &package.functions) {
        let (import, intrinsic, is_main) = annotations(func).with_context(|| format!("in {}", func.name))?;
        if is_main {
            ensure!(main.is_none(), "multiple functions annotated with @main()");
            let ty = func.ty.as_func().unwrap();
            ensure!(
                func.typeargs.is_none() && ty.params.is_empty() && ty.return_type.is_void(),
                "@main() requires a non-generic function with no parameters or return value: {}",
                func.name
            );
        }
        if let Some(symbol) = import {
            backend
                .layouts
                .check_ffi(func.ty.as_func().unwrap())
                .with_context(|| format!("in native import {}", func.name))?;
            ensure!(symbol != "main" && !symbol.starts_with("__magelang_"), "reserved native symbol: {symbol}");
            ensure!(!symbol.is_empty() && !symbol.contains('\0'), "invalid native import symbol");
            ensure!(symbols.insert(symbol, func.name).is_none(), "duplicate native import: {symbol}");
        }
        let name = import.map(str::to_owned).unwrap_or_else(|| format!("__magelang_fn_{}", definitions.len()));
        let signature = backend.layouts.signature(&backend.object, func.ty.as_func().unwrap())?;
        let id = backend.object.declare_function(
            &name,
            if import.is_some() { Linkage::Import } else { Linkage::Local },
            &signature,
        )?;
        backend.functions.insert((func.name, func.typeargs), id);
        if is_main {
            main = Some(id);
        }
        definitions.push((func, id, import.is_some(), intrinsic));
    }
    let main = main.context("native executables require a function annotated with @main()")?;
    for (index, global) in module.packages.iter().flat_map(|package| &package.globals).enumerate() {
        ensure!(
            global.annotations.is_empty(),
            "global annotations are not yet supported by the native backend: {}",
            global.name
        );
        let layout = backend.layouts.get(global.ty)?;
        let id = backend.object.declare_data(&format!("__magelang_global_{index}"), Linkage::Local, true, false)?;
        let mut data = DataDescription::new();
        data.define_zeroinit(layout.size.max(1) as usize);
        data.set_align(layout.align as u64);
        backend.object.define_data(id, &data)?;
        backend.globals.insert(global.name, id);
    }
    for (func, id, imported, intrinsic) in definitions {
        if imported {
            continue;
        }
        let mut context = backend.object.make_context();
        context.func.signature = backend.layouts.signature(&backend.object, func.ty.as_func().unwrap())?;
        let mut builder_context = FunctionBuilderContext::new();
        let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
        let mut code = Codegen::new(&mut backend, builder);
        code.function(func, intrinsic).with_context(|| format!("in {}", func.name))?;
        code.finish();
        backend.object.define_function(id, &mut context).with_context(|| format!("compiling {}", func.name))?;
    }
    let mut context = backend.object.make_context();
    context.func.signature.returns.push(AbiParam::new(types::I32));
    let entry = backend.object.declare_function("main", Linkage::Export, &context.func.signature)?;
    let mut builder_context = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let mut code = Codegen::new(&mut backend, builder);
    let globals: HashMap<_, _> =
        module.packages.iter().flat_map(|package| &package.globals).map(|g| (g.name, g)).collect();
    for name in &module.global_init_order {
        let global = globals.get(name).context("missing global initializer")?;
        let value = code.expr(&global.value).with_context(|| format!("initializing {}", global.name))?;
        let address = code.global_address(global.name);
        code.store(global.ty, address, &value)?;
    }
    let main = code.backend.object.declare_func_in_func(main, code.builder.func);
    code.builder.ins().call(main, &[]);
    let zero = code.builder.ins().iconst(types::I32, 0);
    code.builder.ins().return_(&[zero]);
    code.finish();
    backend.object.define_function(entry, &mut context)?;
    backend.object.finish().emit().context("writing native object")
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
            "wasm_export" => {
                ensure!(args.len() == 1, "@wasm_export expects one name");
            }
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
