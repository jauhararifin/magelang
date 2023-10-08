use crate::context::Context;
use crate::data::DataManager;
use crate::func::{build_intrinsic_func, setup_functions, FuncManager};
use crate::ty::TypeManager;
use crate::var::GlobalManager;
use bumpalo::Bump;
use magelang_syntax::{ErrorReporter, FileManager};
use magelang_typecheck::Module;
use wasm_helper as wasm;

pub fn generate<'ctx>(
    arena: &'ctx Bump,
    file_manager: &'ctx mut FileManager,
    error_manager: &'ctx impl ErrorReporter,
    module: &'ctx Module<'ctx>,
) -> Option<wasm::Module<'ctx>> {
    let ctx = Context {
        arena,
        files: file_manager,
        errors: error_manager,
        module,
    };

    let data_manager = DataManager::build(ctx);
    let type_manager = TypeManager::build(ctx);
    let global_manager = GlobalManager::build(module);

    let functions = setup_functions(&ctx, &type_manager);

    if ctx.errors.has_errors() {
        return None;
    }

    let func_manager = FuncManager::new(&functions);

    let mut module_functions = Vec::default();
    for func in &functions {
        if func.intrinsic.is_some() {
            let result = build_intrinsic_func(&type_manager, &data_manager, func);
            module_functions.push(result);
        } else if func.body.is_some() {
            todo!();
        }
    }

    if ctx.errors.has_errors() {
        return None;
    }

    let func_elems = func_manager.func_elems;
    let func_table = wasm::Table {
        limits: wasm::Limits {
            min: module_functions.len() as u32,
            max: None,
        },
        ref_type: wasm::RefType::FuncRef,
    };
    let opaque_table = wasm::TableType {
        limits: wasm::Limits { min: 32, max: None },
        ref_type: wasm::RefType::ExternRef,
    };
    let tables = vec![func_table, opaque_table];

    let data = data_manager.take();

    let min_page = data.num_pages;
    let mems = vec![wasm::Mem {
        min: min_page as u32,
        max: None,
    }];

    Some(wasm::Module {
        types: type_manager.take(),
        funcs: module_functions,
        tables,
        mems,
        globals: global_manager.take(),
        elems: func_elems,
        datas: data.datas,
        start: func_manager.main_func,
        imports: func_manager.imports,
        exports: func_manager.exports,
    })
}
