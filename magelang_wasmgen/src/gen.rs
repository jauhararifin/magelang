use crate::context::Context;
use crate::data::DataManager;
use crate::func::setup_functions;
use crate::ty::TypeManager;
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

    let functions = setup_functions(&ctx, &type_manager);

    if ctx.errors.has_errors() {
        return None;
    }

    let mut exports = Vec::default();
    let mut imports = Vec::default();

    for (idx, func) in functions.iter().enumerate() {
        let func_id = idx as wasm::FuncIdx;

        if let Some(name) = func.export {
            exports.push(wasm::Export {
                name: name.to_string(),
                desc: wasm::ExportDesc::Func(func_id),
            });
        }

        if let Some((module_name, object_name)) = func.import {
            imports.push(wasm::Import {
                module: module_name.to_string(),
                name: object_name.to_string(),
                desc: wasm::ImportDesc::Func(func.type_id),
            });
        }
    }

    todo!();
}
