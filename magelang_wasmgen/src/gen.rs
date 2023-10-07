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
) -> wasm::Module<'ctx> {
    let ctx = Context {
        arena,
        files: file_manager,
        errors: error_manager,
        module,
    };

    let data_manager = DataManager::build(ctx);
    let type_manager = TypeManager::build(ctx);

    let functions = setup_functions(&ctx, &type_manager);

    todo!();
}
