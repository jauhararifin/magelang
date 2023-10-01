use magelang_typecheck::Module;
use wasm_helper as wasm;

mod data;

use data::Data;

pub fn generate_wasm_ir<'ctx>(module: Module<'ctx>) -> wasm::Module {
    let data = Data::build(&module);

    todo!();
}

