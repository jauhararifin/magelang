use crate::data::Data;
use magelang_typecheck::Module;
use wasm_helper as wasm;

pub fn generate<'ctx>(module: Module<'ctx>) -> wasm::Module {
    let data = Data::build(&module);

    todo!();
}
