use crate::data::Data;
use crate::layout::LayoutManager;
use crate::var::VarMapper;
use magelang_typecheck::Module;
use wasm_helper as wasm;

pub fn generate<'ctx>(module: Module<'ctx>) -> wasm::Module {
    let data = Data::build(&module);
    let layout_manager = LayoutManager::default();
    let var_mapper = VarMapper::build(&module);

    todo!();
}
