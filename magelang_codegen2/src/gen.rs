use crate::data::Data;
use crate::layout::LayoutManager;
use crate::var::VarMapper;
use magelang_typecheck::Module;
use wasm_helper as wasm;

pub fn generate<'ctx>(module: Module<'ctx>) -> wasm::Module {
    let data = Data::build(&module);
    let layout_manager = LayoutManager::default();
    let var_mapper = VarMapper::build(&module);

    let min_page = data.get_min_page();
    let mems = vec![wasm::Mem {
        min: min_page,
        max: None,
    }];

    wasm::Module {
        types: Vec::default(),
        funcs: Vec::default(),
        tables: Vec::default(),
        mems,
        globals: Vec::default(),
        elems: Vec::default(),
        datas: data.take(),
        start: None,
        imports: Vec::default(),
        exports: Vec::default(),
    }
}
