use crate::data::Data;
use crate::func::FuncManager;
use crate::layout::LayoutManager;
use crate::var::{GlobalMapper, LocalManager};
use bumpalo::Bump;
use magelang_typecheck::Module;
use wasm_helper as wasm;

pub fn generate<'ctx>(arena: &'ctx Bump, module: Module<'ctx>) -> wasm::Module<'ctx> {
    let data = Data::build(arena, &module);
    let layout_manager = LayoutManager::default();
    let locals = LocalManager::default();
    let globals = GlobalMapper::build(&module);
    let func_manager = FuncManager::build(&data, &layout_manager, &locals, &globals, &module);

    let min_page = data.get_min_page();
    let mems = vec![wasm::Mem {
        min: min_page,
        max: None,
    }];

    let mut exports = Vec::<wasm::Export>::default();
    exports.push(wasm::Export {
        name: "memory".to_string(),
        desc: wasm::ExportDesc::Mem(0),
    });

    let func_table = func_manager.get_wasm_table();
    let func_types = func_manager.get_types();
    let func_elems = func_manager.func_elems;
    let func_imports = func_manager.imports;
    let func_exports = func_manager.exports;
    let functions = func_manager.functions;
    let start = func_manager.starting_point;

    exports.extend(func_exports);

    let opaque_table = wasm::TableType {
        limits: wasm::Limits { min: 32, max: None },
        ref_type: wasm::RefType::ExternRef,
    };
    let tables = vec![func_table, opaque_table];

    wasm::Module {
        types: func_types,
        funcs: functions,
        tables,
        mems,
        globals: globals.take(),
        elems: func_elems,
        datas: data.take(),
        start,
        imports: func_imports,
        exports,
    }
}
