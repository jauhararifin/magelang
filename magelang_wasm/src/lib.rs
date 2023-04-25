use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{BinOp, Expr, ExprKind, Package, Statement, Type, TypeDisplay, TypeLoader, UnOp};
use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::rc::Rc;
use walrus::{
    ir::BinaryOp, ir::InstrSeqId, ir::UnaryOp, ActiveData, ActiveDataLocation, DataKind, FunctionBuilder, FunctionId,
    FunctionKind, InitExpr, InstrSeqBuilder, LocalId, Module, ModuleLocals, ValType,
};

pub struct Compiler<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

impl<'sym, 'typ> Compiler<'sym, 'typ> {
    pub fn new(symbol_loader: &'sym SymbolLoader, type_loader: &'typ TypeLoader) -> Self {
        Self {
            symbol_loader,
            type_loader,
        }
    }

    pub fn compile(&self, packages: Vec<Rc<Package>>, main_package: SymbolId) {
        self.compile_global_module(packages.as_slice(), main_package);
    }

    fn compile_global_module(&self, packages: &[Rc<Package>], main_package: SymbolId) {
        let mut global_module = self.build_global_module(packages, main_package);

        let filename = get_module_file_path("magelang_global");
        let filedir = filename.parent().unwrap();
        std::fs::create_dir_all(filedir).unwrap();
        global_module.emit_wasm_file(filename).expect("todo: error handling");
    }

    fn build_global_module(&self, packages: &[Rc<Package>], main_package: SymbolId) -> Module {
        let mut module = Module::default();

        // import main func
        let main_pkg_name = self.symbol_loader.get_symbol(main_package).unwrap();
        let main_func_typeid = module.types.add(&[], &[]);
        let (main_func_id, _) = module.add_import_func(&main_pkg_name, "main", main_func_typeid);

        // create __start func
        let mut builder = FunctionBuilder::new(&mut module.types, &[], &[]);
        builder.name(String::from("__start"));
        builder.func_body().call(main_func_id);
        let start_func_id = builder.finish(vec![], &mut module.funcs);
        module.start = Some(start_func_id);

        // export global memory
        let memory_id = module.memories.add_local(true, 1, None);
        module.exports.add("__mag_memory", memory_id);

        // setup data segments
        let mut string_offset_table = HashMap::new();
        let mut offset = 0usize;
        for pkg in packages {
            for (index, string) in pkg.strings.iter().enumerate() {
                let content = string.as_ref().to_vec();
                let content_len = (content.len() as u32).to_le_bytes();
                let content_offset = ((offset + 8) as u32).to_le_bytes();
                let mut data_buf = Vec::with_capacity(8 + content.len());
                data_buf.extend_from_slice(&content_offset);
                data_buf.extend_from_slice(&content_len);
                data_buf.extend_from_slice(&content);
                let data_len = data_buf.len();

                module.data.add(
                    DataKind::Active(ActiveData {
                        memory: memory_id,
                        location: ActiveDataLocation::Absolute(offset as u32),
                    }),
                    data_buf,
                );
                string_offset_table.insert((pkg.name, index), offset);
                offset += data_len;
            }
        }

        // setup global pointers
        let data_end_offset_id = module.globals.add_local(
            ValType::I32,
            false,
            InitExpr::Value(walrus::ir::Value::I32(offset as i32)),
        );
        module.exports.add("__mag_data_end", data_end_offset_id);

        let stack_size = 0x1000;
        let heap_start_offset = offset + stack_size;
        let heap_start_offset_id = module.globals.add_local(
            ValType::I32,
            false,
            InitExpr::Value(walrus::ir::Value::I32(heap_start_offset as i32)),
        );
        module.exports.add("__mag_heap_start", heap_start_offset_id);

        let stack_ptr = heap_start_offset;
        let stack_ptr_id = module.globals.add_local(
            ValType::I32,
            true,
            InitExpr::Value(walrus::ir::Value::I32(stack_ptr as i32)),
        );
        module.exports.add("__mag_stack_ptr", stack_ptr_id);

        module
    }
}

fn mangle_func(pkg_name: &str, func_name: &str) -> String {
    format!("{}.{}", pkg_name, func_name)
}

fn to_wasm_type(ty: &Type) -> ValType {
    match ty {
        Type::I64 | Type::U64 => ValType::I64,
        Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8 | Type::Bool => ValType::I32,
        Type::F64 => ValType::F64,
        Type::F32 => ValType::F32,
        Type::ArrayPtr(_) => ValType::I32,
        _ => todo!(),
    }
}

fn get_module_file_path(pkg_name: &str) -> PathBuf {
    let mut result = PathBuf::new();
    result.push(".cache/wasm/");
    for seg in pkg_name.split("/") {
        result.push(seg);
    }
    result.set_extension("wasm");
    result
}
