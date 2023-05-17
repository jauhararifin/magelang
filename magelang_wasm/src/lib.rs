use indexmap::IndexMap;
use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{
    ArrayPtrType, BinOp, Expr, ExprKind, FuncType, GlobalId, Package, PointerType, SliceType, Statement, Type, TypeId,
    TypeLoader, TypePrinter, UnOp,
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::rc::Rc;
use walrus::{
    ir::BinaryOp, ir::InstrSeqId, ir::UnaryOp, ActiveData, ActiveDataLocation, DataKind, FunctionBuilder, FunctionId,
    FunctionKind, GlobalId as WasmGlobalId, InitExpr, InstrSeqBuilder, LocalId, MemoryId, Module, ValType,
};

pub struct Compiler<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
    type_printer: &'typ TypePrinter<'sym, 'typ>,
}

const START_FUNC_NAME: &str = "__start";

impl<'sym, 'typ> Compiler<'sym, 'typ> {
    pub fn new(
        symbol_loader: &'sym SymbolLoader,
        type_loader: &'typ TypeLoader,
        type_printer: &'typ TypePrinter<'sym, 'typ>,
    ) -> Self {
        Self {
            symbol_loader,
            type_loader,
            type_printer,
        }
    }

    pub fn compile(&self, packages: Vec<Rc<Package>>, main_package: SymbolId) -> Result<Vec<u8>, Box<dyn Error>> {
        let main_package_name = self.symbol_loader.get_symbol(main_package).unwrap();
        let mut module = Module::default();
        module.name = Some(String::from(main_package_name.as_ref()));

        ProgramCompiler::new(
            self.symbol_loader,
            self.type_loader,
            self.type_printer,
            &mut module,
            &packages,
            main_package,
        )
        .compile();

        let wasm_result = module.emit_wasm();
        Ok(wasm_result)
    }
}

pub struct ProgramCompiler<'sym, 'typ, 'pkg> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
    type_printer: &'typ TypePrinter<'sym, 'typ>,
    module: &'pkg mut Module,
    data_end_ptr: Option<WasmGlobalId>,
    globals: HashMap<GlobalId, WasmGlobalId>,
    packages: &'pkg [Rc<Package>],
    main_package: SymbolId,
    // the map of the reachable global item + their possible type parameters
    reachable_globals: HashMap<GlobalId, HashSet<Vec<TypeId>>>,
    data_offset_table: HashMap<(SymbolId, usize), usize>,
    function_ids: HashMap<(GlobalId, Vec<TypeId>), FunctionId>,
    builtin_functions: HashMap<GlobalId, Rc<str>>,
}

impl<'sym, 'typ, 'pkg> ProgramCompiler<'sym, 'typ, 'pkg> {
    pub fn new(
        symbol_loader: &'sym SymbolLoader,
        type_loader: &'typ TypeLoader,
        type_printer: &'typ TypePrinter<'sym, 'typ>,
        module: &'pkg mut Module,
        packages: &'pkg [Rc<Package>],
        main_package: SymbolId,
    ) -> Self {
        Self {
            symbol_loader,
            type_loader,
            type_printer,
            module,
            data_end_ptr: None,
            globals: HashMap::default(),
            packages,
            main_package,
            reachable_globals: HashMap::default(),
            data_offset_table: HashMap::default(),
            function_ids: HashMap::default(),
            builtin_functions: HashMap::default(),
        }
    }

    fn compile(&mut self) {
        self.calculate_reachable_globals();
        self.declare_native_functions();
        self.declare_local_functions();
        self.build_main_func();
        let memory_id = self.build_global_memory();
        let data_end = self.build_data_segment(memory_id);
        self.build_mem_pointers(data_end);
        self.declare_globals(data_end);

        let functions = self.packages.iter().flat_map(|pkg| pkg.functions.iter());
        for func in functions {
            let func_global_id = GlobalId::new(func.package_name, func.function_name);
            let Some(variants) = self.reachable_globals.get(&func_global_id) else {
                continue;
            };
            for type_params in variants {
                let func_id = self
                    .function_ids
                    .get(&(func_global_id.clone(), type_params.clone()))
                    .unwrap();
                let wasm_func = self.module.funcs.get_mut(*func_id);
                let FunctionKind::Local(ref mut wasm_func) = wasm_func.kind else {
                    unreachable!();
                };

                let mut type_args = IndexMap::<SymbolId, TypeId>::default();
                for (name, type_id) in std::iter::zip(func.func_type.type_parameters.iter(), type_params.iter()) {
                    type_args.insert(*name, *type_id);
                }

                let mut variables: Vec<_> = wasm_func.args.to_vec();
                for local in func.locals.iter().skip(func.func_type.parameters.len()) {
                    let ty = self.type_loader.get_type(*local).unwrap();
                    let ty = to_wasm_type(self.type_loader, &type_args, &ty);
                    variables.push(self.module.locals.add(ty));
                }

                let builder = wasm_func.builder_mut();

                let mut body_builder = builder.func_body();
                let mut func_compiler = FunctionCompiler::new(
                    memory_id,
                    &type_args,
                    &variables,
                    self.type_loader,
                    self.type_printer,
                    self.data_end_ptr.unwrap(),
                    &self.data_offset_table,
                    &self.globals,
                    &self.function_ids,
                    &self.builtin_functions,
                );
                func_compiler.process_statement(&mut body_builder, &func.body);
            }
        }
    }

    fn calculate_reachable_globals(&mut self) {
        let mut func_to_body = HashMap::new();
        let mut func_to_type = HashMap::new();
        for pkg in self.packages {
            for func in &pkg.functions {
                let global_id = GlobalId::new(pkg.name, func.function_name);
                func_to_body.insert(global_id.clone(), &func.body);
                func_to_type.insert(global_id, &func.func_type);
            }
        }

        let mut stack = vec![];
        let mut in_stack = HashSet::new();

        let main_sym = self.symbol_loader.declare_symbol("main");
        stack.push((GlobalId::new(self.main_package, main_sym), vec![]));
        in_stack.insert((GlobalId::new(self.main_package, main_sym), vec![]));

        while let Some((func_id, type_arguments)) = stack.pop() {
            let type_args_set = self.reachable_globals.entry(func_id.clone()).or_default();
            if type_args_set.contains(&type_arguments) {
                continue;
            }
            type_args_set.insert(type_arguments.clone());

            let Some(body) = func_to_body.get(&func_id) else {
                continue;
            };

            let Some(func_type) = func_to_type.get(&func_id).cloned() else {
                continue;
            };
            let mut type_args = IndexMap::<SymbolId, TypeId>::default();
            for (name, type_id) in std::iter::zip(func_type.type_parameters.iter(), type_arguments.iter()) {
                type_args.insert(*name, *type_id);
            }

            let mut used_globals = vec![];
            self.get_used_globals(&type_args, body, &mut used_globals);
            for used_global_id in used_globals {
                if !in_stack.contains(&used_global_id) {
                    stack.push(used_global_id.clone());
                    in_stack.insert(used_global_id.clone());
                }
            }
        }
    }

    fn get_used_globals(
        &self,
        type_args: &IndexMap<SymbolId, TypeId>,
        stmt: &Statement,
        result: &mut Vec<(GlobalId, Vec<TypeId>)>,
    ) {
        let mut stmt_stack = vec![stmt];
        while let Some(stmt) = stmt_stack.pop() {
            match stmt {
                Statement::Invalid | Statement::Continue | Statement::Break => (),
                Statement::SetLocal(_, expr) => self.get_used_globals_in_expr(type_args, expr, result),
                Statement::SetGlobal(global_id, value) => {
                    result.push((global_id.clone(), vec![]));
                    self.get_used_globals_in_expr(type_args, value, result); // somehow causing stackoverflow
                }
                Statement::SetIndex { target, index, value } => {
                    self.get_used_globals_in_expr(type_args, target, result);
                    self.get_used_globals_in_expr(type_args, index, result);
                    self.get_used_globals_in_expr(type_args, value, result);
                }
                Statement::SetAddr { addr, value } => {
                    self.get_used_globals_in_expr(type_args, addr, result);
                    self.get_used_globals_in_expr(type_args, value, result);
                }
                Statement::If(if_stmt) => {
                    self.get_used_globals_in_expr(type_args, &if_stmt.condition, result);
                    stmt_stack.push(&if_stmt.body);
                    if let Some(ref else_body) = if_stmt.else_body {
                        stmt_stack.push(else_body);
                    }
                }
                Statement::While(while_stmt) => {
                    self.get_used_globals_in_expr(type_args, &while_stmt.condition, result);
                    stmt_stack.push(&while_stmt.body);
                }
                Statement::Block(block_stmt) => {
                    for stmt in &block_stmt.statements {
                        stmt_stack.push(stmt);
                    }
                }
                Statement::Return(ret_stmt) => {
                    if let Some(ref expr) = ret_stmt.value {
                        self.get_used_globals_in_expr(type_args, expr, result);
                    }
                }
                Statement::Expr(expr) => self.get_used_globals_in_expr(type_args, expr, result),
            }
        }
    }

    fn get_used_globals_in_expr(
        &self,
        type_args: &IndexMap<SymbolId, TypeId>,
        expr: &Expr,
        result: &mut Vec<(GlobalId, Vec<TypeId>)>,
    ) {
        let mut expr_stack = vec![expr];
        while let Some(expr) = expr_stack.pop() {
            match &expr.kind {
                ExprKind::Invalid
                | ExprKind::I64(..)
                | ExprKind::I32(..)
                | ExprKind::I16(..)
                | ExprKind::I8(..)
                | ExprKind::U64(..)
                | ExprKind::U32(..)
                | ExprKind::U16(..)
                | ExprKind::U8(..)
                | ExprKind::F64(..)
                | ExprKind::F32(..)
                | ExprKind::Bool(..)
                | ExprKind::Isize(..)
                | ExprKind::Usize(..)
                | ExprKind::Local(..)
                | ExprKind::ZeroOf(..)
                | ExprKind::SizeOf(..)
                | ExprKind::AlignOf(..)
                | ExprKind::DataEnd
                | ExprKind::StringLit(..)
                | ExprKind::Deref(..) => (),
                ExprKind::Global(global_id) => result.push((global_id.clone(), vec![])),
                ExprKind::FuncInit(global_id, type_parameters) => {
                    let converted_types: Vec<TypeId> = type_parameters
                        .iter()
                        .map(|type_id| transform_opaque_type(self.type_loader, type_args, *type_id))
                        .collect();

                    result.push((global_id.clone(), converted_types));
                }
                ExprKind::Binary { a, op: _, b } => {
                    expr_stack.push(a);
                    expr_stack.push(b);
                }
                ExprKind::Unary { val, op: _ } => {
                    expr_stack.push(val);
                }
                ExprKind::Call(func, args) => {
                    expr_stack.push(func);
                    for arg in args {
                        expr_stack.push(arg);
                    }
                }
                ExprKind::Index(target, index) => {
                    expr_stack.push(target);
                    expr_stack.push(index);
                }
                ExprKind::Cast(value, _) => {
                    expr_stack.push(value);
                }
            }
        }
    }

    fn declare_globals(&mut self, data_end: usize) {
        let globals = self.packages.iter().flat_map(|pkg| pkg.globals.iter());
        for global in globals {
            let ty = self.type_loader.get_type(global.type_id).unwrap();
            let wasm_ty = to_wasm_type(self.type_loader, &IndexMap::default(), &ty);

            let val = match global.value.kind {
                ExprKind::I64(val) => walrus::ir::Value::I64(val),
                ExprKind::I32(val) => walrus::ir::Value::I32(val),
                ExprKind::I16(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::I8(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::U64(val) => walrus::ir::Value::I64(val as i64),
                ExprKind::U32(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::U16(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::U8(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::F64(val) => walrus::ir::Value::F64(val),
                ExprKind::F32(val) => walrus::ir::Value::F32(val),
                ExprKind::Bool(val) => walrus::ir::Value::I32(if val { 1 } else { 0 }),
                ExprKind::Isize(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::Usize(val) => walrus::ir::Value::I32(val as i32),
                ExprKind::ZeroOf(type_id) => {
                    let type_id = transform_opaque_type(self.type_loader, &IndexMap::default(), type_id);
                    let ty = self.type_loader.get_type(type_id).unwrap();
                    match ty.as_ref() {
                        Type::Isize => walrus::ir::Value::I32(0),
                        Type::I64 => walrus::ir::Value::I64(0),
                        Type::I32 => walrus::ir::Value::I32(0),
                        Type::I16 => walrus::ir::Value::I32(0),
                        Type::I8 => walrus::ir::Value::I32(0),
                        Type::Usize => walrus::ir::Value::I32(0),
                        Type::U64 => walrus::ir::Value::I64(0),
                        Type::U32 => walrus::ir::Value::I32(0),
                        Type::U16 => walrus::ir::Value::I32(0),
                        Type::U8 => walrus::ir::Value::I32(0),
                        Type::F32 => walrus::ir::Value::I32(0),
                        Type::F64 => walrus::ir::Value::I32(0),
                        Type::Bool => walrus::ir::Value::I32(0),
                        Type::Slice(..) => walrus::ir::Value::I32(0),
                        Type::Pointer(..) => walrus::ir::Value::I32(0),
                        Type::ArrayPtr(..) => walrus::ir::Value::I32(0),
                        Type::Opaque(..) => unreachable!("this should be impossible"),
                        Type::Void | Type::Func(..) | Type::Invalid => todo!(),
                    }
                }
                ExprKind::SizeOf(type_id) => {
                    let ty = self.type_loader.get_type(type_id).unwrap();
                    let (size, _) = get_size_and_alignment(self.type_loader, &IndexMap::default(), &ty);
                    walrus::ir::Value::I32(size as i32)
                }
                ExprKind::AlignOf(type_id) => {
                    let ty = self.type_loader.get_type(type_id).unwrap();
                    let (_, align) = get_size_and_alignment(self.type_loader, &IndexMap::default(), &ty);
                    walrus::ir::Value::I32(align as i32)
                }
                ExprKind::DataEnd => walrus::ir::Value::I32(data_end as i32),
                _ => todo!(),
            };

            let id = self.module.globals.add_local(wasm_ty, true, InitExpr::Value(val));
            self.globals
                .insert(GlobalId::new(global.package_name, global.variable_name), id);
        }
    }

    fn declare_native_functions(&mut self) {
        let native_functions = self.packages.iter().flat_map(|pkg| pkg.native_functions.iter());
        for func in native_functions {
            let func_global_id = GlobalId::new(func.package_name, func.function_name);
            let Some(variants) = self.reachable_globals.get(&func_global_id) else {
                continue;
            };
            for type_params in variants {
                let wasm_link_sym = self.symbol_loader.declare_symbol("wasm_link");
                let mut is_external_func = false;

                let mut type_args = IndexMap::<SymbolId, TypeId>::default();
                for (name, type_id) in std::iter::zip(func.func_type.type_parameters.iter(), type_params.iter()) {
                    type_args.insert(*name, *type_id);
                }

                if let Some(wasm_link_tag) = func.tags.iter().find(|tag| tag.name == wasm_link_sym) {
                    if wasm_link_tag.arguments.len() == 2 {
                        let wasm_module = String::from_utf8(wasm_link_tag.arguments[0].to_vec())
                            .expect("module name should be a valid utf-8 string");
                        let wasm_name = String::from_utf8(wasm_link_tag.arguments[1].to_vec())
                            .expect("imported name should be a valid utf-8 string");
                        is_external_func = true;

                        let mut return_type = vec![];
                        if let Some(ret_type) = &func.func_type.return_type {
                            let ret_type = self.type_loader.get_type(*ret_type).unwrap();
                            let ret_type = to_wasm_type(self.type_loader, &type_args, &ret_type);
                            return_type = vec![ret_type];
                        }
                        let mut param_types = vec![];
                        for param_ty in &func.func_type.parameters {
                            let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                            let param_ty = to_wasm_type(self.type_loader, &type_args, &param_ty);
                            param_types.push(param_ty);
                        }

                        let type_id = self.module.types.add(&param_types, &return_type);
                        let (func_id, _) = self.module.add_import_func(&wasm_module, &wasm_name, type_id);
                        self.function_ids
                            .insert((func_global_id.clone(), type_params.clone()), func_id);
                    }
                }

                let builtin_sym = self.symbol_loader.declare_symbol("builtin");
                let mut is_builtin_func = false;
                if let Some(builtin_tag) = func.tags.iter().find(|tag| tag.name == builtin_sym) {
                    if let Some(builtin_name) = builtin_tag.arguments.first() {
                        let builtin_name = String::from_utf8(builtin_name.to_vec())
                            .expect("the function builtin name has to be a valud utf-8 string");
                        // TODO: check the builtin name, and the signature.
                        self.builtin_functions
                            .insert(func_global_id.clone(), builtin_name.into());
                        is_builtin_func = true;
                    }
                }

                if !is_external_func && !is_builtin_func {
                    panic!("native function can't be compiled");
                }
                if is_external_func && is_builtin_func {
                    panic!("can't compile, ambiguous native function linking");
                }
            }
        }
    }

    fn declare_local_functions(&mut self) {
        let functions = self.packages.iter().flat_map(|pkg| pkg.functions.iter());
        for func in functions {
            let func_global_id = GlobalId::new(func.package_name, func.function_name);
            let Some(variants) = self.reachable_globals.get(&func_global_id) else {
                continue;
            };
            for type_params in variants {
                let package_name = self.symbol_loader.get_symbol(func.package_name).unwrap();
                let func_name = self.symbol_loader.get_symbol(func.function_name).unwrap();

                let mut type_args = IndexMap::<SymbolId, TypeId>::default();
                for (name, type_id) in std::iter::zip(func.func_type.type_parameters.iter(), type_params.iter()) {
                    type_args.insert(*name, *type_id);
                }

                let mut return_type = vec![];
                if let Some(ret_type) = func.func_type.return_type {
                    let ret_type = self.type_loader.get_type(ret_type).unwrap();
                    let ret_type = to_wasm_type(self.type_loader, &type_args, &ret_type);
                    return_type = vec![ret_type];
                }

                let mut param_types = vec![];
                let mut variables = vec![];
                for param_ty in &func.func_type.parameters {
                    let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                    let param_ty = to_wasm_type(self.type_loader, &type_args, &param_ty);
                    param_types.push(param_ty);
                    variables.push(self.module.locals.add(param_ty));
                }

                let mut builder = FunctionBuilder::new(&mut self.module.types, &param_types, &return_type);
                let mangled_name = mangle_func(&package_name, &func_name, type_params);
                builder.name(mangled_name);

                let function_id = builder.finish(
                    variables.iter().take(param_types.len()).cloned().collect(),
                    &mut self.module.funcs,
                );

                self.function_ids
                    .insert((func_global_id.clone(), type_params.clone()), function_id);
            }
        }
    }

    fn build_main_func(&mut self) {
        let main_sym = self.symbol_loader.declare_symbol("main");
        let main_func_global_id = GlobalId::new(self.main_package, main_sym);
        let Some(main_func_id) = self.function_ids.get(&(main_func_global_id, vec![])) else {
            return;
        };

        let mut builder = FunctionBuilder::new(&mut self.module.types, &[], &[]);
        builder.name(String::from(START_FUNC_NAME));
        builder.func_body().call(*main_func_id);
        let start_func_id = builder.finish(vec![], &mut self.module.funcs);
        self.module.start = Some(start_func_id);
    }

    fn build_global_memory(&mut self) -> MemoryId {
        let memory_id = self.module.memories.add_local(true, 1, None);
        self.module.exports.add("memory", memory_id);
        memory_id
    }

    fn build_data_segment(&mut self, memory_id: MemoryId) -> usize {
        let mut offset = 8usize; // let's not use 0 since 0 can be considered as NULL;

        for pkg in self.packages {
            for (index, string) in pkg.strings.iter().enumerate() {
                let content = string.as_ref().to_vec();
                let content_len = (content.len() as u32).to_le_bytes();
                let content_offset = ((offset + 8) as u32).to_le_bytes();
                let mut data_buf = Vec::with_capacity(8 + content.len());
                data_buf.extend_from_slice(&content_offset);
                data_buf.extend_from_slice(&content_len);
                data_buf.extend_from_slice(&content);
                let data_len = data_buf.len();

                self.module.data.add(
                    DataKind::Active(ActiveData {
                        memory: memory_id,
                        location: ActiveDataLocation::Absolute(offset as u32),
                    }),
                    data_buf,
                );
                self.data_offset_table.insert((pkg.name, index), offset);
                offset += data_len;

                // need to align to 4
                let to_add = (4 - offset % 4) % 4;
                offset += to_add;
            }
        }

        offset
    }

    fn build_mem_pointers(&mut self, data_end: usize) {
        self.data_end_ptr = Some(self.module.globals.add_local(
            ValType::I32,
            false,
            InitExpr::Value(walrus::ir::Value::I32(data_end as i32)),
        ));

        let stack_size = 0x1000;
        let heap_start_offset = data_end + stack_size;
        self.module.globals.add_local(
            ValType::I32,
            false,
            InitExpr::Value(walrus::ir::Value::I32(heap_start_offset as i32)),
        );

        let stack_ptr = heap_start_offset;
        self.module.globals.add_local(
            ValType::I32,
            true,
            InitExpr::Value(walrus::ir::Value::I32(stack_ptr as i32)),
        );
    }
}

struct FunctionCompiler<'sym, 'typ, 'pkg> {
    type_loader: &'typ TypeLoader,
    type_printer: &'typ TypePrinter<'sym, 'typ>,
    data_end_ptr: WasmGlobalId,
    variables: &'pkg [LocalId],

    memory_id: MemoryId,
    type_arguments: &'pkg IndexMap<SymbolId, TypeId>,
    loop_blocks: Vec<(InstrSeqId, InstrSeqId)>,
    globals: &'pkg HashMap<GlobalId, WasmGlobalId>,
    function_ids: &'pkg HashMap<(GlobalId, Vec<TypeId>), FunctionId>,
    data_offsets: &'pkg HashMap<(SymbolId, usize), usize>,
    builtin_functions: &'pkg HashMap<GlobalId, Rc<str>>,
}

impl<'sym, 'typ, 'pkg> FunctionCompiler<'sym, 'typ, 'pkg> {
    pub fn new(
        memory_id: MemoryId,
        type_arguments: &'pkg IndexMap<SymbolId, TypeId>,
        variables: &'pkg [LocalId],
        type_loader: &'typ TypeLoader,
        type_printer: &'typ TypePrinter<'sym, 'typ>,
        data_end_ptr: WasmGlobalId,
        data_offsets: &'pkg HashMap<(SymbolId, usize), usize>,
        globals: &'pkg HashMap<GlobalId, WasmGlobalId>,
        function_ids: &'pkg HashMap<(GlobalId, Vec<TypeId>), FunctionId>,
        builtin_functions: &'pkg HashMap<GlobalId, Rc<str>>,
    ) -> Self {
        Self {
            type_loader,
            type_printer,
            data_end_ptr,
            memory_id,
            type_arguments,
            variables,
            loop_blocks: vec![],
            globals,
            function_ids,
            data_offsets,
            builtin_functions,
        }
    }

    fn process_statement(&mut self, builder: &mut InstrSeqBuilder, stmt: &Statement) {
        match stmt {
            Statement::SetLocal(id, expr) => {
                self.process_expr(builder, expr);
                builder.local_set(self.variables[*id]);
            }
            Statement::SetGlobal(global_id, value) => {
                self.process_expr(builder, value);
                builder.global_set(*self.globals.get(global_id).unwrap());
            }
            Statement::SetIndex { target, index, value } => {
                let ty = self.type_loader.get_type(target.type_id).unwrap();
                match ty.as_ref() {
                    Type::Slice(..) => self.process_slice_index(builder, target, index, value),
                    Type::ArrayPtr(..) => self.process_array_ptr_index(builder, target, index, value),
                    _ => unreachable!(),
                }
            }
            Statement::SetAddr { addr, value } => {
                let element_ty = self.type_loader.get_type(value.type_id).unwrap();

                let (_, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);
                let store_kind = match element_ty.as_ref() {
                    Type::I64 | Type::U64 => walrus::ir::StoreKind::I64 { atomic: false },
                    Type::I32 | Type::U32 | Type::Usize | Type::Isize => walrus::ir::StoreKind::I32 { atomic: false },
                    Type::I16 => walrus::ir::StoreKind::I32_16 { atomic: false },
                    Type::U16 => walrus::ir::StoreKind::I32_16 { atomic: false },
                    Type::I8 => walrus::ir::StoreKind::I32_8 { atomic: false },
                    Type::U8 => walrus::ir::StoreKind::I32_8 { atomic: false },
                    Type::Invalid | Type::Void | Type::Func(..) => todo!(),
                    _ => todo!(),
                };

                self.process_expr(builder, addr);
                self.process_expr(builder, value);
                builder.store(
                    self.memory_id,
                    store_kind,
                    walrus::ir::MemArg {
                        align: align as u32,
                        offset: 0,
                    },
                );
            }
            Statement::If(if_stmt) => {
                builder.block(None, |outer_block| {
                    let outer_id = outer_block.id();
                    outer_block.block(None, |block_builder| {
                        self.process_expr(block_builder, &if_stmt.condition);
                        block_builder.unop(UnaryOp::I32Eqz);
                        block_builder.br_if(block_builder.id());

                        self.process_statement(block_builder, &if_stmt.body);

                        block_builder.br(outer_id);
                    });

                    if let Some(else_body) = if_stmt.else_body.as_ref() {
                        self.process_statement(outer_block, else_body.as_ref());
                    }
                });
            }
            Statement::While(while_stmt) => {
                builder.block(None, |block_builder| {
                    let outer_id = block_builder.id();
                    block_builder.loop_(None, |middle_builder| {
                        self.loop_blocks.push((outer_id, middle_builder.id()));
                        middle_builder.block(None, |body_builder| {
                            self.process_expr(body_builder, &while_stmt.condition);
                            body_builder.unop(UnaryOp::I32Eqz);
                            body_builder.br_if(outer_id);
                            self.process_statement(body_builder, &while_stmt.body);
                        });

                        middle_builder.br(middle_builder.id());
                    });
                });

                self.loop_blocks.pop();
            }
            Statement::Continue => {
                let (_, continue_id) = self.loop_blocks.last().unwrap();
                builder.br(*continue_id);
            }
            Statement::Break => {
                let (break_id, _) = self.loop_blocks.last().unwrap();
                builder.br(*break_id);
            }
            Statement::Block(block) => {
                for stmt in &block.statements {
                    self.process_statement(builder, stmt);
                }
            }
            Statement::Return(ret_stmt) => {
                if let Some(ref val) = ret_stmt.value {
                    self.process_expr(builder, val);
                }
                builder.return_();
            }
            Statement::Expr(expr) => {
                self.process_expr(builder, expr);
                if !self.type_loader.get_type(expr.type_id).unwrap().is_void() {
                    builder.drop();
                }
            }
            Statement::Invalid => unreachable!(),
        }
    }

    fn process_slice_index(&mut self, builder: &mut InstrSeqBuilder, target: &Expr, index: &Expr, value: &Expr) {
        let ty = self.type_loader.get_type(target.type_id).unwrap();
        let Type::Slice(slice_ty) = ty.as_ref() else {
            unreachable!();
        };

        let element_ty = self.type_loader.get_type(slice_ty.element_type).unwrap();
        let index_ty = self.type_loader.get_type(index.type_id).unwrap();
        let (size, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);

        // TODO: check index range

        // index as u32 * size
        self.process_expr(builder, index);
        self.do_cast(builder, &index_ty, &Type::U32);
        builder.i32_const(size as i32);
        builder.binop(BinaryOp::I32Mul);

        // *target
        self.process_expr(builder, target);
        builder.load(
            self.memory_id,
            walrus::ir::LoadKind::I32 { atomic: false },
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );

        // *target + index * size
        builder.binop(BinaryOp::I32Add);

        let store_kind = match element_ty.as_ref() {
            Type::I64 | Type::U64 => walrus::ir::StoreKind::I64 { atomic: false },
            Type::I32 | Type::U32 => walrus::ir::StoreKind::I32 { atomic: false },
            Type::I16 => walrus::ir::StoreKind::I32_16 { atomic: false },
            Type::U16 => walrus::ir::StoreKind::I32_16 { atomic: false },
            Type::I8 => walrus::ir::StoreKind::I32_8 { atomic: false },
            Type::U8 => walrus::ir::StoreKind::I32_8 { atomic: false },
            Type::Invalid | Type::Void | Type::Func(..) => todo!(),
            _ => todo!(),
        };

        self.process_expr(builder, value);
        builder.store(
            self.memory_id,
            store_kind,
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );
    }

    fn process_array_ptr_index(&mut self, builder: &mut InstrSeqBuilder, target: &Expr, index: &Expr, value: &Expr) {
        let ty = self.type_loader.get_type(target.type_id).unwrap();
        let Type::ArrayPtr(array_ptr_ty) = ty.as_ref() else {
            unreachable!();
        };

        let element_ty = self.type_loader.get_type(array_ptr_ty.element_type).unwrap();
        let index_ty = self.type_loader.get_type(index.type_id).unwrap();
        let (size, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);

        // i * size
        builder.i32_const(size as i32);
        self.process_expr(builder, index);
        self.do_cast(builder, &index_ty, &Type::U32);
        builder.binop(BinaryOp::I32Mul);
        // target + i * size
        self.process_expr(builder, target);
        builder.binop(BinaryOp::I32Add);

        let store_kind = match element_ty.as_ref() {
            Type::I64 | Type::U64 => walrus::ir::StoreKind::I64 { atomic: false },
            Type::I32 | Type::U32 | Type::Usize | Type::Isize => walrus::ir::StoreKind::I32 { atomic: false },
            Type::I16 => walrus::ir::StoreKind::I32_16 { atomic: false },
            Type::U16 => walrus::ir::StoreKind::I32_16 { atomic: false },
            Type::I8 => walrus::ir::StoreKind::I32_8 { atomic: false },
            Type::U8 => walrus::ir::StoreKind::I32_8 { atomic: false },
            Type::Invalid | Type::Void | Type::Func(..) => todo!(),
            _ => todo!(),
        };

        self.process_expr(builder, value);
        builder.store(
            self.memory_id,
            store_kind,
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );
    }

    fn process_expr(&self, builder: &mut InstrSeqBuilder, expr: &Expr) {
        match &expr.kind {
            ExprKind::Invalid => unreachable!(),
            ExprKind::I64(val) => {
                builder.i64_const(*val);
            }
            ExprKind::I32(val) => {
                builder.i32_const(*val);
            }
            ExprKind::I16(val) => {
                builder.i32_const(*val as i32);
                builder.unop(UnaryOp::I32Extend16S);
            }
            ExprKind::I8(val) => {
                builder.i32_const(*val as i32);
                builder.unop(UnaryOp::I32Extend8S);
            }
            ExprKind::U64(val) => {
                builder.i64_const(*val as i64);
            }
            ExprKind::U32(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::U16(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::U8(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::F64(val) => {
                builder.f64_const(*val);
            }
            ExprKind::F32(val) => {
                builder.f32_const(*val);
            }
            ExprKind::Bool(val) => {
                builder.i32_const(if *val { 1 } else { 0 });
            }
            ExprKind::Isize(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::Usize(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::Local(index) => {
                builder.local_get(self.variables[*index]);
            }
            ExprKind::Global(global_id) => {
                builder.global_get(*self.globals.get(global_id).unwrap());
            }
            ExprKind::FuncInit(..) => {
                todo!();
            }
            ExprKind::ZeroOf(type_id) => {
                let type_id = transform_opaque_type(self.type_loader, self.type_arguments, *type_id);
                let ty = self.type_loader.get_type(type_id).unwrap();
                match ty.as_ref() {
                    Type::Isize => builder.i32_const(0),
                    Type::I64 => builder.i64_const(0),
                    Type::I32 => builder.i32_const(0),
                    Type::I16 => builder.i32_const(0),
                    Type::I8 => builder.i32_const(0),
                    Type::Usize => builder.i32_const(0),
                    Type::U64 => builder.i64_const(0),
                    Type::U32 => builder.i32_const(0),
                    Type::U16 => builder.i32_const(0),
                    Type::U8 => builder.i32_const(0),
                    Type::F32 => builder.i32_const(0),
                    Type::F64 => builder.i32_const(0),
                    Type::Bool => builder.i32_const(0),
                    Type::Slice(..) => builder.i32_const(0),
                    Type::Pointer(..) => builder.i32_const(0),
                    Type::ArrayPtr(..) => builder.i32_const(0),
                    Type::Opaque(..) => unreachable!("this should be converted above"),
                    Type::Void | Type::Func(..) | Type::Invalid => builder.i32_const(0),
                };
            }
            ExprKind::SizeOf(type_id) => {
                let ty = self.type_loader.get_type(*type_id).unwrap();
                let (size, _) = get_size_and_alignment(self.type_loader, self.type_arguments, &ty);
                builder.i32_const(size as i32);
            }
            ExprKind::AlignOf(type_id) => {
                let ty = self.type_loader.get_type(*type_id).unwrap();
                let (_, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &ty);
                builder.i32_const(align as i32);
            }
            ExprKind::DataEnd => {
                builder.global_get(self.data_end_ptr);
            }
            ExprKind::StringLit(str_lit) => {
                let offset = self.data_offsets.get(&(str_lit.package_name, str_lit.index)).unwrap();
                builder.i32_const(*offset as i32);
            }
            ExprKind::Call(target, arguments) => {
                self.process_call_expr(builder, target, arguments);
            }
            ExprKind::Index(target, index) => {
                let ty = self.type_loader.get_type(target.type_id).unwrap();

                match ty.as_ref() {
                    Type::Slice(..) => self.process_slice_index_expr(builder, target, index),
                    Type::ArrayPtr(..) => self.process_array_ptr_index_expr(builder, target, index),
                    _ => unreachable!(),
                }
            }
            ExprKind::Cast(value, target_ty) => {
                self.process_cast_expr(builder, value, *target_ty);
            }
            ExprKind::Binary { a, op, b } => {
                self.process_binary_expr(builder, a, *op, b);
            }
            ExprKind::Unary { op, val } => {
                self.process_unary_expr(builder, *op, val);
            }
            ExprKind::Deref(addr) => {
                let ty = self.type_loader.get_type(addr.type_id).unwrap();
                let Type::Pointer(pointer_ty) = ty.as_ref() else {
                    unreachable!();
                };
                let element_ty = self.type_loader.get_type(pointer_ty.element_type).unwrap();

                let (_, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);
                let load_kind = match element_ty.as_ref() {
                    Type::I64 | Type::U64 => walrus::ir::LoadKind::I64 { atomic: false },
                    Type::I32 | Type::U32 => walrus::ir::LoadKind::I32 { atomic: false },
                    Type::I16 => walrus::ir::LoadKind::I32_16 {
                        kind: walrus::ir::ExtendedLoad::SignExtend,
                    },
                    Type::U16 => walrus::ir::LoadKind::I32_16 {
                        kind: walrus::ir::ExtendedLoad::ZeroExtend,
                    },
                    Type::I8 => walrus::ir::LoadKind::I32_8 {
                        kind: walrus::ir::ExtendedLoad::SignExtend,
                    },
                    Type::U8 => walrus::ir::LoadKind::I32_8 {
                        kind: walrus::ir::ExtendedLoad::ZeroExtend,
                    },
                    Type::Invalid | Type::Void | Type::Func(..) => todo!(),
                    _ => todo!(),
                };

                self.process_expr(builder, addr);
                builder.load(
                    self.memory_id,
                    load_kind,
                    walrus::ir::MemArg {
                        align: align as u32,
                        offset: 0,
                    },
                );
            }
        }
    }

    fn process_call_expr(&self, builder: &mut InstrSeqBuilder, target: &Expr, arguments: &[Expr]) {
        for arg in arguments.iter() {
            self.process_expr(builder, arg);
        }

        let key = match &target.kind {
            ExprKind::Global(global_id) => (global_id.clone(), vec![]),
            ExprKind::FuncInit(global_id, type_arguments) => (
                global_id.clone(),
                type_arguments
                    .iter()
                    .map(|type_id| transform_opaque_type(self.type_loader, self.type_arguments, *type_id))
                    .collect(),
            ),
            _ => unreachable!("the callee expression is not a function"),
        };

        if let Some(func_id) = self.function_ids.get(&key) {
            builder.call(*func_id);
        } else if let Some(builtin_name) = self.builtin_functions.get(&key.0) {
            self.process_builtin_call(builder, builtin_name, &key.1);
        } else {
            unreachable!("function definition is not found");
        }
    }

    fn process_slice_index_expr(&self, builder: &mut InstrSeqBuilder, target: &Expr, index: &Expr) {
        let ty = self.type_loader.get_type(target.type_id).unwrap();
        let Type::Slice(slice_ty) = ty.as_ref() else {
            unreachable!();
        };
        let element_ty = self.type_loader.get_type(slice_ty.element_type).unwrap();
        let index_ty = self.type_loader.get_type(index.type_id).unwrap();
        let (size, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);

        // TODO: check index range

        // calculate index as i32 * size
        self.process_expr(builder, index);
        self.do_cast(builder, &index_ty, &Type::U32);
        builder.i32_const(size as i32);
        builder.binop(BinaryOp::I32Mul);

        // calculate addr of target[0]
        self.process_expr(builder, target);
        builder.load(
            self.memory_id,
            walrus::ir::LoadKind::I32 { atomic: false },
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );

        // calculate *target + index * size
        builder.binop(BinaryOp::I32Add);

        let load_kind = match element_ty.as_ref() {
            Type::I64 | Type::U64 => walrus::ir::LoadKind::I64 { atomic: false },
            Type::I32 | Type::U32 => walrus::ir::LoadKind::I32 { atomic: false },
            Type::I16 => walrus::ir::LoadKind::I32_16 {
                kind: walrus::ir::ExtendedLoad::SignExtend,
            },
            Type::U16 => walrus::ir::LoadKind::I32_16 {
                kind: walrus::ir::ExtendedLoad::ZeroExtend,
            },
            Type::I8 => walrus::ir::LoadKind::I32_8 {
                kind: walrus::ir::ExtendedLoad::SignExtend,
            },
            Type::U8 => walrus::ir::LoadKind::I32_8 {
                kind: walrus::ir::ExtendedLoad::ZeroExtend,
            },
            Type::Invalid | Type::Void | Type::Func(..) => todo!(),
            _ => todo!(),
        };

        builder.load(
            self.memory_id,
            load_kind,
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );
    }

    fn process_array_ptr_index_expr(&self, builder: &mut InstrSeqBuilder, target: &Expr, index: &Expr) {
        let ty = self.type_loader.get_type(target.type_id).unwrap();
        let Type::ArrayPtr(array_ptr_ty) = ty.as_ref() else {
            unreachable!();
        };

        let element_ty = self.type_loader.get_type(array_ptr_ty.element_type).unwrap();
        let index_ty = self.type_loader.get_type(index.type_id).unwrap();
        let (size, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &element_ty);

        // i * size
        builder.i32_const(size as i32);
        self.process_expr(builder, index);
        self.do_cast(builder, &index_ty, &Type::U32);
        builder.binop(BinaryOp::I32Mul);
        // target + i * size
        self.process_expr(builder, target);
        builder.binop(BinaryOp::I32Add);

        let load_kind = match element_ty.as_ref() {
            Type::I64 | Type::U64 => walrus::ir::LoadKind::I64 { atomic: false },
            Type::I32 | Type::U32 => walrus::ir::LoadKind::I32 { atomic: false },
            Type::I16 => walrus::ir::LoadKind::I32_16 {
                kind: walrus::ir::ExtendedLoad::SignExtend,
            },
            Type::U16 => walrus::ir::LoadKind::I32_16 {
                kind: walrus::ir::ExtendedLoad::ZeroExtend,
            },
            Type::I8 => walrus::ir::LoadKind::I32_8 {
                kind: walrus::ir::ExtendedLoad::SignExtend,
            },
            Type::U8 => walrus::ir::LoadKind::I32_8 {
                kind: walrus::ir::ExtendedLoad::ZeroExtend,
            },
            Type::Invalid | Type::Void | Type::Func(..) => todo!(),
            _ => todo!(),
        };

        builder.load(
            self.memory_id,
            load_kind,
            walrus::ir::MemArg {
                align: align as u32,
                offset: 0,
            },
        );
    }

    fn process_builtin_call(&self, builder: &mut InstrSeqBuilder, builtin_name: &str, type_arguments: &[TypeId]) {
        match builtin_name {
            "memory.grow" => {
                builder.memory_grow(self.memory_id);
            }
            "memory.size" => {
                builder.memory_size(self.memory_id);
            }
            "size_of" => {
                let ty = self.type_loader.get_type(type_arguments[0]).unwrap();
                let (size, _) = get_size_and_alignment(self.type_loader, self.type_arguments, &ty);
                builder.i32_const(size as i32);
            }
            "align_of" => {
                let ty = self.type_loader.get_type(type_arguments[0]).unwrap();
                let (_, align) = get_size_and_alignment(self.type_loader, self.type_arguments, &ty);
                builder.i32_const(align as i32);
            }
            builtin_name => panic!("unknown builtin function for {builtin_name}"),
        }
    }

    fn process_cast_expr(&self, builder: &mut InstrSeqBuilder, value: &Expr, target_ty: TypeId) {
        let value_ty = self.type_loader.get_type(value.type_id).unwrap();
        let target_ty = self.type_loader.get_type(target_ty).unwrap();

        self.process_expr(builder, value);
        self.do_cast(builder, &value_ty, &target_ty);
    }

    fn do_cast(&self, builder: &mut InstrSeqBuilder, src: &Type, dst: &Type) {
        match (src, dst) {
            (Type::I64 | Type::U64, Type::I64 | Type::U64) => {}
            (
                Type::I64 | Type::U64,
                Type::I32 | Type::U32 | Type::Usize | Type::Isize | Type::Slice(..) | Type::Pointer(..),
            ) => {
                builder.unop(UnaryOp::I32WrapI64);
            }
            (Type::I64 | Type::U64, Type::I16 | Type::U16) => {
                builder.unop(UnaryOp::I32WrapI64);
                builder.unop(UnaryOp::I32Extend16S);
            }
            (Type::I64 | Type::U64, Type::I8 | Type::U8) => {
                builder.unop(UnaryOp::I32WrapI64);
                builder.unop(UnaryOp::I32Extend8S);
            }
            (Type::I32 | Type::Isize | Type::I16 | Type::I8, Type::I64) => {
                builder.unop(UnaryOp::I64ExtendSI32);
            }
            (Type::U32 | Type::Usize | Type::U16 | Type::U8, Type::I64 | Type::U64) => {
                builder.unop(UnaryOp::I64ExtendUI32);
            }
            (Type::I32 | Type::Isize | Type::U32 | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8, _) => {}
            (Type::Slice(..), Type::I64) => {
                builder.unop(UnaryOp::I64Extend32S);
            }
            (Type::Slice(..), _) => {}
            (Type::ArrayPtr(..), Type::I64) => {
                builder.unop(UnaryOp::I64Extend32S);
            }
            (Type::ArrayPtr(..), _) => {}
            (Type::Pointer(..), Type::I64) => {
                builder.unop(UnaryOp::I64Extend32S);
            }
            (Type::Pointer(..), _) => {}
            (source, target) => todo!(
                "casting from {} to {} is not supported yet",
                self.type_printer.display_type(source),
                self.type_printer.display_type(target),
            ),
        }
    }

    fn process_binary_expr(&self, builder: &mut InstrSeqBuilder, a: &Expr, op: BinOp, b: &Expr) {
        if op == BinOp::And {
            return self.process_logical_and_expr(builder, a, b);
        }

        if op == BinOp::Or {
            return self.process_logical_or_expr(builder, a, b);
        }

        match op {
            BinOp::And => self.process_logical_and_expr(builder, a, b),
            BinOp::Or => self.process_logical_or_expr(builder, a, b),
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod
            | BinOp::BitOr
            | BinOp::BitAnd
            | BinOp::BitXor
            | BinOp::ShiftLeft
            | BinOp::ShiftRight
            | BinOp::Eq
            | BinOp::NEq
            | BinOp::Gt
            | BinOp::GEq
            | BinOp::Lt
            | BinOp::LEq => self.process_numeric_binary_expr(builder, a, op, b),
        }
    }

    fn process_logical_and_expr(&self, builder: &mut InstrSeqBuilder, a: &Expr, b: &Expr) {
        self.process_expr(builder, a);
        builder.if_else(
            ValType::I32,
            |builder| {
                self.process_expr(builder, b);
            },
            |builder| {
                builder.i32_const(0);
            },
        );
    }

    fn process_logical_or_expr(&self, builder: &mut InstrSeqBuilder, a: &Expr, b: &Expr) {
        self.process_expr(builder, a);
        builder.if_else(
            ValType::I32,
            |builder| {
                builder.i32_const(1);
            },
            |builder| {
                self.process_expr(builder, b);
            },
        );
    }

    fn process_numeric_binary_expr(&self, builder: &mut InstrSeqBuilder, a: &Expr, op: BinOp, b: &Expr) {
        self.process_expr(builder, a);
        self.process_expr(builder, b);

        let ty = self.type_loader.get_type(a.type_id).unwrap();

        match (op, ty.as_ref()) {
            (BinOp::Add, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Add),
            (
                BinOp::Add,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => builder.binop(BinaryOp::I32Add),
            (BinOp::Add, Type::F64) => builder.binop(BinaryOp::F64Add),
            (BinOp::Add, Type::F32) => builder.binop(BinaryOp::F32Add),

            (BinOp::Sub, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Sub),
            (
                BinOp::Sub,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => builder.binop(BinaryOp::I32Sub),
            (BinOp::Sub, Type::F64) => builder.binop(BinaryOp::F64Sub),
            (BinOp::Sub, Type::F32) => builder.binop(BinaryOp::F32Sub),

            (BinOp::Mul, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Mul),
            (
                BinOp::Mul,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => builder.binop(BinaryOp::I32Mul),
            (BinOp::Mul, Type::F64) => builder.binop(BinaryOp::F64Mul),
            (BinOp::Mul, Type::F32) => builder.binop(BinaryOp::F32Mul),

            (BinOp::Div, Type::I64) => builder.binop(BinaryOp::I64DivS),
            (BinOp::Div, Type::U64) => builder.binop(BinaryOp::I64DivU),
            (BinOp::Div, Type::I32) => builder.binop(BinaryOp::I32DivS),
            (BinOp::Div, Type::U32) => builder.binop(BinaryOp::I32DivU),
            (BinOp::Div, Type::Isize) => builder.binop(BinaryOp::I32DivS),
            (BinOp::Div, Type::Usize) => builder.binop(BinaryOp::I32DivU),
            (BinOp::Div, Type::F64) => builder.binop(BinaryOp::F64Div),
            (BinOp::Div, Type::F32) => builder.binop(BinaryOp::F32Div),

            (BinOp::Mod, Type::I64) => builder.binop(BinaryOp::I64RemS),
            (BinOp::Mod, Type::U64) => builder.binop(BinaryOp::I64RemU),
            (BinOp::Mod, Type::I32) => builder.binop(BinaryOp::I32RemS),
            (BinOp::Mod, Type::U32) => builder.binop(BinaryOp::I32RemU),
            (BinOp::Mod, Type::Isize) => builder.binop(BinaryOp::I32RemS),
            (BinOp::Mod, Type::Usize) => builder.binop(BinaryOp::I32RemU),

            (BinOp::BitOr, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Or),
            (BinOp::BitOr, Type::I32 | Type::U32 | Type::Isize | Type::Usize) => builder.binop(BinaryOp::I32Or),

            (BinOp::BitAnd, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64And),
            (BinOp::BitAnd, Type::I32 | Type::U32 | Type::Isize | Type::Usize) => builder.binop(BinaryOp::I32And),

            (BinOp::BitXor, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Xor),
            (BinOp::BitXor, Type::I32 | Type::U32 | Type::Isize | Type::Usize) => builder.binop(BinaryOp::I32Xor),

            (BinOp::ShiftLeft, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Shl),
            (BinOp::ShiftLeft, Type::I32 | Type::U32 | Type::Isize | Type::Usize) => builder.binop(BinaryOp::I32Shl),
            (BinOp::ShiftRight, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64ShrU),
            (BinOp::ShiftRight, Type::I32 | Type::U32 | Type::Isize | Type::Usize) => builder.binop(BinaryOp::I32ShrU),

            (BinOp::Eq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Eq),
            (
                BinOp::Eq,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => builder.binop(BinaryOp::I32Eq),
            (BinOp::Eq, Type::F64) => builder.binop(BinaryOp::F64Eq),
            (BinOp::Eq, Type::F32) => builder.binop(BinaryOp::F32Eq),

            (BinOp::NEq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Ne),
            (
                BinOp::NEq,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => builder.binop(BinaryOp::I32Ne),
            (BinOp::NEq, Type::F64) => builder.binop(BinaryOp::F64Ne),
            (BinOp::NEq, Type::F32) => builder.binop(BinaryOp::F32Ne),

            (BinOp::Gt, Type::I64) => builder.binop(BinaryOp::I64GtS),
            (BinOp::Gt, Type::I32 | Type::Isize | Type::I16 | Type::I8) => builder.binop(BinaryOp::I32GtS),
            (BinOp::Gt, Type::U64) => builder.binop(BinaryOp::I64GtU),
            (BinOp::Gt, Type::U32 | Type::Usize | Type::U16 | Type::U8) => builder.binop(BinaryOp::I32GtU),
            (BinOp::Gt, Type::F32) => builder.binop(BinaryOp::F32Gt),
            (BinOp::Gt, Type::F64) => builder.binop(BinaryOp::F32Gt),

            (BinOp::GEq, Type::I64) => builder.binop(BinaryOp::I64GeS),
            (BinOp::GEq, Type::I32 | Type::Isize | Type::I16 | Type::I8) => builder.binop(BinaryOp::I32GeS),
            (BinOp::GEq, Type::U64) => builder.binop(BinaryOp::I64GeU),
            (BinOp::GEq, Type::U32 | Type::Usize | Type::U16 | Type::U8) => builder.binop(BinaryOp::I32GeU),
            (BinOp::GEq, Type::F32) => builder.binop(BinaryOp::F32Ge),
            (BinOp::GEq, Type::F64) => builder.binop(BinaryOp::F32Ge),

            (BinOp::Lt, Type::I64) => builder.binop(BinaryOp::I64LtS),
            (BinOp::Lt, Type::I32 | Type::Isize | Type::I16 | Type::I8) => builder.binop(BinaryOp::I32LtS),
            (BinOp::Lt, Type::U64) => builder.binop(BinaryOp::I64LtU),
            (BinOp::Lt, Type::U32 | Type::Usize | Type::U16 | Type::U8) => builder.binop(BinaryOp::I32LtU),
            (BinOp::Lt, Type::F32) => builder.binop(BinaryOp::F32Lt),
            (BinOp::Lt, Type::F64) => builder.binop(BinaryOp::F32Lt),

            (BinOp::LEq, Type::I64) => builder.binop(BinaryOp::I64LeS),
            (BinOp::LEq, Type::I32 | Type::Isize | Type::I16 | Type::I8) => builder.binop(BinaryOp::I32LeS),
            (BinOp::LEq, Type::U64) => builder.binop(BinaryOp::I64LeU),
            (BinOp::LEq, Type::U32 | Type::Usize | Type::U16 | Type::U8) => builder.binop(BinaryOp::I32LeU),
            (BinOp::LEq, Type::F32) => builder.binop(BinaryOp::F32Le),
            (BinOp::LEq, Type::F64) => builder.binop(BinaryOp::F32Le),

            (op, ty) => unreachable!("cannot perform op {op:?} for {ty:?}"),
        };
    }

    fn process_unary_expr(&self, builder: &mut InstrSeqBuilder, op: UnOp, val: &Expr) {
        let ty = self.type_loader.get_type(val.type_id).unwrap();

        match (op, ty.as_ref()) {
            (UnOp::BitNot, Type::I64 | Type::U64) => {
                builder.i64_const(-1);
                self.process_expr(builder, val);
                builder.binop(BinaryOp::I64Xor);
            }
            (
                UnOp::BitNot,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => {
                builder.i32_const(-1);
                self.process_expr(builder, val);
                builder.binop(BinaryOp::I32Xor);
            }
            (UnOp::Add, _) => {
                self.process_expr(builder, val);
            }
            (UnOp::Sub, Type::I64 | Type::U64) => {
                builder.i64_const(0);
                self.process_expr(builder, val);
                builder.binop(BinaryOp::I64Sub);
            }
            (
                UnOp::Sub,
                Type::I32 | Type::U32 | Type::Isize | Type::Usize | Type::I16 | Type::U16 | Type::I8 | Type::U8,
            ) => {
                builder.i32_const(0);
                self.process_expr(builder, val);
                builder.binop(BinaryOp::I32Sub);
            }
            (UnOp::Sub, Type::F64) => {
                self.process_expr(builder, val);
                builder.unop(UnaryOp::F64Neg);
            }
            (UnOp::Sub, Type::F32) => {
                self.process_expr(builder, val);
                builder.unop(UnaryOp::F32Neg);
            }
            (UnOp::Not, _) => {
                builder.i32_const(1);
                self.process_expr(builder, val);
                builder.binop(BinaryOp::I32Xor);
            }
            _ => unreachable!(),
        };
    }
}

fn mangle_func(package_name: &str, function_name: &str, type_params: &[TypeId]) -> String {
    let mut s = format!("{package_name}.{function_name}");
    for type_param in type_params {
        s.push('.');
        s.push_str(&format!("{:?}", type_param));
    }
    s
}

fn transform_opaque_type(
    type_loader: &TypeLoader,
    name_to_type: &IndexMap<SymbolId, TypeId>,
    type_id: TypeId,
) -> TypeId {
    let ty = type_loader.get_type(type_id).unwrap();
    match ty.as_ref() {
        Type::Invalid
        | Type::Void
        | Type::Isize
        | Type::I64
        | Type::I32
        | Type::I16
        | Type::I8
        | Type::Usize
        | Type::U64
        | Type::U32
        | Type::U16
        | Type::U8
        | Type::F32
        | Type::F64
        | Type::Bool => type_id,
        Type::Func(func_type) => type_loader.declare_type(Type::Func(FuncType {
            type_parameters: Vec::default(),
            parameters: func_type
                .parameters
                .iter()
                .map(|type_id| transform_opaque_type(type_loader, name_to_type, *type_id))
                .collect(),
            return_type: func_type
                .return_type
                .map(|type_id| transform_opaque_type(type_loader, name_to_type, type_id)),
        })),
        Type::Slice(slice_type) => type_loader.declare_type(Type::Slice(SliceType {
            element_type: transform_opaque_type(type_loader, name_to_type, slice_type.element_type),
        })),
        Type::Pointer(pointer_type) => type_loader.declare_type(Type::Pointer(PointerType {
            element_type: transform_opaque_type(type_loader, name_to_type, pointer_type.element_type),
        })),
        Type::ArrayPtr(array_ptr_type) => type_loader.declare_type(Type::ArrayPtr(ArrayPtrType {
            element_type: transform_opaque_type(type_loader, name_to_type, array_ptr_type.element_type),
        })),
        Type::Opaque(name) => *name_to_type.get(name).expect("todo: handle the error"),
    }
}

fn to_wasm_type(type_loader: &TypeLoader, type_args: &IndexMap<SymbolId, TypeId>, ty: &Type) -> ValType {
    let type_id = type_loader.declare_type(ty.clone());
    let type_id = transform_opaque_type(type_loader, type_args, type_id);
    let ty = type_loader.get_type(type_id).unwrap();

    match ty.as_ref() {
        Type::I64 | Type::U64 => ValType::I64,
        Type::Isize
        | Type::Usize
        | Type::I32
        | Type::U32
        | Type::I16
        | Type::U16
        | Type::I8
        | Type::U8
        | Type::Bool => ValType::I32,
        Type::F64 => ValType::F64,
        Type::F32 => ValType::F32,
        Type::Slice(_) => ValType::I32,
        Type::Pointer(_) => ValType::I32,
        Type::ArrayPtr(_) => ValType::I32,
        Type::Func(..) => todo!("function pointer is not supported yet"),
        Type::Void => unreachable!("void cannot be constructed"),
        Type::Invalid => unreachable!("program with invalid type shouldn't be compiled"),
        Type::Opaque(_) => unreachable!("opaque type should be converted above"),
    }
}

// can we guarantee: size is multiple of alignment?
fn get_size_and_alignment(
    type_loader: &TypeLoader,
    type_args: &IndexMap<SymbolId, TypeId>,
    ty: &Type,
) -> (usize, usize) {
    let type_id = type_loader.declare_type(ty.clone());
    let type_id = transform_opaque_type(type_loader, type_args, type_id);
    let ty = type_loader.get_type(type_id).unwrap();

    match ty.as_ref() {
        Type::I64 | Type::U64 => (8, 4),
        Type::Isize | Type::Usize | Type::I32 | Type::U32 => (4, 4),
        Type::I16 | Type::U16 => (2, 2),
        Type::I8 | Type::U8 => (1, 1),
        Type::F64 => (8, 4),
        Type::F32 => (4, 4),
        Type::Bool => (1, 1),
        Type::Slice(..) => (4, 4),
        Type::Pointer(..) => (4, 4),
        Type::ArrayPtr(..) => (4, 4),
        Type::Func(..) => todo!("function pointer is not supported yet"),
        Type::Void => unreachable!("void cannot be constructed"),
        Type::Invalid => unreachable!("program with invalid type shouldn't be compiled"),
        Type::Opaque(_) => unreachable!("opaque type should be converted above"),
    }
}
