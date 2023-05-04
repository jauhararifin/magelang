use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{BinOp, Expr, ExprKind, Package, Statement, Type, TypeDisplay, TypeId, TypeLoader, UnOp};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::rc::Rc;
use walrus::{
    ir::BinaryOp, ir::InstrSeqId, ir::UnaryOp, ActiveData, ActiveDataLocation, DataKind, FunctionBuilder, FunctionId,
    FunctionKind, InitExpr, InstrSeqBuilder, LocalId, MemoryId, Module, ValType,
};

pub struct Compiler<'sym, 'typ> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
}

const START_FUNC_NAME: &str = "__start";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct GlobalId(SymbolId, SymbolId);

impl<'sym, 'typ> Compiler<'sym, 'typ> {
    pub fn new(symbol_loader: &'sym SymbolLoader, type_loader: &'typ TypeLoader) -> Self {
        Self {
            symbol_loader,
            type_loader,
        }
    }

    pub fn compile(
        &self,
        packages: Vec<Rc<Package>>,
        main_package: SymbolId,
        output_file: &str,
    ) -> Result<(), Box<dyn Error>> {
        let main_package_name = self.symbol_loader.get_symbol(main_package).unwrap();
        let mut module = Module::default();
        module.name = Some(String::from(main_package_name.as_ref()));

        ProgramCompiler::new(
            self.symbol_loader,
            self.type_loader,
            &mut module,
            &packages,
            main_package,
        )
        .compile();

        module.emit_wasm_file(output_file)?;
        Ok(())
    }
}

pub struct ProgramCompiler<'sym, 'typ, 'pkg> {
    symbol_loader: &'sym SymbolLoader,
    type_loader: &'typ TypeLoader,
    module: &'pkg mut Module,
    packages: &'pkg [Rc<Package>],
    main_package: SymbolId,
    reachable_functions: HashSet<GlobalId>,
    data_offset_table: HashMap<(SymbolId, usize), usize>,
    function_ids: HashMap<GlobalId, FunctionId>,
    builtin_functions: HashMap<GlobalId, Rc<str>>,
}

impl<'sym, 'typ, 'pkg> ProgramCompiler<'sym, 'typ, 'pkg> {
    pub fn new(
        symbol_loader: &'sym SymbolLoader,
        type_loader: &'typ TypeLoader,
        module: &'pkg mut Module,
        packages: &'pkg [Rc<Package>],
        main_package: SymbolId,
    ) -> Self {
        Self {
            symbol_loader,
            type_loader,
            module,
            packages,
            main_package,
            reachable_functions: HashSet::default(),
            data_offset_table: HashMap::default(),
            function_ids: HashMap::default(),
            builtin_functions: HashMap::default(),
        }
    }

    fn compile(&mut self) {
        self.calculate_reachable_functions();
        self.declare_native_functions();
        self.declare_local_functions();
        self.build_main_func();
        let memory_id = self.build_global_memory();
        let data_size = self.build_data_segment(memory_id);
        self.build_mem_pointers(data_size);

        let functions = self.packages.iter().flat_map(|pkg| pkg.functions.iter());
        for func in functions {
            let func_global_id = GlobalId(func.package_name, func.function_name);
            if !self.reachable_functions.contains(&func_global_id) {
                continue;
            }

            let func_id = self.function_ids.get(&func_global_id).unwrap();
            let wasm_func = self.module.funcs.get_mut(*func_id);
            let FunctionKind::Local(ref mut wasm_func) = wasm_func.kind else {
                unreachable!();
            };

            let mut variables: Vec<_> = wasm_func.args.to_vec();
            for local in func.locals.iter().skip(func.func_type.parameters.len()) {
                let ty = self.type_loader.get_type(*local).unwrap();
                let ty = to_wasm_type(&ty);
                variables.push(self.module.locals.add(ty));
            }

            let builder = wasm_func.builder_mut();

            let mut body_builder = builder.func_body();
            let mut func_compiler = FunctionCompiler::new(
                memory_id,
                &variables,
                self.type_loader,
                &self.data_offset_table,
                &self.function_ids,
                &self.builtin_functions,
            );
            func_compiler.process_statement(&mut body_builder, &func.body);
        }
    }

    fn calculate_reachable_functions(&mut self) {
        let mut local_funcs = HashMap::new();
        for pkg in self.packages {
            for func in &pkg.functions {
                let global_id = GlobalId(pkg.name, func.function_name);
                local_funcs.insert(global_id, &func.body);
            }
        }

        let mut stack = vec![];
        let mut in_stack = HashSet::new();

        let main_sym = self.symbol_loader.declare_symbol("main");
        stack.push(GlobalId(self.main_package, main_sym));
        in_stack.insert(GlobalId(self.main_package, main_sym));

        while let Some(func_id) = stack.pop() {
            if self.reachable_functions.contains(&func_id) {
                continue;
            }
            self.reachable_functions.insert(func_id);

            let Some(body) = local_funcs.get(&func_id) else {
                continue;
            };

            let mut called_funcs = vec![];
            Self::get_called_functions(body, &mut called_funcs);
            for called_func_id in called_funcs {
                if !in_stack.contains(&called_func_id) {
                    stack.push(called_func_id);
                    in_stack.insert(called_func_id);
                }
            }
        }
    }

    fn get_called_functions(stmt: &Statement, result: &mut Vec<GlobalId>) {
        match stmt {
            Statement::Invalid | Statement::Continue | Statement::Break => (),
            Statement::SetLocal(_, expr) => Self::get_called_functions_in_expr(expr, result),
            Statement::SetIndex { target, index, value } => {
                Self::get_called_functions_in_expr(target, result);
                Self::get_called_functions_in_expr(index, result);
                Self::get_called_functions_in_expr(value, result);
            }
            Statement::SetAddr { addr, value } => {
                Self::get_called_functions_in_expr(addr, result);
                Self::get_called_functions_in_expr(value, result);
            }
            Statement::If(if_stmt) => {
                Self::get_called_functions_in_expr(&if_stmt.condition, result);
                Self::get_called_functions(&if_stmt.body, result);
                if let Some(ref else_body) = if_stmt.else_body {
                    Self::get_called_functions(else_body, result);
                }
            }
            Statement::While(while_stmt) => {
                Self::get_called_functions_in_expr(&while_stmt.condition, result);
                Self::get_called_functions(&while_stmt.body, result);
            }
            Statement::Block(block_stmt) => {
                for stmt in &block_stmt.statements {
                    Self::get_called_functions(stmt, result);
                }
            }
            Statement::Return(ret_stmt) => {
                if let Some(ref expr) = ret_stmt.value {
                    Self::get_called_functions_in_expr(expr, result);
                }
            }
            Statement::Expr(expr) => Self::get_called_functions_in_expr(expr, result),
        }
    }

    fn get_called_functions_in_expr(expr: &Expr, result: &mut Vec<GlobalId>) {
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
            | ExprKind::Usize(..)
            | ExprKind::Local(..)
            | ExprKind::StringLit(..)
            | ExprKind::Pointer(..)
            | ExprKind::Deref(..) => (),
            ExprKind::Func(func_expr) => {
                result.push(GlobalId(func_expr.package_name, func_expr.function_name));
            }
            ExprKind::Binary { a, op: _, b } => {
                Self::get_called_functions_in_expr(a, result);
                Self::get_called_functions_in_expr(b, result);
            }
            ExprKind::Unary { val, op: _ } => {
                Self::get_called_functions_in_expr(val, result);
            }
            ExprKind::Call(func, args) => {
                Self::get_called_functions_in_expr(func, result);
                for arg in args {
                    Self::get_called_functions_in_expr(arg, result);
                }
            }
            ExprKind::Index(target, index) => {
                Self::get_called_functions_in_expr(target, result);
                Self::get_called_functions_in_expr(index, result);
            }
            ExprKind::Cast(value, _) => {
                Self::get_called_functions_in_expr(value, result);
            }
        }
    }

    fn declare_native_functions(&mut self) {
        let native_functions = self.packages.iter().flat_map(|pkg| pkg.native_functions.iter());
        for func in native_functions {
            let func_global_id = GlobalId(func.package_name, func.function_name);
            if !self.reachable_functions.contains(&func_global_id) {
                continue;
            }

            let wasm_link_sym = self.symbol_loader.declare_symbol("wasm_link");
            let mut is_external_func = false;

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
                        let ret_type = to_wasm_type(&ret_type);
                        return_type = vec![ret_type];
                    }
                    let mut param_types = vec![];
                    for param_ty in &func.func_type.parameters {
                        let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                        let param_ty = to_wasm_type(&param_ty);
                        param_types.push(param_ty);
                    }

                    let type_id = self.module.types.add(&param_types, &return_type);
                    let (func_id, _) = self.module.add_import_func(&wasm_module, &wasm_name, type_id);
                    self.function_ids.insert(func_global_id, func_id);
                }
            }

            let wasm_builtin_sym = self.symbol_loader.declare_symbol("wasm_builtin");
            let mut is_builtin_func = false;
            if let Some(wasm_builtin_tag) = func.tags.iter().find(|tag| tag.name == wasm_builtin_sym) {
                if let Some(builtin_name) = wasm_builtin_tag.arguments.first() {
                    let builtin_name = String::from_utf8(builtin_name.to_vec())
                        .expect("the function builtin name has to be a valud utf-8 string");
                    // TODO: check the builtin name, and the signature.
                    self.builtin_functions.insert(func_global_id, builtin_name.into());
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

    fn declare_local_functions(&mut self) {
        let functions = self.packages.iter().flat_map(|pkg| pkg.functions.iter());
        for func in functions {
            let global_id = GlobalId(func.package_name, func.function_name);
            if !self.reachable_functions.contains(&global_id) {
                continue;
            }

            let package_name = self.symbol_loader.get_symbol(func.package_name).unwrap();
            let func_name = self.symbol_loader.get_symbol(func.function_name).unwrap();

            let mut return_type = vec![];
            if let Some(ret_type) = func.func_type.return_type {
                let ret_type = self.type_loader.get_type(ret_type).unwrap();
                let ret_type = to_wasm_type(&ret_type);
                return_type = vec![ret_type];
            }

            let mut param_types = vec![];
            let mut variables = vec![];
            for param_ty in &func.func_type.parameters {
                let param_ty = self.type_loader.get_type(*param_ty).unwrap();
                let param_ty = to_wasm_type(&param_ty);
                param_types.push(param_ty);
                variables.push(self.module.locals.add(param_ty));
            }

            let mut builder = FunctionBuilder::new(&mut self.module.types, &param_types, &return_type);
            let mangled_name = mangle_func(&package_name, &func_name);
            builder.name(mangled_name);

            let function_id = builder.finish(
                variables.iter().take(param_types.len()).cloned().collect(),
                &mut self.module.funcs,
            );

            self.function_ids.insert(global_id, function_id);
        }
    }

    fn build_main_func(&mut self) {
        let main_sym = self.symbol_loader.declare_symbol("main");
        let main_func_global_id = GlobalId(self.main_package, main_sym);
        let main_func_id = self.function_ids.get(&main_func_global_id).unwrap();

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
            }
        }

        offset
    }

    fn build_mem_pointers(&mut self, data_size: usize) {
        self.module.globals.add_local(
            ValType::I32,
            false,
            InitExpr::Value(walrus::ir::Value::I32(data_size as i32)),
        );

        let stack_size = 0x1000;
        let heap_start_offset = data_size + stack_size;
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

struct FunctionCompiler<'typ, 'pkg> {
    type_loader: &'typ TypeLoader,
    variables: &'pkg [LocalId],

    memory_id: MemoryId,
    loop_blocks: Vec<(InstrSeqId, InstrSeqId)>,
    function_ids: &'pkg HashMap<GlobalId, FunctionId>,
    data_offsets: &'pkg HashMap<(SymbolId, usize), usize>,
    builtin_functions: &'pkg HashMap<GlobalId, Rc<str>>,
}

impl<'typ, 'pkg> FunctionCompiler<'typ, 'pkg> {
    pub fn new(
        memory_id: MemoryId,
        variables: &'pkg [LocalId],
        type_loader: &'typ TypeLoader,
        data_offsets: &'pkg HashMap<(SymbolId, usize), usize>,
        function_ids: &'pkg HashMap<GlobalId, FunctionId>,
        builtin_functions: &'pkg HashMap<GlobalId, Rc<str>>,
    ) -> Self {
        Self {
            type_loader,
            memory_id,
            variables,
            loop_blocks: vec![],
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
            Statement::SetIndex { target, index, value } => {
                let ty = self.type_loader.get_type(target.type_id).unwrap();
                let Type::Slice(slice_ty) = ty.as_ref() else {
                    unreachable!();
                };

                let element_ty = self.type_loader.get_type(slice_ty.element_type).unwrap();
                let index_ty = self.type_loader.get_type(index.type_id).unwrap();
                let (size, align) = get_size_and_alignment(&element_ty);

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
            Statement::SetAddr { addr, value } => {
                let element_ty = self.type_loader.get_type(value.type_id).unwrap();

                let (_, align) = get_size_and_alignment(&element_ty);
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
            ExprKind::Usize(val) => {
                builder.i32_const(*val as i32);
            }
            ExprKind::Local(index) => {
                builder.local_get(self.variables[*index]);
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
                let Type::Slice(slice_ty) = ty.as_ref() else {
                    unreachable!();
                };
                let element_ty = self.type_loader.get_type(slice_ty.element_type).unwrap();
                let index_ty = self.type_loader.get_type(index.type_id).unwrap();
                let (size, align) = get_size_and_alignment(&element_ty);

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
            ExprKind::Cast(value, target_ty) => {
                self.process_cast_expr(builder, value, *target_ty);
            }
            ExprKind::Binary { a, op, b } => {
                self.process_binary_expr(builder, a, *op, b);
            }
            ExprKind::Unary { op, val } => {
                self.process_unary_expr(builder, *op, val);
            }
            ExprKind::Pointer(addr, _) => {
                builder.i32_const(*addr as i32);
            }
            ExprKind::Deref(addr) => {
                let ty = self.type_loader.get_type(addr.type_id).unwrap();
                let Type::Pointer(pointer_ty) = ty.as_ref() else {
                    unreachable!();
                };
                let element_ty = self.type_loader.get_type(pointer_ty.element_type).unwrap();

                let (_, align) = get_size_and_alignment(&element_ty);
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
            ExprKind::Func(..) => unreachable!("function pointer is not supported yet"),
        }
    }

    fn process_call_expr(&self, builder: &mut InstrSeqBuilder, target: &Expr, arguments: &[Expr]) {
        for arg in arguments.iter() {
            self.process_expr(builder, arg);
        }

        let ExprKind::Func(func_expr) = &target.kind else {
            todo!("currently, calling by function pointer is not supported yet");
        };

        let ident_pair = GlobalId(func_expr.package_name, func_expr.function_name);
        if let Some(func_id) = self.function_ids.get(&ident_pair) {
            builder.call(*func_id);
        } else if let Some(builtin_name) = self.builtin_functions.get(&ident_pair) {
            self.process_builtin_call(builder, builtin_name);
        } else {
            unreachable!("function definition is not found");
        }
    }

    fn process_builtin_call(&self, builder: &mut InstrSeqBuilder, builtin_name: &str) {
        match builtin_name {
            "memory.grow" => {
                builder.memory_grow(self.memory_id);
            }
            "memory.size" => {
                builder.memory_size(self.memory_id);
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
            (Type::I64 | Type::U64, Type::I32 | Type::U32 | Type::Pointer(..)) => {
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
            (Type::I32, Type::I64) => {
                builder.unop(UnaryOp::I64Extend32S);
            }
            (Type::I32, _) => {}
            (Type::U32, Type::I64 | Type::U64) => {
                builder.unop(UnaryOp::I64ExtendUI32);
            }
            (Type::U32, _) => {}
            (Type::I16, Type::I64) => {
                builder.unop(UnaryOp::I64Extend16S);
            }
            (Type::I16, _) => {}
            (Type::U16, Type::I64 | Type::U64) => {
                builder.unop(UnaryOp::I64ExtendUI32);
            }
            (Type::U16, _) => {}
            (Type::I8, Type::I64) => {
                builder.unop(UnaryOp::I64Extend8S);
            }
            (Type::I8, _) => {}
            (Type::U8, Type::I64 | Type::U64) => {
                builder.unop(UnaryOp::I64ExtendUI32);
            }
            (Type::U8, _) => {}
            (Type::Slice(..), Type::I64) => {
                builder.unop(UnaryOp::I64Extend32S);
            }
            (Type::Slice(..), _) => {}
            (Type::I64, Type::Slice(..)) => {
                builder.unop(UnaryOp::I32WrapI64);
            }
            (source, target) => todo!(
                "casting from {} to {} is not supported yet",
                source.display(self.type_loader),
                target.display(self.type_loader)
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
            (BinOp::Add, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
                builder.binop(BinaryOp::I32Add)
            }
            (BinOp::Add, Type::F64) => builder.binop(BinaryOp::F64Add),
            (BinOp::Add, Type::F32) => builder.binop(BinaryOp::F32Add),
            (BinOp::Add, Type::Pointer(ptr_ty)) => {
                let element_ty = self.type_loader.get_type(ptr_ty.element_type).unwrap();
                let (size, _) = get_size_and_alignment(&element_ty);
                builder.i32_const(size as i32);
                builder.binop(BinaryOp::I32Mul);
                builder.binop(BinaryOp::I32Add)
            }

            (BinOp::Sub, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Sub),
            (BinOp::Sub, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
                builder.binop(BinaryOp::I32Sub)
            }
            (BinOp::Sub, Type::F64) => builder.binop(BinaryOp::F64Sub),
            (BinOp::Sub, Type::F32) => builder.binop(BinaryOp::F32Sub),
            (BinOp::Sub, Type::Pointer(ptr_ty)) => {
                let element_ty = self.type_loader.get_type(ptr_ty.element_type).unwrap();
                let (size, _) = get_size_and_alignment(&element_ty);
                builder.i32_const(size as i32);
                builder.binop(BinaryOp::I32Mul);
                builder.binop(BinaryOp::I32Sub)
            }

            (BinOp::Mul, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Mul),
            (BinOp::Mul, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
                builder.binop(BinaryOp::I32Mul)
            }
            (BinOp::Mul, Type::F64) => builder.binop(BinaryOp::F64Mul),
            (BinOp::Mul, Type::F32) => builder.binop(BinaryOp::F32Mul),

            (BinOp::Div, Type::I64) => builder.binop(BinaryOp::I64DivS),
            (BinOp::Div, Type::U64) => builder.binop(BinaryOp::I64DivU),
            (BinOp::Div, Type::I32) => builder.binop(BinaryOp::I32DivS),
            (BinOp::Div, Type::U32) => builder.binop(BinaryOp::I32DivU),
            (BinOp::Div, Type::F64) => builder.binop(BinaryOp::F64Div),
            (BinOp::Div, Type::F32) => builder.binop(BinaryOp::F32Div),

            (BinOp::Mod, Type::I64) => builder.binop(BinaryOp::I64RemS),
            (BinOp::Mod, Type::U64) => builder.binop(BinaryOp::I64RemU),
            (BinOp::Mod, Type::I32) => builder.binop(BinaryOp::I32RemS),
            (BinOp::Mod, Type::U32) => builder.binop(BinaryOp::I32RemU),

            (BinOp::BitOr, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Or),
            (BinOp::BitOr, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Or),

            (BinOp::BitAnd, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64And),
            (BinOp::BitAnd, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32And),

            (BinOp::BitXor, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Xor),
            (BinOp::BitXor, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Xor),

            (BinOp::ShiftLeft, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Shl),
            (BinOp::ShiftLeft, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Shl),
            (BinOp::ShiftRight, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64ShrU),
            (BinOp::ShiftRight, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32ShrU),

            (BinOp::Eq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Eq),
            (BinOp::Eq, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Eq),
            (BinOp::Eq, Type::F64) => builder.binop(BinaryOp::F64Eq),
            (BinOp::Eq, Type::F32) => builder.binop(BinaryOp::F32Eq),

            (BinOp::NEq, Type::I64 | Type::U64) => builder.binop(BinaryOp::I64Ne),
            (BinOp::NEq, Type::I32 | Type::U32) => builder.binop(BinaryOp::I32Ne),
            (BinOp::NEq, Type::F64) => builder.binop(BinaryOp::F64Ne),
            (BinOp::NEq, Type::F32) => builder.binop(BinaryOp::F32Ne),

            (BinOp::Gt, Type::I64) => builder.binop(BinaryOp::I64GtS),
            (BinOp::Gt, Type::I32) => builder.binop(BinaryOp::I32GtS),
            (BinOp::Gt, Type::U64) => builder.binop(BinaryOp::I64GtU),
            (BinOp::Gt, Type::U32) => builder.binop(BinaryOp::I32GtU),
            (BinOp::Gt, Type::F32) => builder.binop(BinaryOp::F32Gt),
            (BinOp::Gt, Type::F64) => builder.binop(BinaryOp::F32Gt),

            (BinOp::GEq, Type::I64) => builder.binop(BinaryOp::I64GeS),
            (BinOp::GEq, Type::I32) => builder.binop(BinaryOp::I32GeS),
            (BinOp::GEq, Type::U64) => builder.binop(BinaryOp::I64GeU),
            (BinOp::GEq, Type::U32) => builder.binop(BinaryOp::I32GeU),
            (BinOp::GEq, Type::F32) => builder.binop(BinaryOp::F32Ge),
            (BinOp::GEq, Type::F64) => builder.binop(BinaryOp::F32Ge),

            (BinOp::Lt, Type::I64) => builder.binop(BinaryOp::I64LtS),
            (BinOp::Lt, Type::I32) => builder.binop(BinaryOp::I32LtS),
            (BinOp::Lt, Type::U64) => builder.binop(BinaryOp::I64LtU),
            (BinOp::Lt, Type::U32) => builder.binop(BinaryOp::I32LtU),
            (BinOp::Lt, Type::F32) => builder.binop(BinaryOp::F32Lt),
            (BinOp::Lt, Type::F64) => builder.binop(BinaryOp::F32Lt),

            (BinOp::LEq, Type::I64) => builder.binop(BinaryOp::I64LeS),
            (BinOp::LEq, Type::I32) => builder.binop(BinaryOp::I32LeS),
            (BinOp::LEq, Type::U64) => builder.binop(BinaryOp::I64LeU),
            (BinOp::LEq, Type::U32) => builder.binop(BinaryOp::I32LeU),
            (BinOp::LEq, Type::F32) => builder.binop(BinaryOp::F32Le),
            (BinOp::LEq, Type::F64) => builder.binop(BinaryOp::F32Le),

            // TODO: pointer arithmetic.
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
            (UnOp::BitNot, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
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
            (UnOp::Sub, Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8) => {
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

fn mangle_func(package_name: &str, function_name: &str) -> String {
    format!("{package_name}.{function_name}")
}

fn to_wasm_type(ty: &Type) -> ValType {
    match ty {
        Type::I64 | Type::U64 => ValType::I64,
        Type::I32 | Type::U32 | Type::I16 | Type::U16 | Type::I8 | Type::U8 | Type::Bool => ValType::I32,
        Type::F64 => ValType::F64,
        Type::F32 => ValType::F32,
        Type::Slice(_) => ValType::I32,
        Type::Pointer(_) => ValType::I32,
        Type::Invalid | Type::Void | Type::Func(..) => todo!(),
    }
}

// can we guarantee: size is multiple of alignment?
fn get_size_and_alignment(ty: &Type) -> (usize, usize) {
    match ty {
        Type::I64 | Type::U64 => (8, 8),
        Type::I32 | Type::U32 => (4, 4),
        Type::I16 | Type::U16 => (2, 2),
        Type::I8 | Type::U8 => (1, 1),
        Type::F64 => (8, 8),
        Type::F32 => (4, 4),
        Type::Bool => (1, 1),
        Type::Slice(..) => (4, 4),
        Type::Pointer(..) => (4, 4),
        Type::Invalid | Type::Void | Type::Func(..) => todo!(),
    }
}
