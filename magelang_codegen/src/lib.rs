use indexmap::IndexMap;
use magelang_analyzer::*;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use wasm_helper as wasm;

pub fn generate_wasm_ir(module: Package) -> wasm::Module {
    let mut generator = Generator::new(module);
    generator.build()
}

struct Generator {
    module: Package,

    data: IndexMap<Rc<[u8]>, u32>,
    data_end: u32,
    imports: Vec<wasm::Import>,
    struct_layouts: HashMap<TypeId, StructLayout>,
    func_map: HashMap<usize, wasm::FuncIdx>,
    unused_func_id: wasm::FuncIdx,
    func_type_cache: RefCell<IndexMap<wasm::FuncType, wasm::TypeIdx>>,
    globals: Vec<wasm::Global>,
    global_maps: Vec<u32>,
    functions: Vec<wasm::Func>,
    func_elems: Vec<wasm::Elem>,
    exports: Vec<wasm::Export>,
    starting_point: Option<wasm::FuncIdx>,
    locals: LocalManager,
}

#[derive(Clone, Copy)]
enum VariableLoc {
    Global(u32),
    Local(u32),
}

impl VariableLoc {
    fn with_offset(self, offset: u32) -> Self {
        match self {
            Self::Global(id) => Self::Global(id + offset),
            Self::Local(id) => Self::Local(id + offset),
        }
    }

    fn add_offset(&mut self, offset: u32) {
        match self {
            Self::Global(id) => *id += offset,
            Self::Local(id) => *id += offset,
        }
    }
}

impl Generator {
    fn new(module: Package) -> Self {
        Self {
            module,
            data: IndexMap::default(),
            data_end: 0,
            imports: Vec::default(),
            struct_layouts: HashMap::default(),
            func_type_cache: RefCell::default(),
            func_map: HashMap::default(),
            unused_func_id: 0,
            globals: Vec::default(),
            global_maps: Vec::default(),
            functions: Vec::default(),
            func_elems: Vec::default(),
            exports: Vec::default(),
            starting_point: None,
            locals: LocalManager::default(),
        }
    }

    fn build(&mut self) -> wasm::Module {
        self.build_data();
        self.build_struct_layout();
        self.build_global_maps();
        self.build_functions();

        let mut datas = Vec::default();
        for (data, offset) in self.data.drain(..) {
            datas.push(wasm::Data {
                init: data.to_vec(),
                mode: wasm::DataMode::Active {
                    memory: 0,
                    offset: wasm::Expr(vec![wasm::Instr::I32Const(offset as i32)]),
                },
            });
        }

        self.exports.push(wasm::Export {
            name: "memory".to_string(),
            desc: wasm::ExportDesc::Mem(0),
        });

        wasm::Module {
            types: self.func_type_cache.take().into_keys().collect(),
            funcs: std::mem::take(&mut self.functions),
            tables: vec![wasm::TableType {
                limits: wasm::Limits {
                    min: self.func_elems.len() as u32,
                    max: None,
                },
                ref_type: wasm::RefType::FuncRef,
            }],
            mems: vec![wasm::Mem { min: 1, max: None }],
            globals: std::mem::take(&mut self.globals),
            elems: std::mem::take(&mut self.func_elems),
            datas,
            start: self.starting_point,
            imports: std::mem::take(&mut self.imports),
            exports: std::mem::take(&mut self.exports),
        }
    }

    fn build_data(&mut self) {
        let mut data_map = IndexMap::<Rc<[u8]>, u32>::default();
        for global in &self.module.globals {
            Self::build_data_from_expr(&mut data_map, &global.value);
        }
        for func in &self.module.functions {
            Self::build_data_from_stmt(&mut data_map, &func.statement);
        }

        self.data_end = data_map
            .last()
            .map(|(raw, offset)| raw.len() as u32 + *offset)
            .unwrap_or(8u32);
        self.data = data_map;
    }

    fn build_data_from_stmt(data_map: &mut IndexMap<Rc<[u8]>, u32>, stmt: &Statement) {
        match stmt {
            Statement::Block(statements) => {
                for stmt in statements {
                    Self::build_data_from_stmt(data_map, stmt);
                }
            }
            Statement::If(if_stmt) => {
                Self::build_data_from_expr(data_map, &if_stmt.cond);
                Self::build_data_from_stmt(data_map, &if_stmt.body);
                if let Some(ref else_body) = if_stmt.else_stmt {
                    Self::build_data_from_stmt(data_map, else_body);
                }
            }
            Statement::While(while_stmt) => {
                Self::build_data_from_expr(data_map, &while_stmt.cond);
                Self::build_data_from_stmt(data_map, &while_stmt.body);
            }
            Statement::Return(Some(val)) => {
                Self::build_data_from_expr(data_map, val);
            }
            Statement::Expr(expr) => Self::build_data_from_expr(data_map, expr),
            Statement::Assign(target, value) => {
                Self::build_data_from_expr(data_map, target);
                Self::build_data_from_expr(data_map, value);
            }
            Statement::Native | Statement::Return(..) | Statement::Continue | Statement::Break => {}
        }
    }

    fn build_data_from_expr(data_map: &mut IndexMap<Rc<[u8]>, u32>, expr: &Expr) {
        match &expr.kind {
            ExprKind::ConstI8(..)
            | ExprKind::ConstI16(..)
            | ExprKind::ConstI32(..)
            | ExprKind::ConstI64(..)
            | ExprKind::ConstIsize(..)
            | ExprKind::ConstF32(..)
            | ExprKind::ConstF64(..)
            | ExprKind::ConstBool(..)
            | ExprKind::Zero(..)
            | ExprKind::Local(..)
            | ExprKind::Global(..)
            | ExprKind::Func(..) => {}
            ExprKind::StructLit(_, values) => {
                for val in values {
                    Self::build_data_from_expr(data_map, val);
                }
            }
            ExprKind::Bytes(data) => {
                if !data_map.contains_key(data) {
                    let next_id = data_map
                        .last()
                        .map(|(bytes, id)| *id + bytes.len() as u32)
                        .unwrap_or(8);
                    data_map.insert(data.clone(), next_id);
                }
            }
            ExprKind::GetElement(expr, _)
            | ExprKind::GetElementAddr(expr, _)
            | ExprKind::Neg(expr)
            | ExprKind::BitNot(expr)
            | ExprKind::Not(expr)
            | ExprKind::Cast(expr, _)
            | ExprKind::Deref(expr) => Self::build_data_from_expr(data_map, expr),
            ExprKind::GetIndex(expr, index) => {
                Self::build_data_from_expr(data_map, expr);
                Self::build_data_from_expr(data_map, index);
            }
            ExprKind::Call(callee, args) => {
                Self::build_data_from_expr(data_map, callee);
                for arg in args {
                    Self::build_data_from_expr(data_map, arg);
                }
            }
            ExprKind::Add(a, b)
            | ExprKind::Sub(a, b)
            | ExprKind::Mul(a, b)
            | ExprKind::Div(a, b)
            | ExprKind::Mod(a, b)
            | ExprKind::BitOr(a, b)
            | ExprKind::BitAnd(a, b)
            | ExprKind::BitXor(a, b)
            | ExprKind::ShiftLeft(a, b)
            | ExprKind::ShiftRight(a, b)
            | ExprKind::And(a, b)
            | ExprKind::Or(a, b)
            | ExprKind::Eq(a, b)
            | ExprKind::NEq(a, b)
            | ExprKind::Gt(a, b)
            | ExprKind::GEq(a, b)
            | ExprKind::Lt(a, b)
            | ExprKind::LEq(a, b) => {
                Self::build_data_from_expr(data_map, a);
                Self::build_data_from_expr(data_map, b);
            }
        }
    }

    fn build_struct_layout(&mut self) {
        for (idx, ty) in self.module.types.iter().enumerate() {
            let Type::Struct(struct_type) = ty else {
                continue;
            };
            let struct_layout = build_struct_layout(&self.module, struct_type);
            self.struct_layouts.insert(TypeId(idx), struct_layout);
        }
    }

    fn build_global_maps(&mut self) {
        let mut unused_global = 0u32;
        for global in &self.module.globals {
            let ty = &self.module.types[global.type_id.0];
            let val_types = self.build_wasm_val_type(ty);

            self.global_maps.push(unused_global);
            unused_global += val_types.len() as u32;

            for val_type in val_types {
                let init = wasm::Expr(Self::build_zero_val_type(&val_type));
                self.globals.push(wasm::Global {
                    ty: wasm::GlobalType {
                        mutability: wasm::Mut::Var,
                        ty: val_type,
                    },
                    init,
                })
            }
        }
    }

    fn build_functions(&mut self) {
        enum InstrinsicFunc {
            MemorySize,
            MemoryGrow,
            DataEnd,
            SizeOf,
            AlignOf,
        }

        struct MappedFunction<'a> {
            name: String,
            id: usize,
            is_native: bool,
            type_id: wasm::TypeIdx,

            import: Option<(&'a str, &'a str)>,
            export: Option<&'a str>,
            intrinsic: Option<InstrinsicFunc>,
        }

        let mut func_with_bodies = Vec::default();
        let mut main_func = None;
        let mut mapped_functions = Vec::default();

        for (i, func) in self.module.functions.iter().enumerate() {
            let mut parameters = Vec::default();
            for type_id in &func.ty.parameters {
                let ty = &self.module.types[type_id.0];
                let val_types = self.build_wasm_val_type(ty);
                parameters.extend(val_types);
            }
            let returns = self.build_wasm_val_type(&self.module.types[func.ty.return_type.0]);
            let func_wasm_type_id = self.get_func_type(wasm::FuncType {
                parameters: parameters.clone(),
                returns,
            });

            let func_name = self.mangle_object_id(&func.id);
            let is_native = matches!(func.statement, Statement::Native);

            let mut import = None;
            let mut export = None;
            let mut wasm_instr = None;

            for annotation in func.annotations.iter() {
                match annotation.name.as_str() {
                    "wasm_import" => {
                        if import.is_some() {
                            todo!("found duplicate wasm_import annotation");
                        }
                        if annotation.arguments.len() != 2 {
                            todo!(
                                "expected to have 2 arguments in wasm_import, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(import_module) = annotation.arguments.get(0) else {
                            todo!("missing module name in the wasm_import annotation");
                        };
                        let Some(import_name) = annotation.arguments.get(1) else {
                            todo!("missing import name in the wasm_import annotation");
                        };
                        import = Some((import_module.as_str(), import_name.as_str()));
                    }
                    "wasm_export" => {
                        if export.is_some() {
                            todo!("found duplicate wasm_export annotation");
                        }
                        if annotation.arguments.len() != 1 {
                            todo!(
                                "expected to have 1 argument in wasm_export, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(export_name) = annotation.arguments.get(0) else {
                            todo!("missing export name in the wasm_export annotation");
                        };
                        export = Some(export_name.as_str());
                    }
                    "intrinsic" => {
                        if wasm_instr.is_some() {
                            todo!("found duplicate wasm_instr annotation");
                        }
                        if annotation.arguments.len() != 1 {
                            todo!(
                                "expected to have 1 argument in wasm_instr, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        let Some(name) = annotation.arguments.get(0) else {
                            todo!("missing wasm instr in the wasm_instr annotation");
                        };
                        // TODO: check function signature. They should match the wasm instructions.
                        let instr = match name.as_str() {
                            "memory.size" => InstrinsicFunc::MemorySize,
                            "memory.grow" => InstrinsicFunc::MemoryGrow,
                            "data.end" => InstrinsicFunc::DataEnd,
                            "size_of" => InstrinsicFunc::SizeOf,
                            "align_of" => InstrinsicFunc::AlignOf,
                            instr_name => todo!("unknwon wasm instr {instr_name}"),
                        };
                        wasm_instr = Some(instr);
                    }
                    "main" => {
                        if main_func.is_some() {
                            todo!("found duplicate main annotation");
                        }
                        if !annotation.arguments.is_empty() {
                            todo!(
                                "expected to have no argument in main annotation, but found {}",
                                annotation.arguments.len()
                            );
                        }
                        main_func = Some(i);
                    }
                    name => todo!("found unknown annotation {name}"),
                }
            }

            mapped_functions.push(MappedFunction {
                name: func_name,
                id: i,
                is_native,
                type_id: func_wasm_type_id,
                import,
                export,
                intrinsic: wasm_instr,
            });
        }

        mapped_functions.sort_by(|a, b| match (a.import, b.import) {
            (Some(..), Some(..)) | (None, None) => a.id.cmp(&b.id),
            (Some(..), None) => std::cmp::Ordering::Less,
            (None, Some(..)) => std::cmp::Ordering::Greater,
        });

        for mapped_func in mapped_functions.into_iter() {
            let MappedFunction {
                name,
                id,
                is_native,
                type_id,
                import,
                export,
                intrinsic: wasm_instr,
            } = mapped_func;
            match (import, export, wasm_instr) {
                (None, None, None) => {
                    if is_native {
                        todo!("cannot compile native function");
                    }

                    self.func_map.insert(id, self.unused_func_id);
                    self.unused_func_id += 1;
                    func_with_bodies.push((id, self.functions.len()));

                    self.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(vec![]),
                    });
                }
                (None, None, Some(native_wasm)) => {
                    if !is_native {
                        todo!("function with native wasm should be a native func");
                    }

                    self.func_map.insert(id, self.unused_func_id);
                    self.unused_func_id += 1;

                    self.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(match native_wasm {
                            InstrinsicFunc::MemorySize => vec![wasm::Instr::MemorySize],
                            InstrinsicFunc::MemoryGrow => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::MemoryGrow]
                            }
                            InstrinsicFunc::DataEnd => {
                                vec![wasm::Instr::I32Const(self.data_end as i32)]
                            }
                            InstrinsicFunc::SizeOf => {
                                let func = &self.module.functions[id];
                                let ObjectId::GenericInst {
                                    package_id: _,
                                    name_id: _,
                                    typeargs_id,
                                } = func.id
                                else {
                                    todo!("size_of requires a generic func");
                                };
                                let typeargs = &self.module.typeargs[typeargs_id.0];
                                if typeargs.0.len() != 1 {
                                    todo!(
                                        "size_of expects 1 generic parameter but found {}",
                                        typeargs.0.len()
                                    );
                                }
                                let type_id = typeargs.0[0];
                                let layout =
                                    build_type_layout(&self.module, &self.module.types[type_id.0]);
                                vec![wasm::Instr::I32Const(layout.mem_size as i32)]
                            }
                            InstrinsicFunc::AlignOf => {
                                let func = &self.module.functions[id];
                                let ObjectId::GenericInst {
                                    package_id: _,
                                    name_id: _,
                                    typeargs_id,
                                } = func.id
                                else {
                                    todo!("align_of requires a generic func");
                                };
                                let typeargs = &self.module.typeargs[typeargs_id.0];
                                if typeargs.0.len() != 1 {
                                    todo!(
                                        "align_of expects 1 generic parameter but found {}",
                                        typeargs.0.len()
                                    );
                                }
                                let type_id = typeargs.0[0];
                                let layout =
                                    build_type_layout(&self.module, &self.module.types[type_id.0]);
                                vec![wasm::Instr::I32Const(layout.alignment as i32)]
                            }
                        }),
                    });
                }
                (None, Some(export_name), None) => {
                    if is_native {
                        todo!("cannot compile native function");
                    }

                    let func_id = self.unused_func_id;
                    self.func_map.insert(id, func_id);
                    self.unused_func_id += 1;
                    func_with_bodies.push((id, self.functions.len()));

                    self.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(vec![]),
                    });
                    self.exports.push(wasm::Export {
                        name: export_name.to_string(),
                        desc: wasm::ExportDesc::Func(func_id),
                    });
                }
                (None, Some(..), Some(..)) => todo!("cannot export wasm instrinsic"),
                (Some((import_module, import_name)), None, None) => {
                    if !is_native {
                        todo!("cannot import function to non native function");
                    }

                    self.func_map.insert(id, self.unused_func_id);
                    self.unused_func_id += 1;

                    self.imports.push(wasm::Import {
                        module: import_module.to_string(),
                        name: import_name.to_string(),
                        desc: wasm::ImportDesc::Func(type_id),
                    });
                }
                (Some(..), None, Some(..)) => todo!("cannot import wasm instrinsic"),
                (Some(..), Some(..), None) => todo!("cannot both import and export function"),
                (Some(..), Some(..), Some(..)) => todo!("cannot compile function"),
            }
        }

        for (i, wasm_idx) in func_with_bodies {
            let func = &self.module.functions[i];

            let mut locals = Vec::default();
            for type_id in &func.ty.parameters {
                let ty = &self.module.types[type_id.0];
                let val_types = self.build_wasm_val_type(ty);
                locals.push(
                    val_types
                        .iter()
                        .map(|ty| wasm::Local {
                            name: "".to_string(),
                            ty: *ty,
                        })
                        .collect(),
                );
            }
            self.locals.set_params(locals.into_iter());

            let mut locals = Vec::default();
            for type_id in func.locals.iter().skip(func.ty.parameters.len()) {
                let ty = &self.module.types[type_id.0];
                let val_types = self.build_wasm_val_type(ty);
                locals.push(
                    val_types
                        .iter()
                        .map(|ty| wasm::Local {
                            name: "".to_string(),
                            ty: *ty,
                        })
                        .collect(),
                );
            }
            self.locals.set_locals(locals.into_iter());

            let body = self.build_statement(0, 0, &func.statement);

            self.functions[wasm_idx].body = wasm::Expr(body);
            self.functions[wasm_idx].locals = self.locals.take();
        }

        for i in 0..self.module.functions.len() {
            self.func_elems.push(wasm::Elem {
                ty: wasm::RefType::FuncRef,
                init: vec![wasm::Expr(vec![wasm::Instr::RefFunc(i as u32)])],
                mode: wasm::ElemMode::Active {
                    table: 0,
                    offset: wasm::Expr(vec![wasm::Instr::I32Const(i as i32)]),
                },
            });
        }

        let mut init_func_body = Vec::default();
        for (i, global) in self.module.globals.iter().enumerate() {
            let instrs = self.build_value_expr(&global.value);
            init_func_body.extend(instrs);
            let global_id = self.global_maps[i];
            let instrs =
                self.build_variable_set_instr(VariableLoc::Global(global_id), global.type_id);
            init_func_body.extend(instrs);
        }
        if let Some(main_func_id) = main_func {
            let func_id = *self.func_map.get(&main_func_id).unwrap();
            init_func_body.push(wasm::Instr::Call(func_id));
        }
        self.starting_point = Some(self.unused_func_id);
        self.unused_func_id += 1;
        self.functions.push(wasm::Func {
            name: "__init".to_string(),
            ty: self.get_func_type(wasm::FuncType {
                parameters: vec![],
                returns: vec![],
            }),
            locals: self.locals.take(),
            body: wasm::Expr(init_func_body),
        });
    }

    fn build_statement(
        &self,
        continue_label: u32,
        break_label: u32,
        stmt: &Statement,
    ) -> Vec<wasm::Instr> {
        match stmt {
            Statement::Native => unreachable!("native function should be handled specially"),
            Statement::Block(statements) => {
                let mut result = Vec::default();
                for stmt in statements.iter() {
                    result.extend(self.build_statement(continue_label, break_label, stmt));
                }
                result
            }
            Statement::If(if_stmt) => {
                let mut result = self.build_value_expr(&if_stmt.cond);
                let body = self.build_statement(continue_label + 1, break_label + 1, &if_stmt.body);

                let else_body = if let Some(ref else_body) = if_stmt.else_stmt {
                    self.build_statement(continue_label + 1, break_label + 1, else_body)
                } else {
                    vec![]
                };

                result.push(wasm::Instr::If(wasm::BlockType::None, body, else_body));
                result
            }
            Statement::While(while_stmt) => {
                let cond = self.build_value_expr(&while_stmt.cond);
                let body = self.build_statement(0, 1, &while_stmt.body);

                let mut inner_block = cond;
                inner_block.push(wasm::Instr::I32Eqz);
                inner_block.push(wasm::Instr::BrIf(1));
                inner_block.extend(body);
                inner_block.push(wasm::Instr::Br(0));

                vec![wasm::Instr::Block(
                    wasm::BlockType::None,
                    vec![wasm::Instr::Loop(wasm::BlockType::None, inner_block)],
                )]
            }
            Statement::Return(value) => {
                let mut result = vec![];
                if let Some(val) = value {
                    result.extend(self.build_value_expr(val));
                }
                result.push(wasm::Instr::Return);
                result
            }
            Statement::Expr(expr) => {
                let mut result = self.build_value_expr(expr);
                let types = self.build_wasm_val_type(&self.module.types[expr.ty.0]);
                for _ in types {
                    result.push(wasm::Instr::Drop);
                }
                result
            }
            Statement::Assign(target, expr) => match &target.kind {
                ExprKind::Deref(ptr) => self.build_mem_assign_stmt(0, ptr, expr),
                ExprKind::Local(..) | ExprKind::Global(..) | ExprKind::GetElement(..) => {
                    self.build_variable_assign_stmt(target, expr)
                }
                _ => unreachable!("expression is not assignable"),
            },
            Statement::Continue => vec![wasm::Instr::Br(continue_label)],
            Statement::Break => vec![wasm::Instr::Br(break_label)],
        }
    }

    fn build_variable_assign_stmt(&self, target: &Expr, value: &Expr) -> Vec<wasm::Instr> {
        match &target.kind {
            ExprKind::Local(idx) => {
                let mut result = self.build_value_expr(value);
                let assign = self.build_variable_set_instr(
                    VariableLoc::Local(self.locals.get_local(*idx)),
                    value.ty,
                );
                result.extend(assign);
                result
            }
            ExprKind::Global(idx) => {
                let mut result = self.build_value_expr(value);
                let assign = self.build_variable_set_instr(
                    VariableLoc::Global(self.global_maps[idx.0]),
                    value.ty,
                );
                result.extend(assign);
                result
            }
            ExprKind::GetElement(..) => {
                let mut result = self.build_value_expr(value);
                let idx = Self::get_variable_loc(target);
                result.extend(self.build_variable_set_instr(idx, target.ty));
                result
            }
            _ => unreachable!("expression is not local target"),
        }
    }

    fn get_variable_loc(expr: &Expr) -> VariableLoc {
        match &expr.kind {
            ExprKind::Global(id) => VariableLoc::Global(id.0 as u32),
            ExprKind::Local(id) => VariableLoc::Local(*id as u32),
            ExprKind::GetElement(target, field) => {
                let mut var_idx = Self::get_variable_loc(target);
                var_idx.add_offset(*field as u32);
                var_idx
            }
            _ => unreachable!(),
        }
    }

    fn build_variable_set_instr(&self, idx: VariableLoc, type_id: TypeId) -> Vec<wasm::Instr> {
        let ty = &self.module.types[type_id.0];
        match ty {
            Type::Struct(struct_type) => {
                let struct_layout = &self.struct_layouts.get(&type_id).unwrap();
                struct_type
                    .fields
                    .iter()
                    .enumerate()
                    .rev()
                    .flat_map(|(i, field)| {
                        let offset = struct_layout.idx_offset[i];
                        self.build_variable_set_instr(idx.with_offset(offset), field.ty)
                    })
                    .collect()
            }
            Type::Void => vec![],
            Type::Func(..)
            | Type::Bool
            | Type::Int(..)
            | Type::Float(..)
            | Type::Ptr(..)
            | Type::ArrayPtr(..) => match idx {
                VariableLoc::Global(id) => vec![wasm::Instr::GlobalSet(id)],
                VariableLoc::Local(id) => vec![wasm::Instr::LocalSet(id)],
            },
        }
    }

    fn build_mem_assign_stmt(
        &self,
        offset: u32,
        pointer_expr: &Expr,
        value: &Expr,
    ) -> Vec<wasm::Instr> {
        let ty = &self.module.types[value.ty.0];
        match ty {
            Type::Struct(struct_type) => {
                // TODO: (optimization) improve this to handle special cases.
                let mut result = self.build_value_expr(value);

                let ptr_instr = self.build_value_expr(pointer_expr);
                result.extend(ptr_instr);

                let mut stack = vec![];
                let struct_layout = self.struct_layouts.get(&value.ty).unwrap();
                for (i, field) in struct_type.fields.iter().enumerate() {
                    stack.push((struct_layout, i, field.ty));
                }

                while let Some((struct_layout, field_id, type_id)) = stack.pop() {
                    let ty = &self.module.types[type_id.0];
                    match ty {
                        Type::Struct(struct_type) => {
                            let struct_layout = self.struct_layouts.get(&type_id).unwrap();
                            for (i, field) in struct_type.fields.iter().enumerate() {
                                stack.push((struct_layout, i, field.ty));
                            }
                        }

                        Type::Void => (),

                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I8,
                        })
                        | Type::Bool => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store8(wasm::MemArg { offset, align: 0 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I16,
                        }) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store16(wasm::MemArg { offset, align: 1 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I32 | BitSize::ISize,
                        })
                        | Type::Ptr(..)
                        | Type::ArrayPtr(..)
                        | Type::Func(..) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I64,
                        }) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I64),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I64Store(wasm::MemArg { offset, align: 3 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }

                        Type::Float(FloatType::F32) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::F32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::F32Store(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Float(FloatType::F64) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::F64),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.mem_offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::F64Store(wasm::MemArg { offset, align: 3 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                    }
                }
                result.push(wasm::Instr::Drop);

                result
            }

            Type::Void => self.build_value_expr(value),

            Type::Int(IntType {
                sign: _,
                size: BitSize::I8,
            })
            | Type::Bool => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::I32Store8(wasm::MemArg { offset, align: 0 }));
                result
            }
            Type::Int(IntType {
                sign: _,
                size: BitSize::I16,
            }) => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::I32Store16(wasm::MemArg { offset, align: 1 }));
                result
            }
            Type::Int(IntType {
                sign: _,
                size: BitSize::I32 | BitSize::ISize,
            })
            | Type::Ptr(..)
            | Type::ArrayPtr(..)
            | Type::Func(..) => {
                let mut result = self.build_value_expr(pointer_expr);

                let value_instr = self.build_value_expr(value);
                result.extend(value_instr);

                result.push(wasm::Instr::I32Store(wasm::MemArg { offset, align: 2 }));
                result
            }
            Type::Int(IntType {
                sign: _,
                size: BitSize::I64,
            }) => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::I64Store(wasm::MemArg { offset, align: 3 }));
                result
            }

            Type::Float(FloatType::F32) => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::F32Store(wasm::MemArg { offset, align: 2 }));
                result
            }
            Type::Float(FloatType::F64) => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::F64Store(wasm::MemArg { offset, align: 3 }));
                result
            }
        }
    }

    fn build_value_expr(&self, expr: &Expr) -> Vec<wasm::Instr> {
        match &expr.kind {
            ExprKind::ConstI8(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI16(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI32(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI64(val) => vec![wasm::Instr::I64Const(*val as i64)],
            ExprKind::ConstIsize(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstF32(val) => vec![wasm::Instr::F32Const(*val)],
            ExprKind::ConstF64(val) => vec![wasm::Instr::F64Const(*val)],
            ExprKind::ConstBool(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::Zero(type_id) => self.build_zero_type(&self.module.types[type_id.0]),
            ExprKind::StructLit(_, values) => values
                .iter()
                .flat_map(|expr| self.build_value_expr(expr))
                .collect(),
            ExprKind::Bytes(bytes) => {
                vec![wasm::Instr::I32Const(*self.data.get(bytes).unwrap() as i32)]
            }
            ExprKind::Local(idx) => self.build_value_expr_from_var(
                VariableLoc::Local(self.locals.get_local(*idx)),
                expr.ty,
            ),
            ExprKind::Global(idx) => self
                .build_value_expr_from_var(VariableLoc::Global(self.global_maps[idx.0]), expr.ty),
            ExprKind::Func(idx) => {
                vec![wasm::Instr::I32Const(
                    *self.func_map.get(&idx.0).unwrap() as i32
                )]
            }
            ExprKind::GetElement(struct_expr, field) => {
                // TODO: (optimization) optimize this for various cases like selecting field in local/global var,
                // or dereference field from memory.

                let mut result = self.build_value_expr(struct_expr);
                let struct_layout = self.struct_layouts.get(&struct_expr.ty).unwrap();
                let target_offset = struct_layout.idx_offset[*field];
                // TODO: (optimization) optimize field layout calculation by using cache.
                let field_layout = build_type_layout(&self.module, &self.module.types[expr.ty.0]);
                let field_size = field_layout.idx_size;
                let target_size = struct_layout.layout.idx_size;

                let drop_count = target_size - field_size - target_offset;
                for _ in 0..drop_count {
                    result.push(wasm::Instr::Drop);
                }

                let field_type = &self.module.types[expr.ty.0];
                let wasm_field_type = self.build_wasm_val_type(field_type);
                let temporary_locals = self.locals.get_temporary_locals(wasm_field_type);
                for i in (0..field_size).rev() {
                    result.push(wasm::Instr::LocalSet(temporary_locals[i as usize]));
                }

                for _ in 0..target_offset {
                    result.push(wasm::Instr::Drop);
                }
                for i in 0..field_size {
                    result.push(wasm::Instr::LocalGet(temporary_locals[i as usize]));
                }

                result
            }
            ExprKind::GetElementAddr(addr, field) => {
                let mut result = self.build_value_expr(addr);

                let Type::Ptr(element_type_id) = self.module.types[addr.ty.0] else {
                    unreachable!()
                };
                let struct_layout = self.struct_layouts.get(&element_type_id).unwrap();
                let mem_offset = struct_layout.mem_offset[*field];
                result.push(wasm::Instr::I32Const(mem_offset as i32));
                result.push(wasm::Instr::I32Add);

                result
            }
            ExprKind::GetIndex(arr, index) => {
                let mut result = self.build_value_expr(arr);

                // TODO: (optimization) optimize field layout calculation by using cache.
                let Type::ArrayPtr(element_type_id) = self.module.types[arr.ty.0] else {
                    unreachable!()
                };
                let element_type = &self.module.types[element_type_id.0];
                let element_layout = build_type_layout(&self.module, element_type);
                let element_size = element_layout.mem_size;

                result.push(wasm::Instr::I32Const(element_size as i32));
                result.extend(self.build_value_expr(index));

                let ty = &self.module.types[index.ty.0];
                if let Type::Int(int_type) = ty {
                    if int_type.size == BitSize::I64 {
                        result.push(wasm::Instr::I32WrapI64);
                    }
                }

                result.push(wasm::Instr::I32Mul);
                result.push(wasm::Instr::I32Add);

                result
            }
            ExprKind::Deref(addr) => {
                let mut result = self.build_value_expr(addr);

                let Type::Ptr(element_type_id) = self.module.types[addr.ty.0] else {
                    unreachable!()
                };
                let mut stack = vec![(0u32, element_type_id)];
                while let Some((offset, element_type_id)) = stack.pop() {
                    let ty = &self.module.types[element_type_id.0];
                    match ty {
                        Type::Struct(struct_type) => {
                            let struct_layout = self.struct_layouts.get(&element_type_id).unwrap();
                            for (i, off) in struct_layout.mem_offset.iter().enumerate().rev() {
                                stack.push((offset + off, struct_type.fields[i].ty));
                            }
                        }

                        Type::Func(..) => {
                            result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 }))
                        }
                        Type::Void => (),
                        Type::Bool => {
                            result.push(wasm::Instr::I32Load8U(wasm::MemArg { offset, align: 9 }))
                        }
                        Type::Int(IntType {
                            sign: true,
                            size: BitSize::I8,
                        }) => {
                            result.push(wasm::Instr::I32Load8S(wasm::MemArg { offset, align: 0 }))
                        }
                        Type::Int(IntType {
                            sign: false,
                            size: BitSize::I8,
                        }) => {
                            result.push(wasm::Instr::I32Load8U(wasm::MemArg { offset, align: 0 }))
                        }
                        Type::Int(IntType {
                            sign: true,
                            size: BitSize::I16,
                        }) => {
                            result.push(wasm::Instr::I32Load16S(wasm::MemArg { offset, align: 1 }))
                        }
                        Type::Int(IntType {
                            sign: false,
                            size: BitSize::I16,
                        }) => {
                            result.push(wasm::Instr::I32Load16U(wasm::MemArg { offset, align: 1 }))
                        }
                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I32 | BitSize::ISize,
                        }) => result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 })),
                        Type::Int(IntType {
                            sign: _,
                            size: BitSize::I64,
                        }) => result.push(wasm::Instr::I64Load(wasm::MemArg { offset, align: 3 })),
                        Type::Float(FloatType::F32) => {
                            result.push(wasm::Instr::F32Load(wasm::MemArg { offset, align: 2 }))
                        }
                        Type::Float(FloatType::F64) => {
                            result.push(wasm::Instr::F64Load(wasm::MemArg { offset, align: 3 }))
                        }
                        Type::Ptr(..) | Type::ArrayPtr(..) => {
                            result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 }))
                        }
                    }
                }

                result
            }
            ExprKind::Call(callee, arguments) => {
                let mut result = Vec::default();
                for arg in arguments.iter() {
                    result.extend(self.build_value_expr(arg));
                }
                match callee.kind {
                    ExprKind::Func(idx) => {
                        result.push(wasm::Instr::Call(*self.func_map.get(&idx.0).unwrap()))
                    }
                    _ => {
                        for arg in arguments.iter() {
                            result.extend(self.build_value_expr(arg));
                        }

                        let ty = &self.module.types[callee.ty.0];
                        let Type::Func(func_type) = ty else {
                            unreachable!("cannot call non-function expression")
                        };

                        let parameters: Vec<wasm::ValType> = func_type
                            .parameters
                            .iter()
                            .flat_map(|ty| self.build_wasm_val_type(&self.module.types[ty.0]))
                            .collect();
                        let returns: Vec<wasm::ValType> =
                            self.build_wasm_val_type(&self.module.types[func_type.return_type.0]);
                        let wasm_type_id = self.get_func_type(wasm::FuncType {
                            parameters,
                            returns,
                        });
                        result.push(wasm::Instr::CallIndirect(0, wasm_type_id));
                    }
                }
                result
            }
            ExprKind::Add(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Add, wasm::Instr::I32Extend8S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Add, wasm::Instr::I32Extend16S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Add],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Add],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Add],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Add],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Add],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Sub(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend8S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend16S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Sub],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Sub],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Sub],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Sub],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Sub],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Mul(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend8S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend16S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Mul],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Mul],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Mul],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Mul],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Mul],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Div(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend8S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend16S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32DivS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32DivS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64DivS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32DivU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32DivU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32DivU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32DivU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64DivU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Div],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Mod(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend8S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend16S],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32RemS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32RemS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64RemS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32RemU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32RemU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32RemU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32RemU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64RemU],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitOr(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Or],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Or],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Or],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Or],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Or],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitAnd(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32And],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32And],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32And],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32And],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64And],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitXor(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Xor],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Xor],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Xor],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Xor],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Xor],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::ShiftLeft(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32Shl],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32Shl],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32Shl],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32Shl],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Shl],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::ShiftRight(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32ShrS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32ShrS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32ShrS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32ShrS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64ShrS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8,
                    }) => vec![wasm::Instr::I32ShrU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I16,
                    }) => vec![wasm::Instr::I32ShrU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I32,
                    }) => vec![wasm::Instr::I32ShrU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::ISize,
                    }) => vec![wasm::Instr::I32ShrU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64ShrU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Div],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::And(a, b) => {
                let mut result = self.build_value_expr(a);

                result.push(wasm::Instr::I32Eqz);
                result.push(wasm::Instr::If(
                    wasm::BlockType::ValTy(wasm::ValType::Num(wasm::NumType::I32)),
                    vec![wasm::Instr::I32Const(0)],
                    self.build_value_expr(b),
                ));

                result
            }
            ExprKind::Or(a, b) => {
                let mut result = self.build_value_expr(a);

                result.push(wasm::Instr::I32Eqz);
                result.push(wasm::Instr::If(
                    wasm::BlockType::ValTy(wasm::ValType::Num(wasm::NumType::I32)),
                    self.build_value_expr(b),
                    vec![wasm::Instr::I32Const(1)],
                ));

                result
            }
            ExprKind::Eq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    })
                    | Type::Ptr(..)
                    | Type::ArrayPtr(..)
                    | Type::Func(..) => vec![wasm::Instr::I32Eq],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Eq],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::NEq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32Eq],
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Eq],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };
                result.extend(instrs);
                result.push(wasm::Instr::I32Eqz);

                result
            }
            ExprKind::Gt(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32GtS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64GtS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32GtU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64GtU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Gt],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Gt],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::GEq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32GeS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64GeS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32GeU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64GeU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Ge],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Ge],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Lt(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32LtS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64LtS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32LtU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64LtU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Lt],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Lt],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::LEq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = &self.module.types[a.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32LeS],
                    Type::Int(IntType {
                        sign: true,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64LeS],

                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    }) => vec![wasm::Instr::I32LeU],
                    Type::Int(IntType {
                        sign: false,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64LeU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Le],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Le],
                    _ => {
                        vec![
                            wasm::Instr::Drop,
                            wasm::Instr::Drop,
                            wasm::Instr::Unreachable,
                        ]
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Neg(value) => {
                let mut result = self.build_value_expr(value);
                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Mul],
                    Type::Int(IntType { sign: _, size: _ }) => {
                        vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Mul]
                    }
                    Type::Float(FloatType::F32) => {
                        vec![wasm::Instr::F32Const(-1.0), wasm::Instr::F32Mul]
                    }
                    Type::Float(FloatType::F64) => {
                        vec![wasm::Instr::F64Const(-1.0), wasm::Instr::F64Mul]
                    }
                    _ => vec![
                        wasm::Instr::Drop,
                        wasm::Instr::Drop,
                        wasm::Instr::Unreachable,
                    ],
                };
                result.extend(instrs);
                result
            }
            ExprKind::BitNot(value) => {
                let mut result = self.build_value_expr(value);
                let ty = &self.module.types[expr.ty.0];
                let instrs = match ty {
                    Type::Int(IntType {
                        sign: _,
                        size: BitSize::I64,
                    }) => vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Xor],
                    Type::Int(IntType { sign: _, size: _ }) => {
                        vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Xor]
                    }
                    _ => vec![
                        wasm::Instr::Drop,
                        wasm::Instr::Drop,
                        wasm::Instr::Unreachable,
                    ],
                };
                result.extend(instrs);
                result
            }
            ExprKind::Not(expr) => {
                let mut result = self.build_value_expr(expr);
                result.push(wasm::Instr::I32Eqz);
                result
            }
            ExprKind::Cast(value, type_id) => {
                let mut result = self.build_value_expr(value);

                let source_type = &self.module.types[value.ty.0];
                let target_type = &self.module.types[type_id.0];

                let instrs = match source_type {
                    Type::Ptr(..) | Type::ArrayPtr(..) => {
                        let target_int_type = match target_type {
                            Type::Int(target_int_type) => target_int_type,
                            Type::Ptr(..) | Type::ArrayPtr(..) => &IntType {
                                sign: false,
                                size: BitSize::ISize,
                            },
                            _ => unreachable!(),
                        };
                        match target_int_type {
                            IntType {
                                sign: _,
                                size: BitSize::I64,
                            } => vec![wasm::Instr::I64ExtendI32U],
                            _ => vec![],
                        }
                    }
                    Type::Int(source_int_type) => {
                        let target_int_type = match target_type {
                            Type::Int(target_int_type) => target_int_type,
                            Type::Ptr(..) | Type::ArrayPtr(..) => &IntType {
                                sign: false,
                                size: BitSize::ISize,
                            },
                            _ => unreachable!(),
                        };
                        match (source_int_type, target_int_type) {
                            (
                                IntType {
                                    sign: _,
                                    size: BitSize::I64,
                                },
                                IntType {
                                    sign: _,
                                    size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                },
                            ) => vec![wasm::Instr::I32WrapI64],
                            (
                                IntType {
                                    sign: true,
                                    size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                },
                                IntType {
                                    sign: _,
                                    size: BitSize::I64,
                                },
                            ) => vec![wasm::Instr::I64ExtendI32S],
                            (
                                IntType {
                                    sign: false,
                                    size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                },
                                IntType {
                                    sign: _,
                                    size: BitSize::I64,
                                },
                            ) => vec![wasm::Instr::I64ExtendI32U],
                            _ => vec![],
                        }
                    }
                    Type::Float(source_float_type) => {
                        let Type::Float(target_float_type) = target_type else {
                            unreachable!()
                        };
                        match (source_float_type, target_float_type) {
                            (FloatType::F32, FloatType::F64) => vec![wasm::Instr::F64PromoteF32],
                            (FloatType::F64, FloatType::F32) => vec![wasm::Instr::F32DemoteF64],
                            _ => vec![],
                        }
                    }
                    _ => unreachable!(),
                };
                result.extend(instrs);

                result
            }
        }
    }

    fn build_value_expr_from_var(&self, idx: VariableLoc, type_id: TypeId) -> Vec<wasm::Instr> {
        let ty = &self.module.types[type_id.0];
        match ty {
            Type::Struct(struct_type) => {
                let mut result = Vec::default();
                let struct_layout = self.struct_layouts.get(&type_id).unwrap();
                for (i, offset) in struct_layout.idx_offset.iter().enumerate() {
                    result.extend(self.build_value_expr_from_var(
                        idx.with_offset(*offset),
                        struct_type.fields[i].ty,
                    ));
                }
                result
            }
            _ => match idx {
                VariableLoc::Local(id) => vec![wasm::Instr::LocalGet(id)],
                VariableLoc::Global(id) => vec![wasm::Instr::GlobalGet(id)],
            },
        }
    }

    fn build_wasm_val_type(&self, ty: &Type) -> Vec<wasm::ValType> {
        match ty {
            Type::Struct(struct_type) => {
                let mut fields = vec![];
                for field in &struct_type.fields {
                    let ty = &self.module.types[field.ty.0];
                    fields.extend(self.build_wasm_val_type(ty));
                }
                fields
            }

            Type::Func(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],

            Type::Void => vec![],
            Type::Bool => vec![wasm::ValType::Num(wasm::NumType::I32)],

            Type::Int(IntType {
                sign: _,
                size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
            }) => vec![wasm::ValType::Num(wasm::NumType::I32)],
            Type::Int(IntType {
                sign: _,
                size: BitSize::I64,
            }) => vec![wasm::ValType::Num(wasm::NumType::I64)],

            Type::Float(FloatType::F32) => vec![wasm::ValType::Num(wasm::NumType::F32)],
            Type::Float(FloatType::F64) => vec![wasm::ValType::Num(wasm::NumType::F64)],

            Type::Ptr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
            Type::ArrayPtr(..) => vec![wasm::ValType::Num(wasm::NumType::I32)],
        }
    }

    fn build_zero_type(&self, ty: &Type) -> Vec<wasm::Instr> {
        match ty {
            Type::Struct(struct_type) => struct_type
                .fields
                .iter()
                .rev()
                .flat_map(|field| self.build_zero_type(&self.module.types[field.ty.0]))
                .collect(),
            Type::Func(..) => vec![wasm::Instr::I32Const(0)],
            Type::Void => vec![],
            Type::Bool => vec![wasm::Instr::I32Const(0)],
            Type::Int(IntType {
                sign: _,
                size: BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
            }) => vec![wasm::Instr::I32Const(0)],
            Type::Int(IntType {
                sign: _,
                size: BitSize::I64,
            }) => vec![wasm::Instr::I64Const(0)],
            Type::Float(FloatType::F32) => vec![wasm::Instr::F32Const(0f32)],
            Type::Float(FloatType::F64) => vec![wasm::Instr::F64Const(0f64)],
            Type::Ptr(..) | Type::ArrayPtr(..) => vec![wasm::Instr::I32Const(0)],
        }
    }

    fn build_zero_val_type(ty: &wasm::ValType) -> Vec<wasm::Instr> {
        match ty {
            wasm::ValType::Num(wasm::NumType::I32) => vec![wasm::Instr::I32Const(0)],
            wasm::ValType::Num(wasm::NumType::I64) => vec![wasm::Instr::I64Const(0)],
            wasm::ValType::Num(wasm::NumType::F32) => vec![wasm::Instr::F32Const(0f32)],
            wasm::ValType::Num(wasm::NumType::F64) => vec![wasm::Instr::F64Const(0f64)],
            wasm::ValType::Ref(wasm::RefType::FuncRef) => unreachable!(),
            wasm::ValType::Ref(wasm::RefType::ExternRef) => unreachable!(),
        }
    }

    fn get_func_type(&self, func_type: wasm::FuncType) -> wasm::TypeIdx {
        let mut func_type_cache = self.func_type_cache.borrow_mut();
        let next_id = func_type_cache.len() as wasm::TypeIdx;
        *func_type_cache.entry(func_type).or_insert(next_id)
    }

    fn mangle_object_id(&self, object_id: &ObjectId) -> String {
        match object_id {
            ObjectId::Concrete {
                package_id,
                name_id,
            } => format!(
                "{}.{}",
                self.module.symbols[package_id.0], self.module.symbols[name_id.0]
            ),
            ObjectId::GenericInst {
                package_id,
                name_id,
                typeargs_id,
            } => format!(
                "{}.{}[{}]",
                self.module.symbols[package_id.0],
                self.module.symbols[name_id.0],
                self.mangle_typeargs_id(typeargs_id)
            ),
        }
    }

    fn mangle_typeargs_id(&self, typeargs_id: &TypeArgsId) -> String {
        let typeargs = &self.module.typeargs[typeargs_id.0];
        let mut result = String::default();
        for type_id in typeargs.0.iter() {
            if !result.is_empty() {
                result.push(',');
            }
            result.push_str(&format!("{}", type_id.0));
        }
        result
    }
}

struct StructLayout {
    layout: Layout,
    idx_offset: Vec<u32>,
    mem_offset: Vec<u32>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Layout {
    alignment: u32,
    mem_size: u32,
    idx_size: u32,
}

impl Layout {
    #[allow(dead_code)]
    fn new(alignment: u32, mem_size: u32, idx_size: u32) -> Self {
        Self {
            alignment,
            mem_size,
            idx_size,
        }
    }
}

fn build_type_layout(module: &Package, ty: &Type) -> Layout {
    match ty {
        Type::Struct(struct_type) => build_struct_layout(module, struct_type).layout,
        Type::Func(..) => Layout {
            alignment: 4,
            mem_size: 4,
            idx_size: 1,
        },
        Type::Void => Layout {
            alignment: 1,
            mem_size: 0,
            idx_size: 0,
        },
        Type::Bool => Layout {
            alignment: 1,
            mem_size: 1,
            idx_size: 1,
        },
        Type::Int(IntType {
            sign: _,
            size: BitSize::I8,
        }) => Layout {
            alignment: 1,
            mem_size: 1,
            idx_size: 1,
        },
        Type::Int(IntType {
            sign: _,
            size: BitSize::I16,
        }) => Layout {
            alignment: 2,
            mem_size: 2,
            idx_size: 1,
        },
        Type::Int(IntType {
            sign: _,
            size: BitSize::I32,
        }) => Layout {
            alignment: 4,
            mem_size: 4,
            idx_size: 1,
        },
        Type::Int(IntType {
            sign: _,
            size: BitSize::I64,
        }) => Layout {
            alignment: 8,
            mem_size: 8,
            idx_size: 1,
        },
        Type::Int(IntType {
            sign: _,
            size: BitSize::ISize,
        }) => Layout {
            alignment: 4,
            mem_size: 4,
            idx_size: 1,
        },
        Type::Float(FloatType::F32) => Layout {
            alignment: 4,
            mem_size: 4,
            idx_size: 1,
        },
        Type::Float(FloatType::F64) => Layout {
            alignment: 8,
            mem_size: 8,
            idx_size: 1,
        },
        Type::Ptr(..) | Type::ArrayPtr(..) => Layout {
            alignment: 4,
            mem_size: 4,
            idx_size: 1,
        },
    }
}

fn build_struct_layout(module: &Package, struct_type: &StructType) -> StructLayout {
    let mut idx_offset = Vec::default();
    let mut curr_idx = 0;

    let mut mem_offset = Vec::default();
    let mut curr_mem = 0;
    let mut total_align = 1;

    for field in &struct_type.fields {
        idx_offset.push(curr_idx);
        let type_layout = build_type_layout(module, &module.types[field.ty.0]);
        curr_idx += type_layout.idx_size;

        let (size, align) = (type_layout.mem_size, type_layout.alignment);
        if align > total_align {
            total_align = align;
        }
        if curr_mem % align != 0 {
            curr_mem += align - (curr_mem % align);
        }
        mem_offset.push(curr_mem);

        curr_mem += size;
    }

    let mut mem_size = curr_mem;
    if mem_size % total_align != 0 {
        mem_size = mem_size + total_align - (mem_size % total_align)
    }

    StructLayout {
        layout: Layout {
            mem_size,
            idx_size: curr_idx,
            alignment: total_align,
        },
        idx_offset,
        mem_offset,
    }
}

#[derive(Default)]
struct LocalManager {
    internal: RefCell<LocalManagerInternal>,
}

impl LocalManager {
    fn take(&self) -> Vec<wasm::Local> {
        self.internal.borrow_mut().take()
    }

    fn set_params(&self, parameters: impl Iterator<Item = Vec<wasm::Local>>) {
        self.internal.borrow_mut().set_params(parameters);
    }

    fn set_locals(&self, locals: impl Iterator<Item = Vec<wasm::Local>>) {
        self.internal.borrow_mut().set_locals(locals);
    }

    fn get_local(&self, local_id: usize) -> u32 {
        self.internal.borrow().get_local(local_id)
    }

    fn get_temporary_locals(&self, spec: Vec<wasm::ValType>) -> Vec<u32> {
        self.internal.borrow_mut().get_temporary_locals(spec)
    }
}

#[derive(Default)]
struct LocalManagerInternal {
    locals: Vec<wasm::Local>,
    last_used_local: u32,
    local_maps: Vec<u32>,
    temps: HashMap<wasm::ValType, Vec<u32>>,
}

impl LocalManagerInternal {
    fn take(&mut self) -> Vec<wasm::Local> {
        let locals = std::mem::take(&mut self.locals);
        self.last_used_local = 0;
        self.local_maps = Vec::default();
        self.temps = HashMap::default();
        locals
    }

    fn set_params(&mut self, parameters: impl Iterator<Item = Vec<wasm::Local>>) {
        for locals in parameters {
            self.local_maps.push(self.last_used_local);
            self.last_used_local += locals.len() as u32;
        }
    }

    fn set_locals(&mut self, locals: impl Iterator<Item = Vec<wasm::Local>>) {
        for locals in locals {
            self.local_maps.push(self.last_used_local);
            self.last_used_local += locals.len() as u32;
            self.locals.extend(locals);
        }
    }

    fn get_local(&self, local_id: usize) -> u32 {
        self.local_maps[local_id]
    }

    fn get_temporary_locals(&mut self, spec: Vec<wasm::ValType>) -> Vec<u32> {
        let mut maps = IndexMap::<wasm::ValType, u32>::default();
        for ty in &spec {
            *maps.entry(*ty).or_default() += 1;
        }

        let mut type_maps = HashMap::<wasm::ValType, VecDeque<u32>>::default();
        for (val_type, required_count) in maps.into_iter() {
            let indexes = self.temps.entry(val_type).or_default();
            while indexes.len() < required_count as usize {
                indexes.push(self.last_used_local);
                self.last_used_local += 1;
                self.locals.push(wasm::Local {
                    name: "".to_string(),
                    ty: val_type,
                });
            }
            type_maps.insert(
                val_type,
                indexes
                    .iter()
                    .cloned()
                    .take(required_count as usize)
                    .collect(),
            );
        }

        let mut result = Vec::default();
        for ty in &spec {
            result.push(type_maps.get_mut(ty).unwrap().pop_front().unwrap());
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_layout {
        ($name: ident, $package: expr, $type:expr, $expected: expr) => {
            #[test]
            fn $name() {
                let package = $package;
                let layout = build_type_layout(&package, &$type);
                assert_eq!(layout, $expected)
            }
        };
        ($name: ident, $type:expr, $expected: expr) => {
            #[test]
            fn $name() {
                let package = Package::default();
                let layout = build_type_layout(&package, &$type);
                assert_eq!(layout, $expected)
            }
        };
    }

    test_layout! {test_layout_i8, Type::Int(IntType::i8()), Layout::new(1,1,1)}
    test_layout! {test_layout_i16, Type::Int(IntType::i16()), Layout::new(2,2,1)}
    test_layout! {test_layout_i32, Type::Int(IntType::i32()), Layout::new(4,4,1)}
    test_layout! {test_layout_i64, Type::Int(IntType::i64()), Layout::new(8,8,1)}
    test_layout! {test_layout_isize, Type::Int(IntType::isize()), Layout::new(4,4,1)}
    test_layout! {test_layout_u8, Type::Int(IntType::u8()), Layout::new(1,1,1)}
    test_layout! {test_layout_u16, Type::Int(IntType::u16()), Layout::new(2,2,1)}
    test_layout! {test_layout_u32, Type::Int(IntType::u32()), Layout::new(4,4,1)}
    test_layout! {test_layout_u64, Type::Int(IntType::u64()), Layout::new(8,8,1)}
    test_layout! {test_layout_usize, Type::Int(IntType::usize()), Layout::new(4,4,1)}

    test_layout! {
        test_layout_struct_1,
        Package{
            symbols: vec![],
            types: vec![Type::Int(IntType::i32()), Type::Bool],
            typeargs: vec![],
            globals: vec![],
            functions: vec![],
        },
        Type::Struct(StructType{
            id: ObjectId::Concrete{package_id: SymbolId(0), name_id: SymbolId(0)},
            fields: vec![StructField{name: SymbolId(0), ty: TypeId(0)}, StructField{name: SymbolId(1), ty: TypeId(1)}],
        }),
        Layout::new(4,8,2)
    }

    test_layout! {
        test_layout_struct_2,
        Package{
            symbols: vec![],
            types: vec![Type::Int(IntType::i32()), Type::Bool],
            typeargs: vec![],
            globals: vec![],
            functions: vec![],
        },
        Type::Struct(StructType{
            id: ObjectId::Concrete{package_id: SymbolId(0), name_id: SymbolId(0)},
            fields: vec![StructField{name: SymbolId(0), ty: TypeId(0)}, StructField{name: SymbolId(1), ty: TypeId(1)}],
        }),
        Layout::new(4,8,2)
    }
}
