use crate::data::Data;
use crate::layout::LayoutManager;
use crate::ty::{build_val_type, build_zero_type};
use crate::var::{GlobalMapper, LocalManager};
use indexmap::IndexMap;
use magelang_typecheck::{
    BitSize, DefId, Expr, ExprKind, FloatType, Func, Module, Statement, Type, TypeArgs,
};
use std::cell::RefCell;
use std::collections::HashMap;
use wasm_helper as wasm;

pub(crate) struct FuncManager<'a, 'ctx> {
    data: &'a Data,
    layouts: &'a LayoutManager<'ctx>,
    locals: &'a LocalManager,
    globals: &'a GlobalMapper<'ctx>,

    func_type_cache: RefCell<IndexMap<wasm::FuncType, wasm::TypeIdx>>,

    func_map: HashMap<FuncRef<'ctx>, wasm::FuncIdx>,
    next_func_id: wasm::FuncIdx,
    pub(crate) functions: Vec<wasm::Func>,
    pub(crate) exports: Vec<wasm::Export>,
    pub(crate) imports: Vec<wasm::Import>,
    pub(crate) func_elems: Vec<wasm::Elem>,
    pub(crate) starting_point: Option<wasm::FuncIdx>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct FuncRef<'ctx> {
    name: DefId<'ctx>,
    typeargs: Option<&'ctx TypeArgs<'ctx>>,
}

impl<'ctx> From<DefId<'ctx>> for FuncRef<'ctx> {
    fn from(name: DefId<'ctx>) -> Self {
        Self {
            name,
            typeargs: None,
        }
    }
}

impl<'ctx> From<&Func<'ctx>> for FuncRef<'ctx> {
    fn from(value: &Func<'ctx>) -> Self {
        Self {
            name: value.name,
            typeargs: value.typeargs,
        }
    }
}

pub(crate) enum Intrinsic {
    MemorySize,
    MemoryGrow,
    F32Floor,
    F64Floor,
    F32Ceil,
    F64Ceil,
    DataEnd,
    TableGet,
    TableSet,
    SizeOf,
    AlignOf,
    Unreachable,
}

impl<'a, 'ctx> FuncManager<'a, 'ctx> {
    pub(crate) fn build(
        data: &'a Data,
        layouts: &'a LayoutManager<'ctx>,
        locals: &'a LocalManager,
        globals: &'a GlobalMapper<'ctx>,
        module: &Module<'ctx>,
    ) -> Self {
        let mut s = FuncManager {
            data,
            layouts,
            locals,
            globals,

            func_type_cache: RefCell::default(),

            func_map: HashMap::default(),
            next_func_id: 0,
            functions: Vec::default(),
            exports: Vec::default(),
            imports: Vec::default(),
            func_elems: Vec::default(),
            starting_point: None,
        };

        struct MappedFunction<'a, 'ctx> {
            id: FuncRef<'ctx>,
            name: String,
            is_native: bool,
            type_id: wasm::TypeIdx,

            func: &'a Func<'ctx>,

            import: Option<(&'a str, &'a str)>,
            export: Option<&'a str>,
            intrinsic: Option<Intrinsic>,
        }

        let mut main_func: Option<FuncRef<'ctx>> = None;
        let mut mapped_functions = Vec::default();

        let funcs = module.packages.iter().flat_map(|pkg| &pkg.functions);
        for func in funcs.clone() {
            let func_type = func.ty.as_func().expect("func type is not a function");

            let mut parameters = Vec::default();
            for param in func_type.params {
                let val_types = build_val_type(param);
                parameters.extend(val_types);
            }

            let returns = build_val_type(func_type.return_type);
            let func_wasm_type_id = s.get_func_type(wasm::FuncType {
                parameters,
                returns,
            });

            let func_name = func.get_mangled_name();
            let is_native = matches!(func.statement, Statement::Native);

            let mut import = None;
            let mut export = None;
            let mut wasm_instr = None;

            // initialize function kinds, is it import, export, wasm intrinsic, user defined?
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
                            "memory.size" => Intrinsic::MemorySize,
                            "memory.grow" => Intrinsic::MemoryGrow,
                            "f32.floor" => Intrinsic::F32Floor,
                            "f64.floor" => Intrinsic::F64Floor,
                            "f32.ceil" => Intrinsic::F32Ceil,
                            "f64.ceil" => Intrinsic::F64Ceil,
                            "data.end" => Intrinsic::DataEnd,
                            "table.get" => Intrinsic::TableGet,
                            "table.set" => Intrinsic::TableSet,
                            "size_of" => Intrinsic::SizeOf,
                            "align_of" => Intrinsic::AlignOf,
                            "unreachable" => Intrinsic::Unreachable,
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
                        main_func = Some(func.into());
                    }
                    name => todo!("found unknown annotation {name}"),
                }
            }

            mapped_functions.push(MappedFunction {
                id: func.into(),
                name: func_name,
                is_native,
                func,
                type_id: func_wasm_type_id,
                import,
                export,
                intrinsic: wasm_instr,
            });
        }

        // it is important that the imported function appear first due to wasm spec
        mapped_functions.sort_by(|a, b| match (a.import, b.import) {
            (Some(..), Some(..)) | (None, None) => std::cmp::Ordering::Equal,
            (Some(..), None) => std::cmp::Ordering::Less,
            (None, Some(..)) => std::cmp::Ordering::Greater,
        });

        let mut func_with_bodies = Vec::default();

        // build native functions
        for mapped_func in mapped_functions.into_iter() {
            let MappedFunction {
                id,
                name,
                is_native,
                func,
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

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;
                    func_with_bodies.push((id, (s.functions.len(), func)));

                    s.functions.push(wasm::Func {
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

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;

                    s.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(match native_wasm {
                            Intrinsic::MemorySize => vec![wasm::Instr::MemorySize],
                            Intrinsic::MemoryGrow => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::MemoryGrow]
                            }
                            Intrinsic::F32Floor => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F32Floor]
                            }
                            Intrinsic::F64Floor => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F64Floor]
                            }
                            Intrinsic::F32Ceil => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F32Ceil]
                            }
                            Intrinsic::F64Ceil => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::F64Ceil]
                            }
                            Intrinsic::Unreachable => vec![wasm::Instr::Unreachable],
                            Intrinsic::DataEnd => {
                                vec![wasm::Instr::I32Const(s.data.data_end() as i32)]
                            }
                            Intrinsic::TableGet => {
                                vec![wasm::Instr::LocalGet(0), wasm::Instr::TableGet(1)]
                            }
                            Intrinsic::TableSet => {
                                vec![
                                    wasm::Instr::LocalGet(0),
                                    wasm::Instr::LocalGet(1),
                                    wasm::Instr::TableSet(1),
                                ]
                            }
                            Intrinsic::SizeOf => {
                                let Some(typeargs) = func.typeargs else {
                                    todo!("size_of requires a generic func");
                                };

                                if typeargs.len() != 1 {
                                    todo!(
                                        "size_of expects 1 generic parameter but found {}",
                                        typeargs.len()
                                    );
                                }
                                let ty = typeargs[0];
                                let layout = s.layouts.get_mem_layout(ty);
                                vec![wasm::Instr::I32Const(layout.size as i32)]
                            }
                            Intrinsic::AlignOf => {
                                let Some(typeargs) = func.typeargs else {
                                    todo!("align_of requires a generic func");
                                };

                                if typeargs.len() != 1 {
                                    todo!(
                                        "align_of expects 1 generic parameter but found {}",
                                        typeargs.len()
                                    );
                                }
                                let ty = typeargs[0];
                                let layout = s.layouts.get_mem_layout(ty);
                                vec![wasm::Instr::I32Const(layout.align as i32)]
                            }
                        }),
                    });
                }
                (None, Some(export_name), None) => {
                    if is_native {
                        todo!("cannot compile native function");
                    }

                    let func_id = s.next_func_id;
                    s.func_map.insert(id, func_id);
                    s.next_func_id += 1;
                    func_with_bodies.push((id, (s.functions.len(), func)));

                    s.functions.push(wasm::Func {
                        name,
                        ty: type_id,
                        locals: vec![],
                        body: wasm::Expr(vec![]),
                    });
                    s.exports.push(wasm::Export {
                        name: export_name.to_string(),
                        desc: wasm::ExportDesc::Func(func_id),
                    });
                }
                (None, Some(..), Some(..)) => todo!("cannot export wasm instrinsic"),
                (Some((import_module, import_name)), None, None) => {
                    if !is_native {
                        todo!("cannot import function to non native function");
                    }

                    s.func_map.insert(id, s.next_func_id);
                    s.next_func_id += 1;

                    s.imports.push(wasm::Import {
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

        // build user defined function body
        for (_, (wasm_idx, func)) in func_with_bodies {
            let func_type = func.ty.as_func().expect("not a func type");

            let mut locals = Vec::default();
            for ty in func_type.params {
                let val_types = build_val_type(ty);
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
            s.locals.set_params(locals.into_iter());

            let body = s.build_statement(0, 0, func.statement);

            s.functions[wasm_idx].body = wasm::Expr(body);
            s.functions[wasm_idx].locals = s.locals.take();
        }

        for (i, _) in funcs.enumerate() {
            s.func_elems.push(wasm::Elem {
                ty: wasm::RefType::FuncRef,
                init: vec![wasm::Expr(vec![wasm::Instr::RefFunc(i as u32)])],
                mode: wasm::ElemMode::Active {
                    table: 0,
                    offset: wasm::Expr(vec![wasm::Instr::I32Const(i as i32)]),
                },
            });
        }

        let mut init_func_body = Vec::default();
        let globals = module.packages.iter().flat_map(|pkg| &pkg.globals);
        for global in globals {
            // TODO: override this. if annotated with `@file_embed()`, load the content from file.
            let instrs = if let ExprKind::Zero = global.value.kind {
                let ty = global.ty;
                let Type::ArrayPtr(el) = ty else {
                    continue;
                };
                let is_bytes = !matches!(el, Type::Int(true, BitSize::I8));
                if !is_bytes {
                    continue;
                }

                let Some(filepath) = Data::get_embed_file_annotation(&global.annotations) else {
                    continue;
                };

                vec![wasm::Instr::I32Const(
                    s.data.get_file(filepath).unwrap() as i32
                )]
            } else {
                s.build_value_expr(global.value)
            };

            init_func_body.extend(instrs);
            let global_id = s.globals.get(global.name);
            let instrs = s.build_variable_set_instr(VariableLoc::Global(global_id), global.ty);
            init_func_body.extend(instrs);
        }
        if let Some(main_func_id) = main_func {
            let func_id = s.func_map.get(&main_func_id).unwrap();
            init_func_body.push(wasm::Instr::Call(*func_id));
        }

        s.starting_point = Some(s.next_func_id);
        s.next_func_id += 1;
        s.functions.push(wasm::Func {
            name: "__init".to_string(),
            ty: s.get_func_type(wasm::FuncType {
                parameters: vec![],
                returns: vec![],
            }),
            locals: s.locals.take(),
            body: wasm::Expr(init_func_body),
        });

        s
    }

    fn get_func_type(&self, func_type: wasm::FuncType) -> wasm::TypeIdx {
        let mut func_type_cache = self.func_type_cache.borrow_mut();
        let next_id = func_type_cache.len() as wasm::TypeIdx;
        *func_type_cache.entry(func_type).or_insert(next_id)
    }

    fn build_statement(
        &self,
        continue_label: u32,
        break_label: u32,
        stmt: &Statement<'ctx>,
    ) -> Vec<wasm::Instr> {
        match stmt {
            Statement::Native => unreachable!("native function should be handled specially"),
            Statement::NewLocal(id, value) => {
                let val_types = build_val_type(value.ty);
                let id = self.locals.new_local(*id, &val_types);

                let mut result = self.build_value_expr(value);
                let assign = self.build_variable_set_instr(VariableLoc::Local(id), value.ty);
                result.extend(assign);
                result
            }
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
                let types = build_val_type(expr.ty);
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

    fn build_variable_assign_stmt(
        &self,
        target: &Expr<'ctx>,
        value: &Expr<'ctx>,
    ) -> Vec<wasm::Instr> {
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
            ExprKind::Global(def_id) => {
                let mut result = self.build_value_expr(value);
                let assign = self.build_variable_set_instr(
                    VariableLoc::Global(self.globals.get(*def_id)),
                    value.ty,
                );
                result.extend(assign);
                result
            }
            ExprKind::GetElement(..) => {
                let mut result = self.build_value_expr(value);
                let Some(idx) = self.get_variable_loc(target) else {
                    // it's possible that the target is non-local. For example:
                    // let a: *SomeStruct = ...;
                    // a.*.b.c = ...
                    // in this case, this expression doesn't have to be assigned.
                    // only the value should be executed.
                    let types = build_val_type(value.ty);
                    for _ in types {
                        result.push(wasm::Instr::Drop);
                    }
                    return result;
                };
                result.extend(self.build_variable_set_instr(idx, target.ty));
                result
            }
            _ => unreachable!("expression is not local target"),
        }
    }

    fn get_variable_loc(&self, expr: &Expr<'ctx>) -> Option<VariableLoc> {
        match &expr.kind {
            ExprKind::Global(def_id) => Some(VariableLoc::Global(self.globals.get(*def_id))),
            ExprKind::Local(id) => Some(VariableLoc::Local(self.locals.get_local(*id))),
            ExprKind::GetElement(target, field) => {
                let mut var_idx = self.get_variable_loc(target)?;
                let struct_layout = &self.layouts.get_stack_layout(target.ty);
                var_idx.add_offset(struct_layout.offset[*field]);
                Some(var_idx)
            }
            _ => None,
        }
    }

    fn build_variable_set_instr(&self, idx: VariableLoc, ty: &'ctx Type<'ctx>) -> Vec<wasm::Instr> {
        match ty {
            Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),

            Type::Struct(..) | Type::Inst(..) => {
                let struct_layout = &self.layouts.get_stack_layout(ty);
                let body = match ty {
                    Type::Struct(struct_type) => {
                        struct_type.body.get().expect("missing struct body")
                    }
                    Type::Inst(inst_type) => inst_type.body.get().expect("missing struct body"),
                    _ => unreachable!(),
                };

                body.fields
                    .iter()
                    .enumerate()
                    .rev()
                    .flat_map(|(i, (_, field_ty))| {
                        let offset = struct_layout.offset[i];
                        self.build_variable_set_instr(idx.with_offset(offset), field_ty)
                    })
                    .collect()
            }

            Type::Void => vec![],
            Type::Opaque
            | Type::Func(..)
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
        pointer_expr: &Expr<'ctx>,
        value: &Expr<'ctx>,
    ) -> Vec<wasm::Instr> {
        match value.ty {
            Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),

            Type::Struct(..) | Type::Inst(..) => {
                let body = match value.ty {
                    Type::Struct(struct_type) => {
                        struct_type.body.get().expect("missing struct body")
                    }
                    Type::Inst(inst_type) => inst_type.body.get().expect("missing struct body"),
                    _ => unreachable!(),
                };

                // TODO: (optimization) improve this to handle special cases.
                let mut result = self.build_value_expr(value);

                let ptr_instr = self.build_value_expr(pointer_expr);
                result.extend(ptr_instr);

                let mut stack = vec![];
                let struct_layout = self.layouts.get_mem_layout(value.ty);
                for (i, (_, field_ty)) in body.fields.iter().enumerate() {
                    stack.push((struct_layout.clone(), i, field_ty));
                }

                while let Some((struct_layout, field_id, ty)) = stack.pop() {
                    match ty {
                        Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),

                        Type::Struct(struct_type) => {
                            let body = struct_type.body.get().expect("missing struct body");
                            let struct_layout = self.layouts.get_mem_layout(ty);
                            for (i, (_, field_ty)) in body.fields.iter().enumerate() {
                                stack.push((struct_layout.clone(), i, field_ty));
                            }
                        }
                        Type::Inst(inst_type) => {
                            let body = inst_type.body.get().expect("missing struct body");
                            let struct_layout = self.layouts.get_mem_layout(ty);
                            for (i, (_, field_ty)) in body.fields.iter().enumerate() {
                                stack.push((struct_layout.clone(), i, field_ty));
                            }
                        }

                        Type::Void => (),
                        Type::Opaque => unreachable!("opaque type can't be loaded from memory"),

                        Type::Int(_, BitSize::I8) | Type::Bool => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store8(wasm::MemArg { offset, align: 0 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(_, BitSize::I16) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store16(wasm::MemArg { offset, align: 1 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(_, BitSize::I32 | BitSize::ISize)
                        | Type::Ptr(..)
                        | Type::ArrayPtr(..)
                        | Type::Func(..) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I32),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.offset[field_id];
                            result.push(wasm::Instr::LocalSet(temp_addr));
                            result.push(wasm::Instr::LocalSet(temp_val));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                            result.push(wasm::Instr::LocalGet(temp_val));
                            result.push(wasm::Instr::I32Store(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temp_addr));
                        }
                        Type::Int(_, BitSize::I64) => {
                            let temps = self.locals.get_temporary_locals(vec![
                                wasm::ValType::Num(wasm::NumType::I64),
                                wasm::ValType::Num(wasm::NumType::I32),
                            ]);
                            let (temp_val, temp_addr) = (temps[0], temps[1]);
                            let offset = struct_layout.offset[field_id];
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
                            let offset = struct_layout.offset[field_id];
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
                            let offset = struct_layout.offset[field_id];
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
            Type::Opaque => unreachable!("opaque type can't be store in memory"),

            Type::Int(_, BitSize::I8) | Type::Bool => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::I32Store8(wasm::MemArg { offset, align: 0 }));
                result
            }
            Type::Int(_, BitSize::I16) => {
                let mut result = self.build_value_expr(pointer_expr);
                result.extend(self.build_value_expr(value));
                result.push(wasm::Instr::I32Store16(wasm::MemArg { offset, align: 1 }));
                result
            }
            Type::Int(_, BitSize::I32 | BitSize::ISize)
            | Type::Ptr(..)
            | Type::ArrayPtr(..)
            | Type::Func(..) => {
                let mut result = self.build_value_expr(pointer_expr);

                let value_instr = self.build_value_expr(value);
                result.extend(value_instr);

                result.push(wasm::Instr::I32Store(wasm::MemArg { offset, align: 2 }));
                result
            }
            Type::Int(_, BitSize::I64) => {
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

    fn build_value_expr(&self, expr: &Expr<'ctx>) -> Vec<wasm::Instr> {
        match &expr.kind {
            ExprKind::Invalid => unreachable!("found invalid expr"),
            ExprKind::ConstI8(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI16(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI32(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstI64(val) => vec![wasm::Instr::I64Const(*val as i64)],
            ExprKind::ConstIsize(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::ConstF32(val) => vec![wasm::Instr::F32Const(**val)],
            ExprKind::ConstF64(val) => vec![wasm::Instr::F64Const(**val)],
            ExprKind::ConstBool(val) => vec![wasm::Instr::I32Const(*val as i32)],
            ExprKind::Zero => build_zero_type(expr.ty),
            ExprKind::StructLit(_, values) => values
                .iter()
                .flat_map(|expr| self.build_value_expr(expr))
                .collect(),
            ExprKind::Bytes(bytes) => {
                vec![wasm::Instr::I32Const(
                    self.data.get_bytes(bytes).unwrap() as i32
                )]
            }
            ExprKind::Local(idx) => self.build_value_expr_from_var(
                VariableLoc::Local(self.locals.get_local(*idx)),
                expr.ty,
            ),
            ExprKind::Global(idx) => {
                self.build_value_expr_from_var(VariableLoc::Global(self.globals.get(*idx)), expr.ty)
            }
            ExprKind::Func(def_id) => {
                vec![wasm::Instr::I32Const(
                    *self.func_map.get(&(*def_id).into()).unwrap() as i32,
                )]
            }
            ExprKind::FuncInst(def_id, typeargs) => {
                vec![wasm::Instr::I32Const(
                    *self
                        .func_map
                        .get(&FuncRef {
                            name: *def_id,
                            typeargs: Some(*typeargs),
                        })
                        .unwrap() as i32,
                )]
            }
            ExprKind::GetElement(struct_expr, field) => {
                // TODO: (optimization) optimize this for various cases like selecting field in local/global var,
                // or dereference field from memory.

                let mut result = self.build_value_expr(struct_expr);
                let struct_layout = self.layouts.get_stack_layout(struct_expr.ty);
                let target_offset = struct_layout.offset[*field];
                // TODO: (optimization) optimize field layout calculation by using cache.
                let field_layout = self.layouts.get_stack_layout(expr.ty);
                let field_size = field_layout.size;
                let target_size = struct_layout.size;

                let drop_count = target_size - field_size - target_offset;
                for _ in 0..drop_count {
                    result.push(wasm::Instr::Drop);
                }

                let wasm_field_type = build_val_type(expr.ty);
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

                let Type::Ptr(element_type) = addr.ty else {
                    unreachable!()
                };
                let struct_layout = self.layouts.get_mem_layout(element_type);
                let mem_offset = struct_layout.offset[*field];
                result.push(wasm::Instr::I32Const(mem_offset as i32));
                result.push(wasm::Instr::I32Add);

                result
            }
            ExprKind::GetIndex(arr, index) => {
                let mut result = self.build_value_expr(arr);

                // TODO: (optimization) optimize field layout calculation by using cache.
                let Type::ArrayPtr(element_type) = arr.ty else {
                    unreachable!()
                };
                let element_layout = self.layouts.get_mem_layout(element_type);
                let element_size = element_layout.size;

                result.push(wasm::Instr::I32Const(element_size as i32));
                result.extend(self.build_value_expr(index));

                if let Type::Int(_, bit_size) = index.ty {
                    if *bit_size == BitSize::I64 {
                        result.push(wasm::Instr::I32WrapI64);
                    }
                }

                result.push(wasm::Instr::I32Mul);
                result.push(wasm::Instr::I32Add);

                result
            }
            ExprKind::Deref(addr) => {
                let mut result = self.build_value_expr(addr);

                let Type::Ptr(element_type) = addr.ty else {
                    unreachable!()
                };
                let mut stack = vec![(0u32, *element_type)];
                while let Some((offset, element_type)) = stack.pop() {
                    let ty = element_type;
                    match ty {
                        Type::Unknown | Type::TypeArg(..) => unreachable!("found invalid type"),

                        Type::Struct(struct_type) => {
                            let body = struct_type.body.get().expect("missing struct body");
                            let struct_layout = self.layouts.get_mem_layout(element_type);
                            for (i, off) in struct_layout.offset.iter().enumerate().rev() {
                                stack.push((offset + off, *body.fields.get_index(i).unwrap().1));
                            }
                        }
                        Type::Inst(inst_type) => {
                            let body = inst_type.body.get().expect("missing struct body");
                            let struct_layout = self.layouts.get_mem_layout(element_type);
                            for (i, off) in struct_layout.offset.iter().enumerate().rev() {
                                stack.push((offset + off, *body.fields.get_index(i).unwrap().1));
                            }
                        }

                        Type::Func(..) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Void => (),
                        Type::Opaque => unreachable!("opaque type can't be dereferenced"),
                        Type::Bool => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load8U(wasm::MemArg { offset, align: 0 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(true, BitSize::I8) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load8S(wasm::MemArg { offset, align: 0 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(false, BitSize::I8) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load8U(wasm::MemArg { offset, align: 0 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(true, BitSize::I16) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load16S(wasm::MemArg { offset, align: 1 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(false, BitSize::I16) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load16U(wasm::MemArg { offset, align: 1 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(_, BitSize::I32 | BitSize::ISize) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Int(_, BitSize::I64) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I64Load(wasm::MemArg { offset, align: 3 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Float(FloatType::F32) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::F32Load(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Float(FloatType::F64) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::F64Load(wasm::MemArg { offset, align: 3 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                        Type::Ptr(..) | Type::ArrayPtr(..) => {
                            let temps = self
                                .locals
                                .get_temporary_locals(vec![wasm::ValType::Num(wasm::NumType::I32)]);
                            result.push(wasm::Instr::LocalTee(temps[0]));
                            result.push(wasm::Instr::I32Load(wasm::MemArg { offset, align: 2 }));
                            result.push(wasm::Instr::LocalGet(temps[0]));
                        }
                    }
                }
                result.push(wasm::Instr::Drop);

                result
            }
            ExprKind::Call(callee, arguments) => {
                let mut result = Vec::default();
                for arg in arguments.iter() {
                    result.extend(self.build_value_expr(arg));
                }
                match callee.kind {
                    ExprKind::Func(def_id) => {
                        let func_ref = def_id.into();
                        result.push(wasm::Instr::Call(*self.func_map.get(&func_ref).unwrap()))
                    }
                    ExprKind::FuncInst(def_id, typeargs) => {
                        let func_ref = FuncRef {
                            name: def_id,
                            typeargs: Some(typeargs),
                        };
                        result.push(wasm::Instr::Call(*self.func_map.get(&func_ref).unwrap()))
                    }
                    _ => {
                        for arg in arguments.iter() {
                            result.extend(self.build_value_expr(arg));
                        }

                        let ty = callee.ty;
                        let Type::Func(func_type) = ty else {
                            unreachable!("cannot call non-function expression")
                        };

                        let parameters: Vec<wasm::ValType> = func_type
                            .params
                            .iter()
                            .flat_map(|ty| build_val_type(ty))
                            .collect();
                        let returns: Vec<wasm::ValType> = build_val_type(func_type.return_type);
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

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => {
                        vec![wasm::Instr::I32Add, wasm::Instr::I32Extend8S]
                    }
                    Type::Int(true, BitSize::I16) => {
                        vec![wasm::Instr::I32Add, wasm::Instr::I32Extend16S]
                    }
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32Add],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Add],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64Add],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32Add],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32Add],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32Add],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Add],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64Add],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Add],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Add],
                    _ => {
                        todo!("cannot perform add on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Sub(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => {
                        vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend8S]
                    }
                    Type::Int(true, BitSize::I16) => {
                        vec![wasm::Instr::I32Sub, wasm::Instr::I32Extend16S]
                    }
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32Sub],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Sub],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64Sub],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32Sub],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32Sub],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32Sub],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Sub],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64Sub],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Sub],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Sub],
                    _ => {
                        todo!("cannot perform sub on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Mul(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => {
                        vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend8S]
                    }
                    Type::Int(true, BitSize::I16) => {
                        vec![wasm::Instr::I32Mul, wasm::Instr::I32Extend16S]
                    }
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32Mul],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32Mul],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64Mul],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32Mul],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32Mul],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32Mul],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32Mul],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64Mul],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Mul],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Mul],
                    _ => {
                        todo!("cannot perform mul on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Div(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => {
                        vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend8S]
                    }
                    Type::Int(true, BitSize::I16) => {
                        vec![wasm::Instr::I32DivS, wasm::Instr::I32Extend16S]
                    }
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32DivS],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32DivS],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64DivS],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32DivU],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32DivU],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32DivU],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32DivU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64DivU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F32Div],
                    _ => {
                        todo!("cannot perform div on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Mod(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => {
                        vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend8S]
                    }
                    Type::Int(true, BitSize::I16) => {
                        vec![wasm::Instr::I32RemS, wasm::Instr::I32Extend16S]
                    }
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32RemS],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32RemS],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64RemS],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32RemU],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32RemU],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32RemU],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32RemU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64RemU],
                    _ => {
                        todo!("cannot perform mod on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitOr(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I8) => vec![wasm::Instr::I32Or],
                    Type::Int(_, BitSize::I16) => vec![wasm::Instr::I32Or],
                    Type::Int(_, BitSize::I32) => vec![wasm::Instr::I32Or],
                    Type::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Or],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Or],
                    _ => {
                        todo!("cannot perform or on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitAnd(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I8) => vec![wasm::Instr::I32And],
                    Type::Int(_, BitSize::I16) => vec![wasm::Instr::I32And],
                    Type::Int(_, BitSize::I32) => vec![wasm::Instr::I32And],
                    Type::Int(_, BitSize::ISize) => vec![wasm::Instr::I32And],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64And],
                    _ => {
                        todo!("cannot perform and on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::BitXor(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I8) => vec![wasm::Instr::I32Xor],
                    Type::Int(_, BitSize::I16) => vec![wasm::Instr::I32Xor],
                    Type::Int(_, BitSize::I32) => vec![wasm::Instr::I32Xor],
                    Type::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Xor],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Xor],
                    _ => {
                        todo!("cannot perform xor on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::ShiftLeft(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I8) => vec![wasm::Instr::I32Shl],
                    Type::Int(_, BitSize::I16) => vec![wasm::Instr::I32Shl],
                    Type::Int(_, BitSize::I32) => vec![wasm::Instr::I32Shl],
                    Type::Int(_, BitSize::ISize) => vec![wasm::Instr::I32Shl],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Shl],
                    _ => {
                        todo!("cannot perform shl on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::ShiftRight(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8) => vec![wasm::Instr::I32ShrS],
                    Type::Int(true, BitSize::I16) => vec![wasm::Instr::I32ShrS],
                    Type::Int(true, BitSize::I32) => vec![wasm::Instr::I32ShrS],
                    Type::Int(true, BitSize::ISize) => vec![wasm::Instr::I32ShrS],
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64ShrS],

                    Type::Int(false, BitSize::I8) => vec![wasm::Instr::I32ShrU],
                    Type::Int(false, BitSize::I16) => vec![wasm::Instr::I32ShrU],
                    Type::Int(false, BitSize::I32) => vec![wasm::Instr::I32ShrU],
                    Type::Int(false, BitSize::ISize) => vec![wasm::Instr::I32ShrU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64ShrU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Div],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Div],
                    _ => {
                        todo!("cannot perform div on {ty:?}");
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
                let a_instr = self.build_value_expr(a);
                let b_instr = self.build_value_expr(b);

                let ty = a.ty;
                let op_instr = match ty {
                    Type::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize)
                    | Type::Ptr(..)
                    | Type::ArrayPtr(..)
                    | Type::Func(..) => vec![wasm::Instr::I32Eq],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Eq],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],

                    Type::Bool => vec![wasm::Instr::I32Eq],

                    Type::Opaque => {
                        let mut result = Vec::default();
                        if !matches!(a.kind, ExprKind::Zero) {
                            result.extend(a_instr);
                            result.push(wasm::Instr::RefIsNull);
                        } else if !matches!(b.kind, ExprKind::Zero) {
                            result.extend(b_instr);
                            result.push(wasm::Instr::RefIsNull);
                        } else {
                            result.push(wasm::Instr::I32Const(1));
                        }
                        return result;
                    }

                    _ => {
                        todo!("cannot perform eq on {ty:?}");
                    }
                };

                let mut result = Vec::default();
                result.extend(a_instr);
                result.extend(b_instr);
                result.extend(op_instr);
                result
            }
            ExprKind::NEq(a, b) => {
                let a_instr = self.build_value_expr(a);
                let b_instr = self.build_value_expr(b);

                let ty = a.ty;
                let op_instr = match ty {
                    Type::Int(_, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                        vec![wasm::Instr::I32Eq]
                    }
                    Type::Ptr(..) | Type::ArrayPtr(..) | Type::Func(..) => vec![wasm::Instr::I32Eq],
                    Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64Eq],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Eq],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Eq],

                    Type::Bool => vec![wasm::Instr::I32Eq],

                    Type::Opaque => {
                        let mut result = Vec::default();
                        if !matches!(a.kind, ExprKind::Zero) {
                            result.extend(a_instr);
                            result.push(wasm::Instr::RefIsNull);
                        } else if !matches!(b.kind, ExprKind::Zero) {
                            result.extend(b_instr);
                            result.push(wasm::Instr::RefIsNull);
                        } else {
                            result.push(wasm::Instr::I32Const(1));
                        }
                        result.push(wasm::Instr::I32Eqz);
                        return result;
                    }

                    _ => {
                        todo!("cannot perform neq on {ty:?}");
                    }
                };
                let mut result = Vec::default();

                result.extend(a_instr);
                result.extend(b_instr);
                result.extend(op_instr);
                result.push(wasm::Instr::I32Eqz);
                result
            }
            ExprKind::Gt(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = a.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                        vec![wasm::Instr::I32GtS]
                    }
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64GtS],

                    Type::Int(
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => vec![wasm::Instr::I32GtU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64GtU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Gt],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Gt],
                    _ => {
                        todo!("cannot perform > on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::GEq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = a.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                        vec![wasm::Instr::I32GeS]
                    }
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64GeS],

                    Type::Int(
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => vec![wasm::Instr::I32GeU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64GeU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Ge],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Ge],
                    _ => {
                        todo!("cannot perform ge on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Lt(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = a.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                        vec![wasm::Instr::I32LtS]
                    }
                    Type::Int(true, BitSize::I64) => {
                        vec![wasm::Instr::I64LtS]
                    }

                    Type::Int(
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => vec![wasm::Instr::I32LtU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64LtU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Lt],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Lt],
                    _ => {
                        todo!("cannot perform lt on {ty:?}\n{a:?}\n{b:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::LEq(a, b) => {
                let mut result = self.build_value_expr(a);
                result.extend(self.build_value_expr(b));

                let ty = a.ty;
                let instrs = match ty {
                    Type::Int(true, BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize) => {
                        vec![wasm::Instr::I32LeS]
                    }
                    Type::Int(true, BitSize::I64) => vec![wasm::Instr::I64LeS],

                    Type::Int(
                        false,
                        BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                    ) => vec![wasm::Instr::I32LeU],
                    Type::Int(false, BitSize::I64) => vec![wasm::Instr::I64LeU],

                    Type::Float(FloatType::F32) => vec![wasm::Instr::F32Le],
                    Type::Float(FloatType::F64) => vec![wasm::Instr::F64Le],
                    _ => {
                        todo!("cannot perform le on {ty:?}");
                    }
                };

                result.extend(instrs);
                result
            }
            ExprKind::Neg(value) => {
                let mut result = self.build_value_expr(value);
                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I64) => {
                        vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Mul]
                    }
                    Type::Int(..) => {
                        vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Mul]
                    }
                    Type::Float(FloatType::F32) => {
                        vec![wasm::Instr::F32Const(-1.0), wasm::Instr::F32Mul]
                    }
                    Type::Float(FloatType::F64) => {
                        vec![wasm::Instr::F64Const(-1.0), wasm::Instr::F64Mul]
                    }
                    _ => {
                        todo!("cannot perform neg on {ty:?}");
                    }
                };
                result.extend(instrs);
                result
            }
            ExprKind::BitNot(value) => {
                let mut result = self.build_value_expr(value);
                let ty = expr.ty;
                let instrs = match ty {
                    Type::Int(_, BitSize::I64) => {
                        vec![wasm::Instr::I64Const(-1), wasm::Instr::I64Xor]
                    }
                    Type::Int(..) => {
                        vec![wasm::Instr::I32Const(-1), wasm::Instr::I32Xor]
                    }
                    _ => {
                        todo!("cannot perform bitnot on {ty:?}");
                    }
                };
                result.extend(instrs);
                result
            }
            ExprKind::Not(expr) => {
                let mut result = self.build_value_expr(expr);
                result.push(wasm::Instr::I32Eqz);
                result
            }
            ExprKind::Cast(value, ty) => {
                let mut result = self.build_value_expr(value);

                let source_type = value.ty;
                let target_type = ty;

                let instrs = match source_type {
                    Type::Ptr(..) | Type::ArrayPtr(..) => match target_type {
                        Type::Int(_, BitSize::I64) => vec![wasm::Instr::I64ExtendI32U],
                        Type::Int(..) | Type::Ptr(..) | Type::ArrayPtr(..) => vec![],
                        _ => unreachable!(),
                    },
                    Type::Int(source_sign, source_size) => {
                        match (source_sign, source_size, target_type) {
                            (
                                _,
                                BitSize::I64,
                                Type::Int(
                                    _,
                                    BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                )
                                | Type::Ptr(..)
                                | Type::ArrayPtr(..),
                            ) => vec![wasm::Instr::I32WrapI64],
                            (
                                true,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                Type::Int(_, BitSize::I64),
                            ) => vec![wasm::Instr::I64ExtendI32S],
                            (
                                false,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                                Type::Int(_, BitSize::I64),
                            ) => vec![wasm::Instr::I64ExtendI32U],
                            (_, _, Type::Int(..) | Type::ArrayPtr(..) | Type::Ptr(..)) => {
                                vec![]
                            }
                            (true, BitSize::I64, Type::Float(FloatType::F32)) => {
                                vec![wasm::Instr::F32ConvertI64S]
                            }
                            (false, BitSize::I64, Type::Float(FloatType::F32)) => {
                                vec![wasm::Instr::F32ConvertI64U]
                            }
                            (true, _, Type::Float(FloatType::F32)) => {
                                vec![wasm::Instr::F32ConvertI32S]
                            }
                            (false, _, Type::Float(FloatType::F32)) => {
                                vec![wasm::Instr::F32ConvertI32U]
                            }
                            (true, BitSize::I64, Type::Float(FloatType::F64)) => {
                                vec![wasm::Instr::F64ConvertI64S]
                            }
                            (false, BitSize::I64, Type::Float(FloatType::F64)) => {
                                vec![wasm::Instr::F64ConvertI64U]
                            }
                            (true, _, Type::Float(FloatType::F64)) => {
                                vec![wasm::Instr::F64ConvertI32S]
                            }
                            (false, _, Type::Float(FloatType::F64)) => {
                                vec![wasm::Instr::F64ConvertI32U]
                            }
                            _ => unreachable!("{source_sign:?} {source_size:?} {target_type:?}"),
                        }
                    }
                    Type::Float(source_float_type) => match target_type {
                        Type::Float(target_float_type) => {
                            match (source_float_type, target_float_type) {
                                (FloatType::F32, FloatType::F64) => {
                                    vec![wasm::Instr::F64PromoteF32]
                                }
                                (FloatType::F64, FloatType::F32) => {
                                    vec![wasm::Instr::F32DemoteF64]
                                }
                                _ => vec![],
                            }
                        }
                        Type::Int(sign, size) => match (source_float_type, sign, size) {
                            (
                                FloatType::F32,
                                true,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                            ) => {
                                vec![wasm::Instr::I32TruncF32S]
                            }
                            (FloatType::F32, true, BitSize::I64) => {
                                vec![wasm::Instr::I64TruncF32S]
                            }
                            (
                                FloatType::F32,
                                false,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                            ) => {
                                vec![wasm::Instr::I32TruncF32U]
                            }
                            (FloatType::F32, false, BitSize::I64) => {
                                vec![wasm::Instr::I64TruncF32U]
                            }
                            (
                                FloatType::F64,
                                true,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                            ) => {
                                vec![wasm::Instr::I32TruncF64S]
                            }
                            (FloatType::F64, true, BitSize::I64) => {
                                vec![wasm::Instr::I64TruncF64S]
                            }
                            (
                                FloatType::F64,
                                false,
                                BitSize::I8 | BitSize::I16 | BitSize::I32 | BitSize::ISize,
                            ) => {
                                vec![wasm::Instr::I32TruncF64U]
                            }
                            (FloatType::F64, false, BitSize::I64) => {
                                vec![wasm::Instr::I64TruncF64U]
                            }
                        },
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                result.extend(instrs);

                result
            }
        }
    }

    fn build_value_expr_from_var(
        &self,
        idx: VariableLoc,
        ty: &'ctx Type<'ctx>,
    ) -> Vec<wasm::Instr> {
        match ty {
            Type::Struct(struct_type) => {
                let body = struct_type.body.get().expect("missing struct body");
                let mut result = Vec::default();
                let struct_layout = self.layouts.get_stack_layout(ty);
                for (i, offset) in struct_layout.offset.iter().enumerate() {
                    result.extend(self.build_value_expr_from_var(
                        idx.with_offset(*offset),
                        body.fields.get_index(i).unwrap().1,
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

    pub(crate) fn get_wasm_table(&self) -> wasm::Table {
        wasm::Table {
            limits: wasm::Limits {
                min: self.func_elems.len() as u32,
                max: None,
            },
            ref_type: wasm::RefType::FuncRef,
        }
    }

    pub(crate) fn get_types(&self) -> Vec<wasm::FuncType> {
        let cache = self.func_type_cache.borrow();
        let mut types = cache.iter().map(|(ty, i)| (*i, ty)).collect::<Vec<_>>();
        types.sort_by_key(|item| item.0);
        types.into_iter().map(|(_, ty)| ty.clone()).collect()
    }
}

#[derive(Clone, Copy, Debug)]
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

trait Mangle {
    fn get_mangled_name(&self) -> String;
}

impl<'ctx> Mangle for DefId<'ctx> {
    fn get_mangled_name(&self) -> String {
        format!("{}", self)
    }
}

impl<'ctx> Mangle for Func<'ctx> {
    fn get_mangled_name(&self) -> String {
        let mut result = format!("{}", self.name);

        if let Some(typeargs) = self.typeargs {
            result.push('<');
            for (i, arg) in typeargs.iter().enumerate() {
                if i > 0 {
                    result.push(',');
                }
                result.push_str(&arg.get_mangled_name());
            }
            result.push('>');
        }

        result
    }
}

impl<'ctx> Mangle for Type<'ctx> {
    fn get_mangled_name(&self) -> String {
        match self {
            Type::Unknown => unreachable!("found unknown type arg"),
            Type::Struct(struct_type) => struct_type.def_id.get_mangled_name(),
            Type::Inst(inst_type) => {
                let mut result = format!("{}", inst_type.def_id);

                result.push('<');
                for (i, arg) in inst_type.type_args.iter().enumerate() {
                    if i > 0 {
                        result.push(',');
                    }
                    result.push_str(&arg.get_mangled_name());
                }
                result.push('>');

                result
            }
            Type::Func(..)
            | Type::Void
            | Type::Opaque
            | Type::Bool
            | Type::Int(..)
            | Type::Float(..)
            | Type::Ptr(..)
            | Type::ArrayPtr(..) => format!("{}", self),
            Type::TypeArg(..) => unreachable!("found typearg in type"),
        }
    }
}
