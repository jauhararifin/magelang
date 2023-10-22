use crate::data::DataManager;
use crate::errors::CodegenError;
use crate::expr::ExprBuilder;
use crate::func::{FuncMapper, Function};
use crate::ty::{build_val_type, AlignNormalize, PrimitiveType, TypeManager};
use crate::var::{GlobalManager, LocalManager};
use magelang_syntax::ErrorReporter;
use magelang_typecheck::{Expr, ExprKind, Global, IfStatement, Statement, WhileStatement};
use wasm_helper as wasm;

pub(crate) fn build_function<'a, 'ctx, E: ErrorReporter>(
    errors: &'ctx E,
    data_manager: &'a DataManager<'ctx, E>,
    type_manager: &'a TypeManager<'ctx>,
    global_manager: &'a GlobalManager<'ctx>,
    func_manager: &'a FuncMapper<'ctx>,
    func: &'a Function<'ctx>,
) -> wasm::Func {
    let mut locals = Vec::default();
    for ty in func.ty.params {
        let val_types = build_val_type(ty);
        locals.push(
            val_types
                .iter()
                .map(|ty| wasm::Local {
                    name: "".to_string(),
                    ty: (*ty).into(),
                })
                .collect(),
        );
    }
    let local_manager = LocalManager::new(locals.into_iter());

    let builder = FuncBuilder {
        errors,
        locals: &local_manager,
        globals: global_manager,
        types: type_manager,
        func,
        exprs: ExprBuilder {
            errors,
            data: data_manager,
            types: type_manager,
            funcs: func_manager,
            locals: &local_manager,
            globals: global_manager,
        },
    };

    builder.build()
}

pub(crate) fn build_init_function<'a, 'ctx, E: ErrorReporter, G>(
    errors: &'ctx E,
    data_manager: &'a DataManager<'ctx, E>,
    type_manager: &'a TypeManager<'ctx>,
    global_manager: &'a GlobalManager<'ctx>,
    func_manager: &'a FuncMapper<'ctx>,
    globals: G,
    main_func: Option<wasm::FuncIdx>,
) -> wasm::Func
where
    G: Iterator<Item = &'ctx Global<'ctx>>,
{
    let local_manager = LocalManager::new(std::iter::empty());
    let exprs = ExprBuilder {
        errors,
        data: data_manager,
        types: type_manager,
        funcs: func_manager,
        locals: &local_manager,
        globals: global_manager,
    };

    let mut body = Vec::default();

    for global in globals {
        let instrs = if let ExprKind::Zero = global.value.kind {
            if !global.ty.is_byte_array() {
                continue;
            }
            let Some((_, path)) = data_manager.get_embed_file_annotation(&global.annotations)
            else {
                continue;
            };
            let ptr = data_manager.get_file(path).expect("missing path");
            vec![wasm::Instr::I32Const(ptr as i32)]
        } else {
            exprs.build(global.value).flatten()
        };

        body.extend(instrs);
        let global_id = global_manager.get(global.name);
        for i in 0..build_val_type(global.ty).len() {
            body.push(wasm::Instr::GlobalSet(global_id + i as u32))
        }
    }

    if let Some(main_func_id) = main_func {
        body.push(wasm::Instr::Call(main_func_id));
    }

    wasm::Func {
        name: "__init".to_string(),
        ty: type_manager.get_func_type(wasm::FuncType {
            parameters: vec![],
            returns: vec![],
        }),
        locals: local_manager.take(),
        body: wasm::Expr(body),
    }
}

struct FuncBuilder<'a, 'ctx, E> {
    errors: &'a E,
    locals: &'a LocalManager,
    globals: &'a GlobalManager<'ctx>,
    types: &'a TypeManager<'ctx>,
    func: &'a Function<'ctx>,
    exprs: ExprBuilder<'a, 'ctx, E>,
}

impl<'a, 'ctx, E: ErrorReporter> FuncBuilder<'a, 'ctx, E> {
    fn build(self) -> wasm::Func {
        let stmt = if let Some(body) = self.func.body {
            self.build_statement(0, 0, body)
        } else {
            vec![wasm::Instr::Unreachable]
        };

        wasm::Func {
            name: self.func.mangled_name.to_string(),
            ty: self.func.type_id,
            locals: self.locals.take(),
            body: wasm::Expr(stmt),
        }
    }

    fn build_statement(
        &self,
        continue_label: u32,
        break_label: u32,
        stmt: &'ctx Statement<'ctx>,
    ) -> Vec<wasm::Instr> {
        match stmt {
            Statement::Native => unreachable!("native function should be handled specially"),
            Statement::NewLocal(id, value) => self.build_new_local_stmt(*id, value),
            Statement::Block(statements) => {
                self.build_block_stmt(continue_label, break_label, statements)
            }
            Statement::If(if_stmt) => self.build_if_stmt(continue_label, break_label, if_stmt),
            Statement::While(while_stmt) => self.build_while_stmt(while_stmt),
            Statement::Return(value) => self.build_return_stmt(value),
            Statement::Expr(expr) => self.build_expr_stmt(expr),
            Statement::Assign(target, expr) => self.build_assign_stmt(target, expr),
            Statement::Continue => vec![wasm::Instr::Br(continue_label)],
            Statement::Break => vec![wasm::Instr::Br(break_label)],
        }
    }

    fn build_new_local_stmt(&self, id: usize, value: &Expr<'ctx>) -> Vec<wasm::Instr> {
        let val_types = build_val_type(value.ty);
        let id = self.locals.new_local(id, val_types.into_iter());
        let var = VariableLoc::Local(id);

        let exprs = self.exprs.build(value);

        let mut result = exprs.header;
        for (i, val) in exprs.components.into_iter().enumerate() {
            result.extend(val);
            result.push(var.get_set_instr(i));
        }

        result
    }

    fn build_block_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        statements: &'ctx [Statement<'ctx>],
    ) -> Vec<wasm::Instr> {
        let mut result = Vec::default();
        for stmt in statements.iter() {
            result.extend(self.build_statement(continue_label, break_label, stmt));
        }
        result
    }

    fn build_if_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        if_stmt: &'ctx IfStatement<'ctx>,
    ) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(&if_stmt.cond).flatten();
        let body = self.build_statement(continue_label + 1, break_label + 1, &if_stmt.body);

        let else_body = if let Some(ref else_body) = if_stmt.else_stmt {
            self.build_statement(continue_label + 1, break_label + 1, else_body)
        } else {
            vec![]
        };

        result.push(wasm::Instr::If(wasm::BlockType::None, body, else_body));
        result
    }

    fn build_while_stmt(&self, while_stmt: &'ctx WhileStatement<'ctx>) -> Vec<wasm::Instr> {
        let cond = self.exprs.build(&while_stmt.cond).flatten();
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

    fn build_return_stmt(&self, value: &Option<Expr<'ctx>>) -> Vec<wasm::Instr> {
        let mut result = vec![];
        if let Some(val) = value {
            result.extend(self.exprs.build(val).flatten());
        }
        result.push(wasm::Instr::Return);
        result
    }

    fn build_expr_stmt(&self, value: &Expr<'ctx>) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(value).flatten();
        let types = build_val_type(value.ty);
        for _ in types {
            result.push(wasm::Instr::Drop);
        }
        result
    }

    fn build_assign_stmt(
        &self,
        target: &'ctx Expr<'ctx>,
        expr: &'ctx Expr<'ctx>,
    ) -> Vec<wasm::Instr> {
        if let ExprKind::Deref(ptr) = &target.kind {
            self.build_mem_assign_stmt(ptr, expr)
        } else {
            let Some(variable) = self.get_variable_loc(target) else {
                // it's possible that the target is non-local. For example:
                // let a: *SomeStruct = ...;
                // a.*.b.c = ...
                // in this case, this expression doesn't have to be assigned.
                // only the value should be executed.
                let mut result = self.exprs.build(expr).flatten();
                let types = build_val_type(expr.ty);
                for _ in types {
                    result.push(wasm::Instr::Drop);
                }
                return result;
            };

            let exprs = self.exprs.build(expr);

            let mut result = exprs.header;
            for (i, value) in exprs.components.into_iter().enumerate() {
                result.extend(value);
                result.push(variable.get_set_instr(i));
            }
            result
        }
    }

    fn build_mem_assign_stmt(&self, ptr: &Expr<'ctx>, value: &'ctx Expr<'ctx>) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(ptr).flatten();

        let tmp = self.locals.get_temporary_locals(vec![PrimitiveType::U32]);
        let ptr_tmp = *tmp.first().unwrap();
        result.push(wasm::Instr::LocalSet(ptr_tmp));

        let val_types = build_val_type(value.ty);
        let Some(mem_layout) = self.types.get_mem_layout(value.ty) else {
            self.errors.storing_opaque(value.pos);
            return vec![wasm::Instr::Unreachable];
        };

        let exprs = self.exprs.build(value);
        result.extend(exprs.header);

        assert_eq!(val_types.len(), exprs.components.len());
        assert_eq!(val_types.len(), mem_layout.components.len());

        for (i, instrs) in exprs.components.into_iter().enumerate() {
            let component = mem_layout.components[i];
            let ty = val_types[i];

            result.push(wasm::Instr::LocalGet(ptr_tmp));
            result.extend(instrs);

            let mem_arg = wasm::MemArg {
                offset: component.offset,
                align: component.align.normalize(),
            };
            let store_instr = match ty {
                PrimitiveType::I8 | PrimitiveType::U8 => wasm::Instr::I32Store8,
                PrimitiveType::I16 | PrimitiveType::U16 => wasm::Instr::I32Store16,
                PrimitiveType::I32 | PrimitiveType::U32 => wasm::Instr::I32Store,
                PrimitiveType::I64 | PrimitiveType::U64 => wasm::Instr::I64Store,
                PrimitiveType::F32 => wasm::Instr::F32Store,
                PrimitiveType::F64 => wasm::Instr::F64Store,
                PrimitiveType::Extern => {
                    self.errors.storing_opaque(value.pos);
                    return vec![wasm::Instr::Unreachable];
                }
            };

            result.push(store_instr(mem_arg));
        }

        result
    }

    fn get_variable_loc(&self, expr: &'ctx Expr<'ctx>) -> Option<VariableLoc> {
        match &expr.kind {
            ExprKind::Global(def_id) => Some(VariableLoc::Global(self.globals.get(*def_id))),
            ExprKind::Local(id) => Some(VariableLoc::Local(self.locals.get_local(*id))),
            ExprKind::GetElement(target, field) => {
                let var = self.get_variable_loc(target)?;
                let struct_layout = self.types.get_stack_layout(target.ty);
                let var = var.with_offset(struct_layout.components[*field].offset);
                Some(var)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum VariableLoc {
    Global(u32),
    Local(u32),
}

impl VariableLoc {
    fn get_set_instr(&self, offset: usize) -> wasm::Instr {
        match self {
            Self::Global(id) => wasm::Instr::GlobalSet(id + offset as u32),
            Self::Local(id) => wasm::Instr::LocalSet(id + offset as u32),
        }
    }

    fn with_offset(&self, offset: u32) -> Self {
        match self {
            Self::Global(id) => Self::Global(id + offset),
            Self::Local(id) => Self::Local(id + offset),
        }
    }
}
