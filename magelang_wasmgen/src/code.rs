use crate::data::DataManager;
use crate::errors::CodegenError;
use crate::expr::ExprBuilder;
use crate::func::{FuncMapper, Function};
use crate::ty::{build_val_type, AlignNormalize, PrimitiveType, TypeManager};
use crate::var::{GlobalManager, LocalManager};
use magelang_syntax::{BinaryOp, ErrorReporter};
use magelang_typecheck::{
    Expr, ExprKind, ForStatement, IfStatement, Module, Statement, WhileStatement,
};
use std::cell::RefCell;
use std::collections::HashMap;
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
        pending_defers: RefCell::default(),
    };

    builder.build()
}

pub(crate) fn build_init_function<'a, 'ctx, E: ErrorReporter>(
    errors: &'ctx E,
    data_manager: &'a DataManager<'ctx, E>,
    type_manager: &'a TypeManager<'ctx>,
    global_manager: &'a GlobalManager<'ctx>,
    func_manager: &'a FuncMapper<'ctx>,
    module: &'ctx Module<'ctx>,
    main_func: Option<wasm::FuncIdx>,
) -> wasm::Func {
    let local_manager = LocalManager::new(std::iter::empty());
    let exprs = ExprBuilder {
        errors,
        data: data_manager,
        types: type_manager,
        funcs: func_manager,
        locals: &local_manager,
        globals: global_manager,
    };

    let globals = module
        .packages
        .iter()
        .flat_map(|pkg| &pkg.globals)
        .map(|global| (global.name, global))
        .collect::<HashMap<_, _>>();

    let mut body = Vec::default();
    for def_id in &module.global_init_order {
        let Some(global) = globals.get(def_id) else {
            continue;
        };

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
            exprs.build(&global.value)
        };

        body.extend(instrs);
        let global_id = global_manager.get(global.name);
        for i in (0..build_val_type(global.ty).len()).rev() {
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
    pending_defers: RefCell<Vec<&'ctx Statement<'ctx>>>,
}

impl<'a, 'ctx, E: ErrorReporter> FuncBuilder<'a, 'ctx, E> {
    fn build(self) -> wasm::Func {
        let stmt = if let Some(body) = self.func.body {
            let mut stmt = self.build_statement(0, 0, 0, body);

            // The typechecker guarantees that every possible execution path returned the correct
            // values. However, wasm validation still check if the function's main block have the
            // correct stack values at the end of function. Consider the case below:
            //
            //   if cond { return a; } else { return b; }
            //
            // in above case, both block returned, but the main block contains empty values. Even
            // though it's impossible for us to returned at main block, wasm validation require
            // us to have main block contains the correct returned values.
            if !self.func.ty.return_type.is_void() {
                stmt.push(wasm::Instr::Unreachable);
            }
            stmt
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
        loop_defer_mark: usize,
        stmt: &'ctx Statement<'ctx>,
    ) -> Vec<wasm::Instr> {
        match stmt {
            Statement::Native => unreachable!("native function should be handled specially"),
            Statement::NewLocal { id, value } => self.build_new_local_stmt(*id, value),
            Statement::Block(statements) => {
                self.build_block_stmt(continue_label, break_label, loop_defer_mark, statements)
            }
            Statement::If(if_stmt) => {
                self.build_if_stmt(continue_label, break_label, loop_defer_mark, if_stmt)
            }
            Statement::While(while_stmt) => self.build_while_stmt(while_stmt),
            Statement::For(for_stmt) => {
                self.build_for_stmt(continue_label, break_label, loop_defer_mark, for_stmt)
            }
            Statement::Return(value) => {
                self.build_return_stmt(continue_label, break_label, loop_defer_mark, value)
            }
            Statement::Expr(expr) => self.build_expr_stmt(expr),
            Statement::Assign { target, value } => self.build_assign_stmt(target, value),
            Statement::AssignOp { target, op, value } => {
                self.build_assign_op_stmt(target, *op, value)
            }
            Statement::Defer(stmt) => {
                self.pending_defers.borrow_mut().push(stmt);
                vec![]
            }
            Statement::Continue => {
                self.build_jump_stmt(continue_label, break_label, loop_defer_mark, continue_label)
            }
            Statement::Break => {
                self.build_jump_stmt(continue_label, break_label, loop_defer_mark, break_label)
            }
        }
    }

    fn build_new_local_stmt(&self, id: usize, value: &Expr<'ctx>) -> Vec<wasm::Instr> {
        let val_types = build_val_type(value.ty);
        let size = val_types.len();
        let id = self.locals.new_local(id, val_types.into_iter());
        let var = VariableLoc::Local(id);
        let mut result = self.exprs.build(value);
        for i in (0..size).rev() {
            result.push(var.get_set_instr(i));
        }
        result
    }

    fn build_block_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        statements: &'ctx [Statement<'ctx>],
    ) -> Vec<wasm::Instr> {
        let defer_mark = self.pending_defers.borrow().len();
        let mut result = Vec::default();
        for stmt in statements.iter() {
            result.extend(self.build_statement(continue_label, break_label, loop_defer_mark, stmt));
        }
        result.extend(self.build_deferred_stmts(
            continue_label,
            break_label,
            loop_defer_mark,
            defer_mark,
        ));
        result
    }

    fn build_deferred_stmts(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        until: usize,
    ) -> Vec<wasm::Instr> {
        let mut result = Vec::default();
        while self.pending_defers.borrow().len() > until {
            let stmt = self.pending_defers.borrow_mut().pop().unwrap();
            result.extend(self.build_statement(continue_label, break_label, loop_defer_mark, stmt));
        }
        result
    }

    fn build_if_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        if_stmt: &'ctx IfStatement<'ctx>,
    ) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(&if_stmt.cond);
        let body = self.build_statement(
            continue_label + 1,
            break_label + 1,
            loop_defer_mark,
            &if_stmt.body,
        );

        let else_body = if let Some(ref else_body) = if_stmt.else_stmt {
            self.build_statement(
                continue_label + 1,
                break_label + 1,
                loop_defer_mark,
                else_body,
            )
        } else {
            vec![]
        };

        result.push(wasm::Instr::If(wasm::BlockType::None, body, else_body));
        result
    }

    fn build_while_stmt(&self, while_stmt: &'ctx WhileStatement<'ctx>) -> Vec<wasm::Instr> {
        let cond = self.exprs.build(&while_stmt.cond);
        let loop_defer_mark = self.pending_defers.borrow().len();
        let body = self.build_statement(0, 1, loop_defer_mark, &while_stmt.body);

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

    fn build_for_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        for_stmt: &'ctx ForStatement<'ctx>,
    ) -> Vec<wasm::Instr> {
        let mut result = if let Some(ref init) = for_stmt.init {
            self.build_statement(continue_label, break_label, loop_defer_mark, init)
        } else {
            Vec::default()
        };
        let loop_defer_mark = self.pending_defers.borrow().len();

        let mut inner_block = Vec::default();
        if let Some(ref cond) = for_stmt.cond {
            inner_block.extend(self.exprs.build(cond));
            inner_block.push(wasm::Instr::I32Eqz);
            inner_block.push(wasm::Instr::BrIf(1));
        }

        if let Some(ref update) = for_stmt.update {
            inner_block.push(wasm::Instr::Block(
                wasm::BlockType::None,
                self.build_statement(0, 2, loop_defer_mark, &for_stmt.body),
            ));
            inner_block.extend(self.build_statement(0, 1, loop_defer_mark, update));
        } else {
            inner_block.extend(self.build_statement(0, 1, loop_defer_mark, &for_stmt.body));
        }
        inner_block.push(wasm::Instr::Br(0));

        result.push(wasm::Instr::Block(
            wasm::BlockType::None,
            vec![wasm::Instr::Loop(wasm::BlockType::None, inner_block)],
        ));
        result
    }

    fn build_return_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        value: &Option<Expr<'ctx>>,
    ) -> Vec<wasm::Instr> {
        let mut result = vec![];
        if let Some(val) = value {
            result.extend(self.exprs.build(val));
        }
        let pending = self.pending_defers.borrow().clone();
        result.extend(self.build_deferred_stmts(continue_label, break_label, loop_defer_mark, 0));
        *self.pending_defers.borrow_mut() = pending;
        result.push(wasm::Instr::Return);
        result
    }

    fn build_expr_stmt(&self, value: &Expr<'ctx>) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(value);
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
                unreachable!(
                    "assignment target is not a storage location: {:?}",
                    target.kind
                );
            };

            let types = build_val_type(expr.ty);
            let mut result = self.exprs.build(expr);
            for i in (0..types.len()).rev() {
                result.push(variable.get_set_instr(i));
            }

            result
        }
    }

    fn build_mem_assign_stmt(&self, ptr: &Expr<'ctx>, value: &'ctx Expr<'ctx>) -> Vec<wasm::Instr> {
        let mut result = self.exprs.build(ptr);

        let tmp = self.locals.get_temporary_locals(vec![PrimitiveType::U32]);
        let ptr_tmp = *tmp.first().unwrap();
        result.push(wasm::Instr::LocalSet(ptr_tmp));

        let val_types = build_val_type(value.ty);
        let Some(mem_layout) = self.types.get_mem_layout(value.ty) else {
            self.errors.storing_opaque(value.pos);
            return vec![wasm::Instr::Unreachable];
        };

        let exprs = self.exprs.build(value);
        result.extend(exprs);

        assert_eq!(val_types.len(), mem_layout.components.len());

        for (i, component) in mem_layout.components.iter().enumerate().rev() {
            let ty = val_types[i];

            let mem_arg = wasm::MemArg {
                offset: component.offset,
                align: component.align.normalize(),
            };
            let Some(store_instr) = ty.store_instr() else {
                self.errors.storing_opaque(value.pos);
                return vec![wasm::Instr::Unreachable];
            };

            let tmp = self.locals.get_temporary_locals(vec![ty]);
            let value_temp_id = *tmp.first().unwrap();
            result.push(wasm::Instr::LocalSet(value_temp_id));
            result.push(wasm::Instr::LocalGet(ptr_tmp));
            result.push(wasm::Instr::LocalGet(value_temp_id));
            result.push(store_instr(mem_arg));
        }

        result
    }

    fn build_jump_stmt(
        &self,
        continue_label: u32,
        break_label: u32,
        loop_defer_mark: usize,
        target_label: u32,
    ) -> Vec<wasm::Instr> {
        let pending = self.pending_defers.borrow().clone();
        let mut result = self.build_deferred_stmts(
            continue_label,
            break_label,
            loop_defer_mark,
            loop_defer_mark,
        );
        *self.pending_defers.borrow_mut() = pending;
        result.push(wasm::Instr::Br(target_label));
        result
    }

    fn build_assign_op_stmt(
        &self,
        target: &'ctx Expr<'ctx>,
        op: BinaryOp,
        value: &'ctx Expr<'ctx>,
    ) -> Vec<wasm::Instr> {
        let val_types = build_val_type(target.ty);
        assert_eq!(
            val_types.len(),
            1,
            "assignment operators only work on primitive types, but {:?} has {} components",
            target.ty,
            val_types.len()
        );

        let ExprKind::Deref(ptr) = &target.kind else {
            let Some(variable) = self.get_variable_loc(target) else {
                unreachable!(
                    "assignment target is not a storage location: {:?}",
                    target.kind
                );
            };
            let current = vec![variable.get_get_instr(0)];
            let mut result = self.exprs.build_binary_op(op, target.ty, current, value);
            result.push(variable.get_set_instr(0));
            return result;
        };

        let val_type = val_types[0];
        let (Some(mem_layout), Some(load_instr), Some(store_instr)) = (
            self.types.get_mem_layout(target.ty),
            val_type.load_instr(),
            val_type.store_instr(),
        ) else {
            self.errors.storing_opaque(target.pos);
            return vec![wasm::Instr::Unreachable];
        };
        let component = &mem_layout.components[0];

        let mut result = self.exprs.build(ptr);
        let ptr_tmp = self.locals.get_temporary_locals(vec![PrimitiveType::U32]);
        let ptr_tmp_id = *ptr_tmp.first().unwrap();
        result.push(wasm::Instr::LocalSet(ptr_tmp_id));

        let current = vec![
            wasm::Instr::LocalGet(ptr_tmp_id),
            load_instr(wasm::MemArg {
                offset: component.offset,
                align: component.align.normalize(),
            }),
        ];
        result.extend(self.exprs.build_binary_op(op, target.ty, current, value));

        let value_tmp = self.locals.get_temporary_locals(vec![val_type]);
        let value_tmp_id = *value_tmp.first().unwrap();
        result.push(wasm::Instr::LocalSet(value_tmp_id));
        result.push(wasm::Instr::LocalGet(ptr_tmp_id));
        result.push(wasm::Instr::LocalGet(value_tmp_id));
        result.push(store_instr(wasm::MemArg {
            offset: component.offset,
            align: component.align.normalize(),
        }));
        result
    }

    fn get_variable_loc(&self, expr: &'ctx Expr<'ctx>) -> Option<VariableLoc> {
        match &expr.kind {
            ExprKind::Global(def_id) => Some(VariableLoc::Global(self.globals.get(*def_id))),
            ExprKind::Local(id) => Some(VariableLoc::Local(self.locals.get_local(*id))),
            ExprKind::GetElement(target, field) => {
                let var = self.get_variable_loc(target)?;
                let struct_layout = self.types.get_stack_layout(target.ty);
                let field_idx = struct_layout.field_index[*field];
                let var = var.with_offset(struct_layout.components[field_idx].offset);
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
    fn get_get_instr(&self, offset: usize) -> wasm::Instr {
        match self {
            Self::Global(id) => wasm::Instr::GlobalGet(id + offset as u32),
            Self::Local(id) => wasm::Instr::LocalGet(id + offset as u32),
        }
    }

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
