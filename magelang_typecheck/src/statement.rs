use crate::analyze::{Context, LocalObject, ValueObject};
use crate::errors;
use crate::expr::{Expr, ExprKind, get_binary_expr, get_expr_from_node};
use crate::interner::Interner;
use crate::scope::Scope;
use crate::ty::{Type, TypeArgs, TypeKind, TypeRepr, get_type_from_node};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{
    AssignStatementNode, BinaryOp, BlockStatementNode, DeferStatementNode, ForStatementNode, IfStatementNode, LetKind,
    LetStatementNode, Pos, ReturnStatementNode, StatementNode, WhileStatementNode,
};

pub(crate) type StatementInterner<'a> = Interner<'a, Statement<'a>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Statement<'a> {
    Native,
    NewLocal { id: usize, value: Expr<'a> },
    Block(&'a [Statement<'a>]),
    If(IfStatement<'a>),
    While(WhileStatement<'a>),
    For(ForStatement<'a>),
    Defer(Box<Statement<'a>>),
    Return(Option<Expr<'a>>),
    Expr(Expr<'a>),
    Assign { target: Expr<'a>, value: Expr<'a> },
    AssignOp { target: Expr<'a>, op: BinaryOp, value: &'a Expr<'a> },
    Continue,
    Break,
}

impl<'a> Statement<'a> {
    pub(crate) fn monomorphize<'b>(&self, ctx: &'b Context<'a, '_>, type_args: &'a TypeArgs<'a>) -> Statement<'a> {
        match self {
            Statement::Native => Statement::Native,
            Statement::Continue => Statement::Continue,
            Statement::Break => Statement::Break,
            Statement::NewLocal { id, value } => {
                let value = value.monomorphize(ctx, type_args);
                Statement::NewLocal { id: *id, value }
            }
            Statement::Block(statements) => {
                let mut result = BumpVec::with_capacity_in(statements.len(), ctx.arena);
                for stmt in statements.iter() {
                    result.push(stmt.monomorphize(ctx, type_args));
                }
                Statement::Block(result.into_bump_slice())
            }
            Statement::If(if_stmt) => {
                let cond = if_stmt.cond.monomorphize(ctx, type_args);
                let body = if_stmt.body.monomorphize(ctx, type_args);
                let else_stmt = if_stmt.else_stmt.as_ref().map(|stmt| stmt.monomorphize(ctx, type_args)).map(Box::new);
                Statement::If(IfStatement { cond, body: Box::new(body), else_stmt })
            }
            Statement::While(while_stmt) => {
                let cond = while_stmt.cond.monomorphize(ctx, type_args);
                let body = while_stmt.body.monomorphize(ctx, type_args);
                Statement::While(WhileStatement { cond, body: Box::new(body) })
            }
            Statement::For(for_stmt) => {
                let init = for_stmt.init.as_ref().map(|stmt| stmt.monomorphize(ctx, type_args)).map(Box::new);
                let cond = for_stmt.cond.as_ref().map(|cond| cond.monomorphize(ctx, type_args));
                let update = for_stmt.update.as_ref().map(|stmt| stmt.monomorphize(ctx, type_args)).map(Box::new);
                let body = for_stmt.body.monomorphize(ctx, type_args);
                Statement::For(ForStatement { init, cond, update, body: Box::new(body) })
            }
            Statement::Defer(stmt) => Statement::Defer(Box::new(stmt.monomorphize(ctx, type_args))),
            Statement::Return(value) => {
                let value = value.as_ref().map(|value| value.monomorphize(ctx, type_args));
                Statement::Return(value)
            }
            Statement::Expr(value) => {
                let value = value.monomorphize(ctx, type_args);
                Statement::Expr(value)
            }
            Statement::Assign { target, value } => Statement::Assign {
                target: target.monomorphize(ctx, type_args),
                value: value.monomorphize(ctx, type_args),
            },
            Statement::AssignOp { target, op, value } => Statement::AssignOp {
                target: target.monomorphize(ctx, type_args),
                op: *op,
                value: ctx.arena.alloc(value.monomorphize(ctx, type_args)),
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct IfStatement<'a> {
    pub cond: Expr<'a>,
    pub body: Box<Statement<'a>>,
    pub else_stmt: Option<Box<Statement<'a>>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct WhileStatement<'a> {
    pub cond: Expr<'a>,
    pub body: Box<Statement<'a>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ForStatement<'a> {
    pub init: Option<Box<Statement<'a>>>,
    pub cond: Option<Expr<'a>>,
    pub update: Option<Box<Statement<'a>>>,
    pub body: Box<Statement<'a>>,
}

pub(crate) struct StatementResult<'a> {
    pub(crate) statement: Statement<'a>,
    pub(crate) new_scope: Option<Scope<'a>>,
    pub(crate) is_returning: bool,
    pub(crate) last_unused_local: usize,
}

pub(crate) struct StatementContext<'a, 'b, 'syn> {
    ctx: &'b Context<'a, 'syn>,
    scope: &'b Scope<'a>,
    last_unused_local: usize,
    return_type: &'a Type<'a>,
    is_inside_loop: bool,
    is_inside_defer: bool,
}

impl<'a, 'b, 'syn> StatementContext<'a, 'b, 'syn> {
    pub(crate) fn new(
        ctx: &'b Context<'a, 'syn>,
        scope: &'b Scope<'a>,
        last_unused_local: usize,
        return_type: &'a Type<'a>,
    ) -> Self {
        Self { ctx, scope, last_unused_local, return_type, is_inside_loop: false, is_inside_defer: false }
    }
}

pub(crate) fn get_statement_from_node<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &StatementNode,
) -> StatementResult<'a> {
    match node {
        StatementNode::Let(node) => get_statement_from_let(ctx, node),
        StatementNode::Assign(node) => get_statement_from_assign(ctx, node),
        StatementNode::Block(node) => get_statement_from_block(ctx, node),
        StatementNode::If(node) => get_statement_from_if(ctx, node),
        StatementNode::While(node) => get_statement_from_while(ctx, node),
        StatementNode::For(node) => get_statement_from_for(ctx, node),
        StatementNode::Defer(node) => get_statement_from_defer(ctx, node),
        StatementNode::Continue(pos) => get_statement_from_continue(ctx, *pos),
        StatementNode::Break(pos) => get_statement_from_break(ctx, *pos),
        StatementNode::Return(node) => get_statement_from_return(ctx, node),
        StatementNode::Expr(node) => StatementResult {
            statement: Statement::Expr(get_expr_from_node(ctx.ctx, ctx.scope, None, node)),
            new_scope: None,
            is_returning: false,
            last_unused_local: ctx.last_unused_local,
        },
    }
}

pub(crate) fn get_statement_from_let<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &LetStatementNode,
) -> StatementResult<'a> {
    let expr = match &node.kind {
        LetKind::Invalid => {
            let type_id = ctx.ctx.define_type(Type { kind: TypeKind::Anonymous, repr: TypeRepr::Unknown });
            Expr { ty: type_id, kind: ExprKind::Zero, pos: node.pos, assignable: false }
        }
        LetKind::TypeOnly { ty } => {
            let type_id = get_type_from_node(ctx.ctx, ctx.scope, ty);
            Expr { ty: type_id, kind: ExprKind::Zero, pos: node.pos, assignable: false }
        }
        LetKind::TypeValue { ty, value } => {
            let ty = get_type_from_node(ctx.ctx, ctx.scope, ty);
            let mut value_expr = get_expr_from_node(ctx.ctx, ctx.scope, Some(ty), value);
            if !ty.is_assignable_with(value_expr.ty) {
                errors::report_type_mismatch(ctx.ctx.errors, value.pos(), ty, value_expr.ty);
                value_expr.kind = ExprKind::Invalid
            }
            value_expr
        }
        LetKind::ValueOnly { value } => get_expr_from_node(ctx.ctx, ctx.scope, None, value),
    };

    let name = ctx.ctx.define_symbol(&node.name.value);
    let mut new_table = IndexMap::default();
    let id = ctx.last_unused_local;
    new_table.insert(name, ValueObject::Local(LocalObject { id, ty: expr.ty, name }).into());
    let new_scope = ctx.scope.new_child(new_table);

    StatementResult {
        statement: Statement::NewLocal { id, value: expr },
        new_scope: Some(new_scope),
        is_returning: false,
        last_unused_local: ctx.last_unused_local + 1,
    }
}

pub(crate) fn get_statement_from_assign<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &AssignStatementNode,
) -> StatementResult<'a> {
    let receiver = get_expr_from_node(ctx.ctx, ctx.scope, None, &node.receiver);
    if !receiver.assignable {
        errors::report_expr_is_not_assignable(ctx.ctx.errors, node.receiver.pos());
    }

    let value = get_expr_from_node(ctx.ctx, ctx.scope, Some(receiver.ty), &node.value);
    let Some(op) = node.op else {
        if !receiver.ty.is_assignable_with(value.ty) {
            errors::report_type_mismatch(ctx.ctx.errors, node.value.pos(), receiver.ty, value.ty);
        }
        return StatementResult {
            statement: Statement::Assign { target: receiver, value },
            new_scope: None,
            is_returning: false,
            last_unused_local: ctx.last_unused_local,
        };
    };

    let lhs = get_expr_from_node(ctx.ctx, ctx.scope, None, &node.receiver);
    let value = ctx.ctx.arena.alloc(value);
    let operation = get_binary_expr(ctx.ctx, op, node.receiver.pos(), ctx.ctx.arena.alloc(lhs), value);
    if !receiver.ty.is_assignable_with(operation.ty) {
        errors::report_type_mismatch(ctx.ctx.errors, node.value.pos(), receiver.ty, operation.ty);
    }

    StatementResult {
        statement: Statement::AssignOp { target: receiver, op, value },
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_block<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &BlockStatementNode,
) -> StatementResult<'a> {
    let mut scope = ctx.scope.clone();
    let mut statements = BumpVec::with_capacity_in(node.statements.len(), ctx.ctx.arena);
    let mut last_unused_local = ctx.last_unused_local;
    let mut is_returning = false;
    let mut unreachable_error = false;
    for stmt in &node.statements {
        if is_returning && !unreachable_error {
            errors::report_unreachable_statement(ctx.ctx.errors, stmt.pos());
            unreachable_error = true;
        }

        let result = get_statement_from_node(
            &StatementContext {
                ctx: ctx.ctx,
                scope: &scope,
                last_unused_local,
                return_type: ctx.return_type,
                is_inside_loop: ctx.is_inside_loop,
                is_inside_defer: ctx.is_inside_defer,
            },
            stmt,
        );

        statements.push(result.statement);
        last_unused_local = result.last_unused_local;
        if result.is_returning {
            is_returning = true;
        }
        if let Some(new_scope) = result.new_scope {
            scope = new_scope;
        }
    }
    StatementResult {
        statement: Statement::Block(statements.into_bump_slice()),
        new_scope: None,
        is_returning,
        last_unused_local,
    }
}

pub(crate) fn get_statement_from_if<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &IfStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type { kind: TypeKind::Anonymous, repr: TypeRepr::Bool });
    let cond = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !cond.ty.is_bool() {
        errors::report_type_mismatch(ctx.ctx.errors, node.condition.pos(), TypeRepr::Bool, cond.ty);
    }

    let result = get_statement_from_block(ctx, &node.body);
    let body = result.statement;
    let body_is_returning = result.is_returning;
    let mut last_unused_local = result.last_unused_local;

    let else_result = node.else_node.as_ref().map(|else_body| {
        get_statement_from_node(
            &StatementContext {
                ctx: ctx.ctx,
                scope: ctx.scope,
                last_unused_local,
                return_type: ctx.return_type,
                is_inside_loop: ctx.is_inside_loop,
                is_inside_defer: ctx.is_inside_defer,
            },
            else_body,
        )
    });

    let mut else_stmt = None;
    let mut else_is_returning = false;
    if let Some(result) = else_result {
        else_stmt = Some(result.statement);
        last_unused_local = result.last_unused_local;
        else_is_returning = result.is_returning;
    }

    StatementResult {
        statement: Statement::If(IfStatement { cond, body: Box::new(body), else_stmt: else_stmt.map(Box::new) }),
        new_scope: None,
        is_returning: body_is_returning && else_is_returning,
        last_unused_local,
    }
}

pub(crate) fn get_statement_from_while<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &WhileStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type { kind: TypeKind::Anonymous, repr: TypeRepr::Bool });
    let condition = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !condition.ty.is_bool() {
        errors::report_type_mismatch(ctx.ctx.errors, node.condition.pos(), TypeRepr::Bool, condition.ty);
    }

    let body_stmt = get_statement_from_block(
        &StatementContext {
            ctx: ctx.ctx,
            scope: ctx.scope,
            last_unused_local: ctx.last_unused_local,
            return_type: ctx.return_type,
            is_inside_loop: true,
            is_inside_defer: ctx.is_inside_defer,
        },
        &node.body,
    );

    StatementResult {
        statement: Statement::While(WhileStatement { cond: condition, body: Box::new(body_stmt.statement) }),
        new_scope: None,
        is_returning: false,
        last_unused_local: body_stmt.last_unused_local,
    }
}

pub(crate) fn get_statement_from_for<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &ForStatementNode,
) -> StatementResult<'a> {
    let mut scope = ctx.scope.clone();
    let mut last_unused_local = ctx.last_unused_local;

    let init = if let Some(ref init_node) = node.init {
        let result = get_statement_from_node(
            &StatementContext {
                ctx: ctx.ctx,
                scope: &scope,
                last_unused_local,
                return_type: ctx.return_type,
                is_inside_loop: ctx.is_inside_loop,
                is_inside_defer: ctx.is_inside_defer,
            },
            init_node,
        );
        last_unused_local = result.last_unused_local;
        if let Some(new_scope) = result.new_scope {
            scope = new_scope;
        }
        Some(Box::new(result.statement))
    } else {
        None
    };

    // a for statement without condition loops forever.
    let cond = node.condition.as_ref().map(|cond_node| {
        let bool_type = ctx.ctx.define_type(Type { kind: TypeKind::Anonymous, repr: TypeRepr::Bool });
        let cond = get_expr_from_node(ctx.ctx, &scope, Some(bool_type), cond_node);
        if !cond.ty.is_bool() {
            errors::report_type_mismatch(ctx.ctx.errors, cond_node.pos(), TypeRepr::Bool, cond.ty);
        }
        cond
    });

    let body_stmt = get_statement_from_block(
        &StatementContext {
            ctx: ctx.ctx,
            scope: &scope,
            last_unused_local,
            return_type: ctx.return_type,
            is_inside_loop: true,
            is_inside_defer: ctx.is_inside_defer,
        },
        &node.body,
    );
    last_unused_local = body_stmt.last_unused_local;

    let update = if let Some(ref update_node) = node.update {
        let result = get_statement_from_node(
            &StatementContext {
                ctx: ctx.ctx,
                scope: &scope,
                last_unused_local,
                return_type: ctx.return_type,
                is_inside_loop: true,
                is_inside_defer: ctx.is_inside_defer,
            },
            update_node,
        );
        last_unused_local = result.last_unused_local;
        Some(Box::new(result.statement))
    } else {
        None
    };

    StatementResult {
        statement: Statement::For(ForStatement { init, cond, update, body: Box::new(body_stmt.statement) }),
        new_scope: None,
        is_returning: false,
        last_unused_local,
    }
}

pub(crate) fn get_statement_from_defer<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &DeferStatementNode,
) -> StatementResult<'a> {
    // break and continue is not allowed inside defer because it causes confusion.
    let result = get_statement_from_node(
        &StatementContext {
            ctx: ctx.ctx,
            scope: ctx.scope,
            last_unused_local: ctx.last_unused_local,
            return_type: ctx.return_type,
            is_inside_loop: false,
            is_inside_defer: true,
        },
        &node.body,
    );

    StatementResult {
        statement: Statement::Defer(Box::new(result.statement)),
        new_scope: None,
        is_returning: false,
        last_unused_local: result.last_unused_local,
    }
}

pub(crate) fn get_statement_from_continue<'a>(ctx: &StatementContext<'a, '_, '_>, pos: Pos) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        errors::report_operation_outside_loop(ctx.ctx.errors, pos, "continue");
    }
    StatementResult {
        statement: Statement::Continue,
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_break<'a>(ctx: &StatementContext<'a, '_, '_>, pos: Pos) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        errors::report_operation_outside_loop(ctx.ctx.errors, pos, "break");
    }
    StatementResult {
        statement: Statement::Break,
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_return<'a>(
    ctx: &StatementContext<'a, '_, '_>,
    node: &ReturnStatementNode,
) -> StatementResult<'a> {
    if ctx.is_inside_defer {
        errors::report_return_inside_defer(ctx.ctx.errors, node.pos);
    }

    let return_type = ctx.return_type;

    let value = node.value.as_ref().map(|expr| get_expr_from_node(ctx.ctx, ctx.scope, Some(return_type), expr));

    let value_ty = value
        .as_ref()
        .map(|expr| expr.ty)
        .unwrap_or(ctx.ctx.define_type(Type { kind: TypeKind::Anonymous, repr: TypeRepr::Void }));

    if !return_type.is_assignable_with(value_ty) {
        errors::report_type_mismatch(ctx.ctx.errors, node.pos, return_type, value_ty);
    };

    StatementResult {
        statement: Statement::Return(value),
        new_scope: None,
        is_returning: true,
        last_unused_local: ctx.last_unused_local,
    }
}
