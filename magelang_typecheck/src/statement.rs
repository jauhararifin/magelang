use crate::analyze::{Context, LocalObject, Scopes, ValueObject};
use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::interner::Interner;
use crate::ty::{get_type_from_node, Type, TypeArgs, TypeKind, TypeRepr};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{
    AssignStatementNode, BlockStatementNode, ErrorReporter, IfStatementNode, LetKind,
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
    Return(Option<Expr<'a>>),
    Expr(Expr<'a>),
    Assign(Expr<'a>, Expr<'a>),
    Continue,
    Break,
}

impl<'a> Statement<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> Statement<'a> {
        match self {
            Statement::Native => Statement::Native,
            Statement::Continue => Statement::Continue,
            Statement::Break => Statement::Break,
            Statement::NewLocal { id, value } => {
                let value = value.monomorphize(ctx, type_args);
                Statement::NewLocal{id: *id, value}
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
                let else_stmt = if_stmt
                    .else_stmt
                    .as_ref()
                    .map(|stmt| stmt.monomorphize(ctx, type_args))
                    .map(Box::new);
                Statement::If(IfStatement {
                    cond,
                    body: Box::new(body),
                    else_stmt,
                })
            }
            Statement::While(while_stmt) => {
                let cond = while_stmt.cond.monomorphize(ctx, type_args);
                let body = while_stmt.body.monomorphize(ctx, type_args);
                Statement::While(WhileStatement {
                    cond,
                    body: Box::new(body),
                })
            }
            Statement::Return(value) => {
                let value = value
                    .as_ref()
                    .map(|value| value.monomorphize(ctx, type_args));
                Statement::Return(value)
            }
            Statement::Expr(value) => {
                let value = value.monomorphize(ctx, type_args);
                Statement::Expr(value)
            }
            Statement::Assign(target, value) => {
                let target = target.monomorphize(ctx, type_args);
                let value = value.monomorphize(ctx, type_args);
                Statement::Assign(target, value)
            }
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

pub(crate) struct StatementResult<'a> {
    pub(crate) statement: Statement<'a>,
    pub(crate) new_scope: Option<Scopes<'a>>,
    pub(crate) is_returning: bool,
    pub(crate) last_unused_local: usize,
}

pub(crate) struct StatementContext<'a, 'b, 'syn, E: ErrorReporter> {
    ctx: &'b Context<'a, 'syn, E>,
    scope: &'b Scopes<'a>,
    last_unused_local: usize,
    return_type: &'a Type<'a>,
    is_inside_loop: bool,
}

impl<'a, 'b, 'syn, E: ErrorReporter> StatementContext<'a, 'b, 'syn, E> {
    pub(crate) fn new(
        ctx: &'b Context<'a, 'syn, E>,
        scope: &'b Scopes<'a>,
        last_unused_local: usize,
        return_type: &'a Type<'a>,
    ) -> Self {
        Self {
            ctx,
            scope,
            last_unused_local,
            return_type,
            is_inside_loop: false,
        }
    }
}

pub(crate) fn get_statement_from_node<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &StatementNode,
) -> StatementResult<'a> {
    match node {
        StatementNode::Let(node) => get_statement_from_let(ctx, node),
        StatementNode::Assign(node) => get_statement_from_assign(ctx, node),
        StatementNode::Block(node) => get_statement_from_block(ctx, node),
        StatementNode::If(node) => get_statement_from_if(ctx, node),
        StatementNode::While(node) => get_statement_from_while(ctx, node),
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

pub(crate) fn get_statement_from_let<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &LetStatementNode,
) -> StatementResult<'a> {
    let expr = match &node.kind {
        LetKind::Invalid => {
            let type_id = ctx.ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            });
            Expr {
                ty: type_id,
                kind: ExprKind::Zero,
                pos: node.pos,
                assignable: false,
            }
        }
        LetKind::TypeOnly { ty } => {
            let type_id = get_type_from_node(ctx.ctx, ctx.scope, ty);
            Expr {
                ty: type_id,
                kind: ExprKind::Zero,
                pos: node.pos,
                assignable: false,
            }
        }
        LetKind::TypeValue { ty, value } => {
            let ty = get_type_from_node(ctx.ctx, ctx.scope, ty);
            let mut value_expr = get_expr_from_node(ctx.ctx, ctx.scope, Some(ty), value);
            if !ty.is_assignable_with(value_expr.ty) {
                ctx.ctx.errors.type_mismatch(value.pos(), ty, value_expr.ty);
                value_expr.kind = ExprKind::Invalid
            }
            value_expr
        }
        LetKind::ValueOnly { value } => get_expr_from_node(ctx.ctx, ctx.scope, None, value),
    };

    let name = ctx.ctx.define_symbol(&node.name.value);
    let mut new_table = IndexMap::default();
    let id = ctx.last_unused_local;
    new_table.insert(
        name,
        ValueObject::Local(LocalObject {
            id,
            ty: expr.ty,
            name,
        }),
    );
    let new_scope = ctx.scope.value_scopes.new_child(new_table);
    let new_scope = ctx.scope.with_value_scope(new_scope);

    StatementResult {
        statement: Statement::NewLocal{id, value: expr},
        new_scope: Some(new_scope),
        is_returning: false,
        last_unused_local: ctx.last_unused_local + 1,
    }
}

pub(crate) fn get_statement_from_assign<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &AssignStatementNode,
) -> StatementResult<'a> {
    let receiver = get_expr_from_node(ctx.ctx, ctx.scope, None, &node.receiver);
    if !receiver.assignable {
        ctx.ctx.errors.expr_is_not_assignable(node.receiver.pos());
    }

    let value = get_expr_from_node(ctx.ctx, ctx.scope, Some(receiver.ty), &node.value);
    if !receiver.ty.is_assignable_with(value.ty) {
        ctx.ctx
            .errors
            .type_mismatch(node.value.pos(), receiver.ty, value.ty);
    }

    StatementResult {
        statement: Statement::Assign(receiver, value),
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_block<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &BlockStatementNode,
) -> StatementResult<'a> {
    let mut scope = ctx.scope.clone();
    let mut statements = BumpVec::with_capacity_in(node.statements.len(), ctx.ctx.arena);
    let mut last_unused_local = ctx.last_unused_local;
    let mut is_returning = false;
    let mut unreachable_error = false;
    for stmt in &node.statements {
        if is_returning && !unreachable_error {
            ctx.ctx.errors.unreachable_statement(stmt.pos());
            unreachable_error = true;
        }

        let result = get_statement_from_node(
            &StatementContext {
                ctx: ctx.ctx,
                scope: &scope,
                last_unused_local,
                return_type: ctx.return_type,
                is_inside_loop: ctx.is_inside_loop,
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

pub(crate) fn get_statement_from_if<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &IfStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });
    let cond = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !cond.ty.is_bool() {
        ctx.ctx
            .errors
            .type_mismatch(node.condition.pos(), TypeRepr::Bool, cond.ty);
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
        statement: Statement::If(IfStatement {
            cond,
            body: Box::new(body),
            else_stmt: else_stmt.map(Box::new),
        }),
        new_scope: None,
        is_returning: body_is_returning && else_is_returning,
        last_unused_local,
    }
}

pub(crate) fn get_statement_from_while<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &WhileStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });
    let condition = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !condition.ty.is_bool() {
        ctx.ctx
            .errors
            .type_mismatch(node.condition.pos(), TypeRepr::Bool, condition.ty);
    }

    let body_stmt = get_statement_from_block(
        &StatementContext {
            ctx: ctx.ctx,
            scope: ctx.scope,
            last_unused_local: ctx.last_unused_local,
            return_type: ctx.return_type,
            is_inside_loop: true,
        },
        &node.body,
    );

    StatementResult {
        statement: Statement::While(WhileStatement {
            cond: condition,
            body: Box::new(body_stmt.statement),
        }),
        new_scope: None,
        is_returning: false,
        last_unused_local: body_stmt.last_unused_local,
    }
}

pub(crate) fn get_statement_from_continue<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    pos: Pos,
) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        ctx.ctx.errors.operation_outside_loop(pos, "continue");
    }
    StatementResult {
        statement: Statement::Continue,
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_break<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    pos: Pos,
) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        ctx.ctx.errors.operation_outside_loop(pos, "break");
    }
    StatementResult {
        statement: Statement::Break,
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_return<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &ReturnStatementNode,
) -> StatementResult<'a> {
    let return_type = ctx.return_type;

    let value = node
        .value
        .as_ref()
        .map(|expr| get_expr_from_node(ctx.ctx, ctx.scope, Some(return_type), expr));

    let value_ty = value
        .as_ref()
        .map(|expr| expr.ty)
        .unwrap_or(ctx.ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Void,
        }));

    if !return_type.is_assignable_with(value_ty) {
        ctx.ctx
            .errors
            .type_mismatch(node.pos, return_type, value_ty);
    };

    StatementResult {
        statement: Statement::Return(value),
        new_scope: None,
        is_returning: true,
        last_unused_local: ctx.last_unused_local,
    }
}
