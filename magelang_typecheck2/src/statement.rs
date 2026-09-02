//! Statement checking.

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::resolve::resolve_type;
use crate::scope::{Scopes, ValueEntry};
use crate::ty::{Type, TypeRepr};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{
    AssignStatementNode, BlockStatementNode, ErrorReporter, IfStatementNode, LetKind,
    LetStatementNode, Pos, ReturnStatementNode, StatementNode, WhileStatementNode,
};

#[derive(Debug)]
pub enum Statement<'a> {
    /// The body of a function without one (imported or intrinsic).
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

#[derive(Debug)]
pub struct IfStatement<'a> {
    pub cond: Expr<'a>,
    pub body: Box<Statement<'a>>,
    pub else_stmt: Option<Box<Statement<'a>>>,
}

#[derive(Debug)]
pub struct WhileStatement<'a> {
    pub cond: Expr<'a>,
    pub body: Box<Statement<'a>>,
}

pub(crate) struct StatementResult<'a> {
    pub(crate) statement: Statement<'a>,
    /// The scope for the statements that follow (a `let` introduces a local).
    pub(crate) new_scope: Option<Scopes<'a>>,
    /// Whether every path through the statement returns.
    pub(crate) is_returning: bool,
    pub(crate) last_unused_local: usize,
}

pub(crate) struct StatementContext<'a, 'b, 'syn, E> {
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

    fn with(&self, scope: &'b Scopes<'a>, last_unused_local: usize, is_inside_loop: bool) -> Self {
        Self {
            ctx: self.ctx,
            scope,
            last_unused_local,
            return_type: self.return_type,
            is_inside_loop,
        }
    }

    fn result(&self, statement: Statement<'a>) -> StatementResult<'a> {
        StatementResult {
            statement,
            new_scope: None,
            is_returning: false,
            last_unused_local: self.last_unused_local,
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
        StatementNode::Expr(node) => {
            let expr = get_expr_from_node(ctx.ctx, ctx.scope, None, node);
            ctx.result(Statement::Expr(expr))
        }
    }
}

fn get_statement_from_let<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &LetStatementNode,
) -> StatementResult<'a> {
    let zero = |ty: &'a Type<'a>| Expr {
        ty,
        kind: ExprKind::Zero,
        pos: node.pos,
        assignable: false,
    };

    let expr = match &node.kind {
        LetKind::Invalid => zero(ctx.ctx.unknown_type()),
        LetKind::TypeOnly { ty } => zero(resolve_type(ctx.ctx, ctx.scope, ty)),
        LetKind::TypeValue { ty, value } => {
            let ty = resolve_type(ctx.ctx, ctx.scope, ty);
            let mut value_expr = get_expr_from_node(ctx.ctx, ctx.scope, Some(ty), value);
            if !ty.is_assignable_with(value_expr.ty) {
                ctx.ctx.errors.type_mismatch(value.pos(), ty, value_expr.ty);
                value_expr.kind = ExprKind::Invalid;
            }
            value_expr
        }
        LetKind::ValueOnly { value } => get_expr_from_node(ctx.ctx, ctx.scope, None, value),
    };

    let name = ctx.ctx.define_symbol(&node.name.value);
    let id = ctx.last_unused_local;
    let mut locals = IndexMap::default();
    locals.insert(name, ValueEntry::Local { id, ty: expr.ty });
    let new_scope = ctx.scope.with_locals(locals);

    StatementResult {
        statement: Statement::NewLocal { id, value: expr },
        new_scope: Some(new_scope),
        is_returning: false,
        last_unused_local: ctx.last_unused_local + 1,
    }
}

fn get_statement_from_assign<'a, E: ErrorReporter>(
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

    ctx.result(Statement::Assign(receiver, value))
}

pub(crate) fn get_statement_from_block<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &BlockStatementNode,
) -> StatementResult<'a> {
    let mut scope = ctx.scope.clone();
    let mut statements = BumpVec::with_capacity_in(node.statements.len(), ctx.ctx.arena);
    let mut last_unused_local = ctx.last_unused_local;
    let mut is_returning = false;
    let mut unreachable_reported = false;

    for stmt in &node.statements {
        if is_returning && !unreachable_reported {
            ctx.ctx.errors.unreachable_statement(stmt.pos());
            unreachable_reported = true;
        }

        let result = get_statement_from_node(
            &ctx.with(&scope, last_unused_local, ctx.is_inside_loop),
            stmt,
        );

        statements.push(result.statement);
        last_unused_local = result.last_unused_local;
        is_returning |= result.is_returning;
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

fn get_statement_from_if<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &IfStatementNode,
) -> StatementResult<'a> {
    let cond = get_condition(ctx, &node.condition);

    let body = get_statement_from_block(ctx, &node.body);
    let mut last_unused_local = body.last_unused_local;

    let mut else_stmt = None;
    let mut else_is_returning = false;
    if let Some(else_node) = &node.else_node {
        let result = get_statement_from_node(
            &ctx.with(ctx.scope, last_unused_local, ctx.is_inside_loop),
            else_node,
        );
        else_stmt = Some(Box::new(result.statement));
        last_unused_local = result.last_unused_local;
        else_is_returning = result.is_returning;
    }

    StatementResult {
        statement: Statement::If(IfStatement {
            cond,
            body: Box::new(body.statement),
            else_stmt,
        }),
        new_scope: None,
        is_returning: body.is_returning && else_is_returning,
        last_unused_local,
    }
}

fn get_statement_from_while<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &WhileStatementNode,
) -> StatementResult<'a> {
    let cond = get_condition(ctx, &node.condition);

    let body = get_statement_from_block(
        &ctx.with(ctx.scope, ctx.last_unused_local, true),
        &node.body,
    );

    StatementResult {
        statement: Statement::While(WhileStatement {
            cond,
            body: Box::new(body.statement),
        }),
        new_scope: None,
        is_returning: false,
        last_unused_local: body.last_unused_local,
    }
}

fn get_condition<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    node: &magelang_syntax::ExprNode,
) -> Expr<'a> {
    let cond = get_expr_from_node(ctx.ctx, ctx.scope, Some(ctx.ctx.bool_type()), node);
    if !cond.ty.is_bool() {
        ctx.ctx
            .errors
            .type_mismatch(node.pos(), TypeRepr::Bool, cond.ty);
    }
    cond
}

fn get_statement_from_continue<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    pos: Pos,
) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        ctx.ctx.errors.operation_outside_loop(pos, "continue");
    }
    ctx.result(Statement::Continue)
}

fn get_statement_from_break<'a, E: ErrorReporter>(
    ctx: &StatementContext<'a, '_, '_, E>,
    pos: Pos,
) -> StatementResult<'a> {
    if !ctx.is_inside_loop {
        ctx.ctx.errors.operation_outside_loop(pos, "break");
    }
    ctx.result(Statement::Break)
}

fn get_statement_from_return<'a, E: ErrorReporter>(
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
        .unwrap_or_else(|| ctx.ctx.void_type());

    if !return_type.is_assignable_with(value_ty) {
        ctx.ctx
            .errors
            .type_mismatch(node.pos, return_type, value_ty);
    }

    StatementResult {
        statement: Statement::Return(value),
        new_scope: None,
        is_returning: true,
        last_unused_local: ctx.last_unused_local,
    }
}
