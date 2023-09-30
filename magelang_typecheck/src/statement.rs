use crate::analyze::{Context, LocalObject, Scopes, ValueObject};
use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::ty::{get_type_from_node, InternType, InternTypeArgs, Type};
use indexmap::IndexMap;
use magelang_syntax::{
    AssignStatementNode, BlockStatementNode, ErrorReporter, IfStatementNode, LetKind,
    LetStatementNode, ReturnStatementNode, StatementNode, Token, TokenKind, WhileStatementNode,
};

#[derive(Debug)]
pub enum Statement<'a> {
    Native,
    NewLocal(Expr<'a>),
    Block(Vec<Statement<'a>>),
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
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> Statement<'a> {
        match self {
            Statement::Native => Statement::Native,
            Statement::Continue => Statement::Continue,
            Statement::Break => Statement::Break,
            Statement::NewLocal(value) => {
                let value = value.monomorphize(ctx, type_args);
                Statement::NewLocal(value)
            }
            Statement::Block(statements) => {
                let statements = statements
                    .iter()
                    .map(|stmt| stmt.monomorphize(ctx, type_args))
                    .collect();
                Statement::Block(statements)
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

#[derive(Debug)]
pub struct IfStatement<'a> {
    pub(crate) cond: Expr<'a>,
    pub(crate) body: Box<Statement<'a>>,
    pub(crate) else_stmt: Option<Box<Statement<'a>>>,
}

#[derive(Debug)]
pub struct WhileStatement<'a> {
    pub(crate) cond: Expr<'a>,
    pub(crate) body: Box<Statement<'a>>,
}

pub(crate) struct StatementResult<'a> {
    pub(crate) statement: Statement<'a>,
    pub(crate) new_scope: Option<Scopes<'a>>,
    pub(crate) is_returning: bool,
    pub(crate) last_unused_local: usize,
}

pub(crate) struct StatementContext<'a, 'b, E: ErrorReporter> {
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    last_unused_local: usize,
    return_type: InternType<'a>,
    is_inside_loop: bool,
}

impl<'a, 'b, E: ErrorReporter> StatementContext<'a, 'b, E> {
    pub(crate) fn new(
        ctx: &'b Context<'a, E>,
        scope: &'b Scopes<'a>,
        return_type: InternType<'a>,
    ) -> Self {
        Self {
            ctx,
            scope,
            last_unused_local: 0,
            return_type,
            is_inside_loop: false,
        }
    }
}

pub(crate) fn get_statement_from_node<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &StatementNode,
) -> StatementResult<'a> {
    match node {
        StatementNode::Let(node) => get_statement_from_let(ctx, node),
        StatementNode::Assign(node) => get_statement_from_assign(ctx, node),
        StatementNode::Block(node) => get_statement_from_block(ctx, node),
        StatementNode::If(node) => get_statement_from_if(ctx, node),
        StatementNode::While(node) => get_statement_from_while(ctx, node),
        StatementNode::Continue(token) | StatementNode::Break(token) => {
            get_statement_from_keyword(ctx, token)
        }
        StatementNode::Return(node) => get_statement_from_return(ctx, node),
        StatementNode::Expr(node) => StatementResult {
            statement: Statement::Expr(get_expr_from_node(ctx.ctx, ctx.scope, None, node)),
            new_scope: None,
            is_returning: false,
            last_unused_local: ctx.last_unused_local,
        },
    }
}

pub(crate) fn get_statement_from_let<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &LetStatementNode,
) -> StatementResult<'a> {
    let expr = match &node.kind {
        LetKind::TypeOnly { ty } => {
            let type_id = get_type_from_node(ctx.ctx, ctx.scope, ty);
            Expr {
                ty: type_id,
                kind: ExprKind::Zero,
                assignable: false,
            }
        }
        LetKind::TypeValue { ty, value } => {
            let ty = get_type_from_node(ctx.ctx, ctx.scope, ty);
            let mut value_expr = get_expr_from_node(ctx.ctx, ctx.scope, Some(ty), value);
            if !ty.is_assignable_with(&value_expr.ty) {
                ctx.ctx.errors.type_mismatch(value.pos(), ty, value_expr.ty);
                value_expr.kind = ExprKind::Invalid
            }
            value_expr
        }
        LetKind::ValueOnly { value } => get_expr_from_node(ctx.ctx, ctx.scope, None, value),
    };

    let name = ctx.ctx.define_symbol(&node.name.value);
    let mut new_table = IndexMap::default();
    new_table.insert(
        name,
        ValueObject::Local(LocalObject {
            id: ctx.last_unused_local,
            ty: expr.ty,
            name,
        }),
    );
    let new_scope = ctx.scope.value_scopes.new_child(new_table);
    let new_scope = ctx.scope.with_value_scope(new_scope);

    StatementResult {
        statement: Statement::NewLocal(expr),
        new_scope: Some(new_scope),
        is_returning: false,
        last_unused_local: ctx.last_unused_local + 1,
    }
}

pub(crate) fn get_statement_from_assign<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &AssignStatementNode,
) -> StatementResult<'a> {
    let receiver = get_expr_from_node(ctx.ctx, ctx.scope, None, &node.receiver);
    if !receiver.assignable {
        ctx.ctx.errors.expr_is_not_assignable(node.receiver.pos());
    }

    let value = get_expr_from_node(ctx.ctx, ctx.scope, Some(receiver.ty), &node.value);
    if !receiver.ty.is_assignable_with(&value.ty) {
        ctx.ctx
            .errors
            .type_mismatch(node.value.pos(), receiver.ty.as_ref(), value.ty.as_ref());
    }

    StatementResult {
        statement: Statement::Assign(receiver, value),
        new_scope: None,
        is_returning: false,
        last_unused_local: ctx.last_unused_local,
    }
}

pub(crate) fn get_statement_from_block<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &BlockStatementNode,
) -> StatementResult<'a> {
    let mut scope = ctx.scope.clone();
    let mut statements = Vec::default();
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
        statement: Statement::Block(statements),
        new_scope: None,
        is_returning,
        last_unused_local,
    }
}

pub(crate) fn get_statement_from_if<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &IfStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type::Bool);
    let cond = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !cond.ty.is_bool() {
        ctx.ctx
            .errors
            .type_mismatch(node.condition.pos(), Type::Bool, cond.ty);
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

pub(crate) fn get_statement_from_while<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    node: &WhileStatementNode,
) -> StatementResult<'a> {
    let bool_type = ctx.ctx.define_type(Type::Bool);
    let condition = get_expr_from_node(ctx.ctx, ctx.scope, Some(bool_type), &node.condition);

    if !condition.ty.is_bool() {
        ctx.ctx
            .errors
            .type_mismatch(node.condition.pos(), Type::Bool, condition.ty);
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

pub(crate) fn get_statement_from_keyword<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
    token: &Token,
) -> StatementResult<'a> {
    match token.kind {
        TokenKind::Continue => {
            if !ctx.is_inside_loop {
                ctx.ctx.errors.operation_outside_loop(token.pos, "continue");
            }
            StatementResult {
                statement: Statement::Continue,
                new_scope: None,
                is_returning: false,
                last_unused_local: ctx.last_unused_local,
            }
        }
        TokenKind::Break => {
            if !ctx.is_inside_loop {
                ctx.ctx.errors.operation_outside_loop(token.pos, "break");
            }
            StatementResult {
                statement: Statement::Break,
                new_scope: None,
                is_returning: false,
                last_unused_local: ctx.last_unused_local,
            }
        }
        _ => unreachable!("the keyword is not a statement"),
    }
}

pub(crate) fn get_statement_from_return<'a, 'b, E: ErrorReporter>(
    ctx: &StatementContext<'a, 'b, E>,
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
        .unwrap_or(ctx.ctx.define_type(Type::Void));

    if !return_type.is_assignable_with(&value_ty) {
        ctx.ctx
            .errors
            .type_mismatch(node.pos, &return_type, &value_ty);
    };

    StatementResult {
        statement: Statement::Return(value),
        new_scope: None,
        is_returning: true,
        last_unused_local: ctx.last_unused_local,
    }
}
