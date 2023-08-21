use crate::analyze::TypeCheckContext;
use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::scope::{LocalObject, Object, Scope};
use crate::ty::{
    display_type, display_type_id, get_type_from_node, is_type_assignable, Type, TypeId,
};
use indexmap::IndexMap;
use magelang_syntax::{
    BlockStatementNode, ErrorReporter, IfStatementNode, LetKind, LetStatementNode, StatementNode,
};
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement {
    Native,
    NewLocal(Box<Expr>),
    Block(Vec<Statement>),
    If(IfStatement),
    While(WhileStatement),
    Return(Option<Expr>),
    Expr(Expr),
    Assign(Expr, Expr),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
}

pub struct StatementResult {
    pub statement: Statement,
    pub new_scope: Option<Rc<Scope>>,
    pub is_returning: bool,
    pub last_unused_local: usize,
}

pub fn get_stmt_from_block_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    last_unused_local: usize,
    return_type: TypeId,
    is_inside_loop: bool,
    node: &BlockStatementNode,
) -> StatementResult {
    let mut ctx = ctx.with_scope(ctx.scope.clone());

    let mut statements = Vec::default();
    let mut last_unused_local = last_unused_local;
    let mut is_returning = false;
    let mut unreachable_error = false;
    for stmt in &node.statements {
        if is_returning && !unreachable_error {
            ctx.errors.unreachable_statement(stmt.pos());
            unreachable_error = true;
        }

        let result = get_stmt_from_node(&ctx, last_unused_local, return_type, is_inside_loop, stmt);
        statements.push(result.statement);
        last_unused_local = result.last_unused_local;
        if result.is_returning {
            is_returning = true;
        }
        if let Some(new_scope) = result.new_scope {
            ctx = ctx.with_scope(new_scope);
        }
    }
    StatementResult {
        statement: Statement::Block(statements),
        new_scope: None,
        is_returning,
        last_unused_local,
    }
}

fn get_stmt_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    last_unused_local: usize,
    return_type: TypeId,
    is_inside_loop: bool,
    node: &StatementNode,
) -> StatementResult {
    match node {
        StatementNode::Let(node) => get_stmt_from_let_node(ctx, last_unused_local, node),
        StatementNode::Assign(node) => {
            let receiver = get_expr_from_node(ctx, None, &node.receiver);
            if !receiver.assignable {
                ctx.errors.expr_is_not_assignable(node.receiver.pos());
            }

            let value = get_expr_from_node(ctx, Some(receiver.ty), &node.value);
            if !is_type_assignable(ctx, receiver.ty, value.ty) {
                ctx.errors.type_mismatch(
                    node.value.pos(),
                    display_type_id(ctx, receiver.ty),
                    display_type_id(ctx, value.ty),
                );
            }

            StatementResult {
                statement: Statement::Assign(receiver, value),
                new_scope: None,
                is_returning: false,
                last_unused_local,
            }
        }
        StatementNode::Block(node) => {
            get_stmt_from_block_node(ctx, last_unused_local, return_type, is_inside_loop, node)
        }
        StatementNode::If(node) => {
            get_stmt_from_if_node(ctx, last_unused_local, return_type, is_inside_loop, node)
        }
        StatementNode::While(node) => {
            let bool_type_id = ctx.types.define(Type::Bool);
            let condition = get_expr_from_node(ctx, Some(bool_type_id), &node.condition);

            if !ctx.types.get(condition.ty).is_bool() {
                ctx.errors.type_mismatch(
                    node.condition.pos(),
                    display_type(ctx, &Type::Bool),
                    display_type_id(ctx, condition.ty),
                );
            }

            let body_stmt =
                get_stmt_from_block_node(ctx, last_unused_local, return_type, true, &node.body);

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
        StatementNode::Continue(token) => {
            if !is_inside_loop {
                ctx.errors.operation_outside_loop(token.pos, "continue");
            }
            StatementResult {
                statement: Statement::Continue,
                new_scope: None,
                is_returning: false,
                last_unused_local,
            }
        }
        StatementNode::Break(token) => {
            if !is_inside_loop {
                ctx.errors.operation_outside_loop(token.pos, "break");
            }
            StatementResult {
                statement: Statement::Break,
                new_scope: None,
                is_returning: false,
                last_unused_local,
            }
        }
        StatementNode::Return(node) => {
            let return_ty = ctx.types.get(return_type);

            let value = node
                .value
                .as_ref()
                .map(|expr| get_expr_from_node(ctx, Some(return_type), expr));

            let value_ty = value
                .as_ref()
                .map(|expr| expr.ty)
                .unwrap_or(ctx.types.define(Type::Void));

            if !is_type_assignable(ctx, return_type, value_ty) {
                ctx.errors.type_mismatch(
                    node.pos,
                    display_type(ctx, &return_ty),
                    display_type(ctx, &ctx.types.get(value_ty)),
                );
            };

            StatementResult {
                statement: Statement::Return(value),
                new_scope: None,
                is_returning: true,
                last_unused_local,
            }
        }
        StatementNode::Expr(node) => StatementResult {
            statement: Statement::Expr(get_expr_from_node(ctx, None, node)),
            new_scope: None,
            is_returning: false,
            last_unused_local,
        },
    }
}

fn get_stmt_from_let_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    last_unused_local: usize,
    stmt: &LetStatementNode,
) -> StatementResult {
    let expr = match &stmt.kind {
        LetKind::TypeOnly { ty } => {
            let type_id = get_type_from_node(ctx, ty);
            Expr {
                ty: type_id,
                kind: ExprKind::Zero,
                assignable: false,
            }
        }
        LetKind::TypeValue { ty, value } => {
            let type_id = get_type_from_node(ctx, ty);
            let mut value_expr = get_expr_from_node(ctx, Some(type_id), value);
            if !is_type_assignable(ctx, type_id, value_expr.ty) {
                ctx.errors.type_mismatch(
                    value.pos(),
                    display_type_id(ctx, type_id),
                    display_type_id(ctx, value_expr.ty),
                );
                value_expr.kind = ExprKind::Invalid
            }
            value_expr
        }
        LetKind::ValueOnly { value } => get_expr_from_node(ctx, None, value),
    };

    let name = ctx.symbols.define(&stmt.name.value);
    let mut new_table = IndexMap::default();
    new_table.insert(
        name,
        Object::Local(LocalObject {
            id: last_unused_local,
            ty: expr.ty,
        }),
    );
    let new_scope = Rc::new(ctx.scope.new_child(new_table));

    StatementResult {
        statement: Statement::NewLocal(Box::new(expr)),
        new_scope: Some(new_scope),
        is_returning: false,
        last_unused_local: last_unused_local + 1,
    }
}

fn get_stmt_from_if_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    last_unused_local: usize,
    return_type: TypeId,
    is_inside_loop: bool,
    node: &IfStatementNode,
) -> StatementResult {
    let bool_type_id = ctx.types.define(Type::Bool);
    let cond = get_expr_from_node(ctx, Some(bool_type_id), &node.condition);

    if !ctx.types.get(cond.ty).is_bool() {
        ctx.errors.type_mismatch(
            node.condition.pos(),
            display_type(ctx, &Type::Bool),
            display_type_id(ctx, cond.ty),
        );
    }

    let result = get_stmt_from_block_node(
        ctx,
        last_unused_local,
        return_type,
        is_inside_loop,
        &node.body,
    );
    let body = result.statement;
    let body_is_returning = result.is_returning;
    let last_unused_local = result.last_unused_local;

    let else_result = node.else_node.as_ref().map(|else_body| {
        get_stmt_from_node(
            ctx,
            last_unused_local,
            return_type,
            is_inside_loop,
            else_body,
        )
    });

    let (else_stmt, last_unused_local, else_is_returning) = if let Some(result) = else_result {
        (
            Some(result.statement),
            result.last_unused_local,
            result.is_returning,
        )
    } else {
        (None, last_unused_local, false)
    };

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
