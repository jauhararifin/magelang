use crate::analyze::TypeCheckContext;
use crate::expr::Expr;
use crate::scope::Scope;
use crate::ty::TypeId;
use magelang_syntax::BlockStatementNode;
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

pub fn get_stmt_from_block_node<E>(
    ctx: &TypeCheckContext<E>,
    last_unused_local: usize,
    return_type: TypeId,
    is_inside_loop: bool,
    node: &BlockStatementNode,
) -> StatementResult {
    todo!();
}
