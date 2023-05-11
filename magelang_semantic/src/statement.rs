use crate::expr::{Expr, GlobalId};

#[derive(Debug)]
pub enum Statement {
    Invalid,
    SetLocal(usize, Expr),
    SetGlobal(GlobalId, Expr),
    SetIndex { target: Expr, index: Expr, value: Expr },
    SetAddr { addr: Expr, value: Expr },
    If(IfStatement),
    While(WhileStatement),
    Continue,
    Break,
    Block(BlockStatement),
    Return(ReturnStatement),
    Expr(Expr),
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expr,
    pub body: Box<Statement>,
    pub else_body: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Option<Expr>,
}
