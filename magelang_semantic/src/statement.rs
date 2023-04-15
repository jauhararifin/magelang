use crate::expr::Expr;

#[derive(Debug)]
pub enum Statement {
    Invalid,
    Local(Expr),
    SetLocal(usize, Expr),
    If(IfStatement),
    While(WhileStatement),
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
