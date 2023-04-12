use crate::expr::Expr;

#[derive(Debug)]
pub enum Statement {
    Invalid,
    Local(Expr),
    Block(BlockStatement),
    Return(ReturnStatement),
    Expr(Expr),
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Option<Expr>,
}
