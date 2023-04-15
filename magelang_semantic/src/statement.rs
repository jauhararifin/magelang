use crate::expr::Expr;

#[derive(Debug)]
pub enum Statement {
    Invalid,
    Local(Expr),
    While(WhileStatement),
    Block(BlockStatement),
    Return(ReturnStatement),
    Expr(Expr),
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
