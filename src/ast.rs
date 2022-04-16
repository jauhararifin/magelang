use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Root {
    // TODO: add packaging and imports
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Fn {
        name: Token,
        param: Vec<Param>,
        ret_type: Option<Type>,
        body: BlockStatement,
    },
    Var {
        name: Token,
        typ: Type,
        value: Option<Expr>,
    },
    Type {
        name: Token,
        typ: Type,
    },
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Token,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(Token),
    Ident(Token),
    // TODO: add inline type.
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarDecl {
        name: Token,
        typ: Type,
        value: Option<Expr>,
    },
    Assign {
        name: Token,
        value: Expr,
    },
    Return(Expr),
    If {
        cond: Expr,
        body: BlockStatement,
    },
    While {
        cond: Expr,
        body: BlockStatement,
    },
    Block(BlockStatement),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    StringLit(Token),
    BoolLit(Token),
    Binary {
        op: Token,
        a: Box<Expr>,
        b: Box<Expr>,
    },
    Unary {
        op: Token,
        val: Box<Expr>,
    },
    FunctionCall {
        ptr: Box<Expr>,
        args: Vec<Expr>,
    },
    // TODO: add selector statement
    Cast {
        target: Type,
        val: Box<Expr>,
    },
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}
