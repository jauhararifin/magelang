use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Root {
    // TODO: add packaging and imports
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Fn(FnDecl),
    Var(Var),
    Type(TypeDecl),
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: Token,
    pub param: Vec<Param>,
    pub ret_type: Option<Type>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Token,
    pub typ: Type,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: Token,
    pub typ: Type,
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
    Struct(Struct),
    // TODO: add tuple.
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Vec<Param>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Var),
    Assign(Assign),
    Return(Expr),
    If(If),
    While(While),
    Block(BlockStatement),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    StringLit(Token),
    BoolLit(Token),
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
    // TODO: add selector statement
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Token,
    pub a: Box<Expr>,
    pub b: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Token,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub ptr: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub target: Type,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}
