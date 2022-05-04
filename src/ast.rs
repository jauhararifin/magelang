use crate::pos::Pos;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct Root {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Fn(FnDecl),
}

impl Declaration {
    pub fn try_unwrap_func(&self) -> Option<&FnDecl> {
        if let Self::Fn(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn is_func(&self) -> bool {
        self.try_unwrap_func().is_some()
    }
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub fn_token: Token,
    pub name: Token,
    pub header: FnHeader,
    pub body: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct FnHeader {
    pub native: bool,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Token,
    pub typ: Type,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Token,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(Token),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Var),
    Assign(Assign),
    Return(Return),
    If(If),
    While(While),
    Block(BlockStatement),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub receiver: Expr,
    pub op: Token,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub ret: Token,
    pub value: Option<Expr>,
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
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    BoolLit(Token),
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
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
    pub func: Box<Expr>,
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
