use crate::token::{Pos, Token};

#[derive(Debug, Clone)]
pub struct Root {
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
    Pointer(Pointer),
    // TODO: add tuple.
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub fields: Vec<Param>,
}

#[derive(Debug, Clone)]
pub struct Pointer {
    pub elem: Box<Type>,
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
pub struct Expr {
    pub kind: ExprKind,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    StringLit(Token),
    BoolLit(Token),
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
    Selector(Selector),
    // TODO: add struct literal.
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
pub struct Selector {
    pub source: Box<Expr>,
    pub selection: Token,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Ast {
    Root(Root),
    FnDecl(FnDecl),
    Var(Var),
    TypeDecl(TypeDecl),
    Param(Param),
    Type(Type),
    Struct(Struct),
    Statement(Statement),
    Assign(Assign),
    If(If),
    While(While),
    Return(Return),
    Expr(Expr),
    BlockStatement(BlockStatement),
    Empty,
}

macro_rules! from_ast {
    ($target: ident) => {
        impl From<Ast> for $target {
            fn from(ast: Ast) -> $target {
                if let Ast::$target(inner) = ast {
                    inner
                } else {
                    panic!("invalid conversion from AST to $target");
                }
            }
        }
    };
}

from_ast!(Root);
from_ast!(FnDecl);
from_ast!(Var);
from_ast!(TypeDecl);
from_ast!(Param);
from_ast!(Type);
from_ast!(Struct);
from_ast!(Statement);
from_ast!(Assign);
from_ast!(If);
from_ast!(While);
from_ast!(Return);
from_ast!(Expr);
from_ast!(BlockStatement);
