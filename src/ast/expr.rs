use crate::{token::Token, pos::Pos};

use super::TypeNode;

#[derive(Debug, Clone)]
pub struct ExprNode {
    pub kind: ExprNodeKind,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum ExprNodeKind {
    Empty,
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    BoolLit(Token),
    Binary(BinaryNode),
    Unary(UnaryNode),
    FunctionCall(FunctionCallNode),
    Index(IndexNode),
    Array(ArrayNode),
    Cast(CastNode),
}

#[derive(Debug, Clone)]
pub struct BinaryNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct UnaryNode {
    pub op: Token,
    pub val: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallNode {
    pub func: Box<ExprNode>,
    pub args: Vec<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct IndexNode {
    pub array: Box<ExprNode>,
    pub index: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct ArrayNode {
    pub typ: TypeNode,
    pub size: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct CastNode {
    pub val: Box<ExprNode>,
    pub as_token: Token,
    pub target: TypeNode,
}

