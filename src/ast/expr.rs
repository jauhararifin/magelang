use crate::{pos::Pos, token::Token};

#[derive(Debug, Clone, Default)]
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
    Cast(CastNode),
    // type
    PrimitiveType(Token),
    ArrayType(ArrayTypeNode),
}

impl Default for ExprNodeKind {
    fn default() -> Self {
        Self::Empty
    }
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
    pub index: Vec<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct CastNode {
    pub value: Box<ExprNode>,
    pub as_token: Token,
    pub target: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode {
    pub open_brack: Token,
    pub dimension: usize,
    pub elem: Box<ExprNode>,
}
