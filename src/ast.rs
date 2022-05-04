use crate::pos::Pos;
use crate::token::Token;

#[derive(Debug, Clone)]
pub struct RootNode {
    pub declarations: Vec<DeclNode>,
}

#[derive(Debug, Clone)]
pub enum DeclNode {
    Fn(FnDeclNode),
}

impl DeclNode {
    pub fn try_unwrap_func(&self) -> Option<&FnDeclNode> {
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
pub struct FnDeclNode {
    pub fn_token: Token,
    pub name: Token,
    pub header: FnHeaderNode,
    pub body: Option<BlockStatementNode>,
}

#[derive(Debug, Clone)]
pub struct FnHeaderNode {
    pub native_token: Option<Token>,
    pub params: Vec<ParamNode>,
    pub ret_type: Option<TypeNode>,
}

#[derive(Debug, Clone)]
pub struct VarNode {
    pub name: Token,
    pub typ: TypeNode,
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct ParamNode {
    pub name: Token,
    pub typ: TypeNode,
}

#[derive(Debug, Clone)]
pub enum TypeNode {
    Primitive(Token),
}

#[derive(Debug, Clone)]
pub enum StatementNode {
    Var(VarNode),
    Assign(AssignNode),
    Return(ReturnNode),
    If(IfNode),
    While(WhileNode),
    Block(BlockStatementNode),
    Expr(ExprNode),
}

#[derive(Debug, Clone)]
pub struct AssignNode {
    pub receiver: ExprNode,
    pub op: Token,
    pub value: ExprNode,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub ret: Token,
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub cond: ExprNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub cond: ExprNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone)]
pub struct ExprNode {
    pub kind: ExprNodeKind,
    pub pos: Pos,
}

#[derive(Debug, Clone)]
pub enum ExprNodeKind {
    Ident(Token),
    IntegerLit(Token),
    FloatLit(Token),
    BoolLit(Token),
    Binary(BinaryNode),
    Unary(UnaryNode),
    FunctionCall(FunctionCallNode),
    Cast(CastNode),
}

#[derive(Debug, Clone)]
pub struct BinaryNode {
    pub op: Token,
    pub a: Box<ExprNode>,
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
pub struct CastNode {
    pub target: TypeNode,
    pub val: Box<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct BlockStatementNode {
    pub body: Vec<StatementNode>,
}
