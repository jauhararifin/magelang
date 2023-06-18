use crate::token::{Pos, Token};
use std::rc::Rc;

pub trait AstNode {
    fn get_pos(&self) -> Pos;
}

#[derive(Debug, PartialEq, Eq)]
pub struct PackageNode {
    pub items: Vec<Rc<ItemNode>>,
    pub comments: Vec<Token>,
}

impl PackageNode {
    pub fn imports(&self) -> impl Iterator<Item = &ImportNode> {
        self.items.iter().map(Rc::as_ref).filter_map(ItemNode::as_import)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemNode {
    Import(ImportNode),
    Struct(StructNode),
    Global(GlobalNode),
    Function(FunctionNode),
    NativeFunction(SignatureNode),
}

impl ItemNode {
    pub fn name(&self) -> &str {
        match self {
            Self::Import(node) => &node.name.value,
            Self::Struct(node) => &node.name.value,
            Self::Global(node) => &node.name.value,
            Self::Function(node) => &node.signature.name.value,
            Self::NativeFunction(node) => &node.name.value,
        }
    }

    pub fn as_import(&self) -> Option<&ImportNode> {
        if let Self::Import(node) = self {
            Some(node)
        } else {
            None
        }
    }

    pub fn as_global(&self) -> Option<&GlobalNode> {
        if let Self::Global(node) = self {
            Some(node)
        } else {
            None
        }
    }

    pub fn as_function(&self) -> Option<&FunctionNode> {
        if let Self::Function(node) = self {
            Some(node)
        } else {
            None
        }
    }

    pub fn as_native_function(&self) -> Option<&SignatureNode> {
        if let Self::NativeFunction(node) = self {
            Some(node)
        } else {
            None
        }
    }

    pub fn as_struct(&self) -> Option<&StructNode> {
        if let Self::Struct(node) = self {
            Some(node)
        } else {
            None
        }
    }
}

impl AstNode for ItemNode {
    fn get_pos(&self) -> Pos {
        match self {
            Self::Import(node) => node.get_pos(),
            Self::Struct(node) => node.get_pos(),
            Self::Global(node) => node.get_pos(),
            Self::Function(node) => node.get_pos(),
            Self::NativeFunction(node) => node.get_pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportNode {
    pub pos: Pos,
    pub name: Token,
    pub path: Token,
}

impl AstNode for ImportNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructNode {
    pub pos: Pos,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub fields: Vec<StructFieldNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructFieldNode {
    pub pos: Pos,
    pub name: Token,
    pub type_expr: ExprNode,
}

impl AstNode for StructNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalNode {
    pub pos: Pos,
    pub name: Token,
    pub ty: ExprNode,
    pub value: Option<ExprNode>,
}

impl AstNode for GlobalNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionNode {
    pub pos: Pos,
    pub signature: SignatureNode,
    pub body: BlockStatementNode,
}

impl AstNode for FunctionNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SignatureNode {
    pub pos: Pos,
    pub tags: Vec<TagNode>,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<ExprNode>,
}

impl AstNode for SignatureNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TagNode {
    pub pos: Pos,
    pub name: Token,
    pub arguments: Vec<Token>,
}

impl AstNode for TagNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeParameterNode {
    pub name: Token,
}

impl AstNode for TypeParameterNode {
    fn get_pos(&self) -> Pos {
        self.name.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub pos: Pos,
    pub name: Token,
    pub type_expr: ExprNode,
}

impl AstNode for ParameterNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementNode {
    Let(LetStatementNode),
    Assign(AssignStatementNode),
    Block(BlockStatementNode),
    If(IfStatementNode),
    While(WhileStatementNode),
    Continue(Token),
    Break(Token),
    Return(ReturnStatementNode),
    Expr(ExprNode),
}

impl AstNode for StatementNode {
    fn get_pos(&self) -> Pos {
        match self {
            Self::Let(node) => node.get_pos(),
            Self::Assign(node) => node.get_pos(),
            Self::Block(node) => node.get_pos(),
            Self::If(node) => node.get_pos(),
            Self::While(node) => node.get_pos(),
            Self::Continue(token) => token.pos.clone(),
            Self::Break(token) => token.pos.clone(),
            Self::Return(node) => node.get_pos(),
            Self::Expr(node) => node.get_pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStatementNode {
    pub pos: Pos,
    pub name: Token,
    pub kind: LetKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LetKind {
    TypeOnly { ty: ExprNode },
    TypeValue { ty: ExprNode, value: ExprNode },
    ValueOnly { value: ExprNode },
}

impl AstNode for LetStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStatementNode {
    pub pos: Pos,
    pub receiver: ExprNode,
    pub value: ExprNode,
}

impl AstNode for AssignStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatementNode {
    pub pos: Pos,
    pub statements: Vec<StatementNode>,
}

impl AstNode for BlockStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStatementNode {
    pub pos: Pos,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
    pub else_node: ElseNode,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ElseNode {
    None,
    ElseIf(Box<IfStatementNode>),
    Else(BlockStatementNode),
}

impl AstNode for IfStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseIfStatementNode {
    pub pos: Pos,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl AstNode for ElseIfStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileStatementNode {
    pub pos: Pos,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl AstNode for WhileStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStatementNode {
    pub pos: Pos,
    pub value: Option<ExprNode>,
}

impl AstNode for ReturnStatementNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode {
    Ident(Token),
    IntegerLiteral(Token),
    RealLiteral(Token),
    BooleanLit(Token),
    StringLit(Token),
    Binary(BinaryExprNode),
    Deref(DerefExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Cast(CastExprNode),
    ArrayPtr(ArrayPtrExprNode),
    StructLit(StructLitNode),
    Selection(SelectionExprNode),
    Index(IndexExprNode),
    Grouped(GroupedExprNode),
}

impl AstNode for ExprNode {
    fn get_pos(&self) -> Pos {
        match self {
            Self::Ident(val) => val.pos.clone(),
            Self::IntegerLiteral(val) => val.pos.clone(),
            Self::RealLiteral(val) => val.pos.clone(),
            Self::BooleanLit(val) => val.pos.clone(),
            Self::StringLit(val) => val.pos.clone(),
            Self::Binary(val) => val.get_pos(),
            Self::Deref(val) => val.get_pos(),
            Self::Unary(val) => val.get_pos(),
            Self::Call(expr) => expr.pos.clone(),
            Self::Cast(val) => val.get_pos(),
            Self::ArrayPtr(val) => val.get_pos(),
            Self::StructLit(val) => val.get_pos(),
            Self::Selection(val) => val.get_pos(),
            Self::Index(val) => val.get_pos(),
            Self::Grouped(val) => val.get_pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

impl AstNode for BinaryExprNode {
    fn get_pos(&self) -> Pos {
        self.a.get_pos()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct DerefExprNode {
    pub pos: Pos,
    pub value: Box<ExprNode>,
}

impl AstNode for DerefExprNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

impl AstNode for UnaryExprNode {
    fn get_pos(&self) -> Pos {
        self.op.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BuiltinCallExprNode {
    pub target: Token,
    pub arguments: Vec<ExprNode>,
}

impl AstNode for BuiltinCallExprNode {
    fn get_pos(&self) -> Pos {
        self.target.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExprNode {
    pub pos: Pos,
    pub target: Box<ExprNode>,
    pub arguments: Vec<ExprNode>,
}

impl AstNode for CallExprNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub target: Box<ExprNode>,
}

impl AstNode for CastExprNode {
    fn get_pos(&self) -> Pos {
        self.value.get_pos()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayPtrExprNode {
    pub pos: Pos,
    pub element: Box<ExprNode>,
}

impl AstNode for ArrayPtrExprNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructLitNode {
    pub pos: Pos,
    pub target: Box<ExprNode>,
    pub elements: Vec<KeyValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct KeyValue {
    pub pos: Pos,
    pub key: Token,
    pub value: Box<ExprNode>,
}

impl AstNode for StructLitNode {
    fn get_pos(&self) -> Pos {
        self.pos.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub selection: Token,
}

impl AstNode for SelectionExprNode {
    fn get_pos(&self) -> Pos {
        self.value.get_pos()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IndexExprNode {
    pub value: Box<ExprNode>,
    pub index: Vec<ExprNode>,
}

impl AstNode for IndexExprNode {
    fn get_pos(&self) -> Pos {
        self.value.get_pos()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedExprNode {
    pub value: Box<ExprNode>,
}

impl AstNode for GroupedExprNode {
    fn get_pos(&self) -> Pos {
        self.value.get_pos()
    }
}
