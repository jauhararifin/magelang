use crate::token::{Pos, Token};

#[derive(Debug, PartialEq, Eq)]
pub struct PackageNode {
    pub items: Vec<ItemNode>,
    pub comments: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemNode {
    Import(ImportNode),
    Struct(StructNode),
    Global(GlobalNode),
    Function(FunctionNode),
}

impl ItemNode {
    pub fn name(&self) -> &str {
        match self {
            Self::Import(node) => &node.name.value,
            Self::Struct(node) => &node.name.value,
            Self::Global(node) => &node.name.value,
            Self::Function(node) => &node.signature.name.value,
        }
    }

    pub fn pos(&self) -> Pos {
        match self {
            Self::Import(node) => node.pos,
            Self::Struct(node) => node.pos,
            Self::Global(node) => node.pos,
            Self::Function(node) => node.pos,
        }
    }

    pub fn as_import(&self) -> Option<&ImportNode> {
        if let Self::Import(node) = self {
            Some(node)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Token,
    pub path: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub fields: Vec<StructFieldNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructFieldNode {
    pub pos: Pos,
    pub name: Token,
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Token,
    pub ty: TypeExprNode,
    pub value: Option<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionNode {
    pub pos: Pos,
    pub signature: SignatureNode,
    pub body: Option<BlockStatementNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SignatureNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<TypeExprNode>,
    pub end_pos: Pos,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AnnotationNode {
    pub pos: Pos,
    pub name: Token,
    pub arguments: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeParameterNode {
    pub name: Token,
}

impl From<Token> for TypeParameterNode {
    fn from(name: Token) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub pos: Pos,
    pub name: Token,
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeExprNode {
    Invalid(Pos),
    Named(NamedTypeNode),
    Ptr(PtrTypeNode),
    ArrayPtr(ArrayPtrTypeNode),
    Instance(TypeInstanceNode),
    Grouped(Box<TypeExprNode>),
}

impl TypeExprNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Invalid(pos) => *pos,
            Self::Named(node) => node.pos(),
            Self::Ptr(node) => node.pos,
            Self::ArrayPtr(node) => node.pos,
            Self::Instance(node) => node.ty.pos(),
            Self::Grouped(node) => node.pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NamedTypeNode {
    Ident(Token),
    Selection(Token, Token),
}

impl NamedTypeNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Ident(tok) => tok.pos,
            Self::Selection(tok, _) => tok.pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct PtrTypeNode {
    pub pos: Pos,
    pub ty: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrayPtrTypeNode {
    pub pos: Pos,
    pub ty: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeInstanceNode {
    pub ty: NamedTypeNode,
    pub args: Vec<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode {
    Ident(Token),
    Integer(Token),
    Frac(Token),
    Bool(Token),
    String(Token),
    Binary(BinaryExprNode),
    Deref(DerefExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Cast(CastExprNode),
    Struct(StructExprNode),
    Selection(SelectionExprNode),
    Index(IndexExprNode),
    Instance(InstanceExprNode),
    Grouped(Box<ExprNode>),
}

impl ExprNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Ident(tok) => tok.pos,
            Self::Integer(tok) => tok.pos,
            Self::Frac(tok) => tok.pos,
            Self::Bool(tok) => tok.pos,
            Self::String(tok) => tok.pos,
            Self::Binary(node) => node.a.pos(),
            Self::Deref(node) => node.value.pos(),
            Self::Unary(node) => node.value.pos(),
            Self::Call(node) => node.callee.pos(),
            Self::Cast(node) => node.value.pos(),
            Self::Struct(node) => node.pos,
            Self::Selection(node) => node.value.pos(),
            Self::Index(node) => node.value.pos(),
            Self::Instance(node) => node.value.pos(),
            Self::Grouped(node) => node.pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DerefExprNode {
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExprNode {
    pub pos: Pos,
    pub callee: Box<ExprNode>,
    pub arguments: Vec<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub target: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructExprNode {
    pub pos: Pos,
    pub target: TypeExprNode,
    pub elements: Vec<KeyValue>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct KeyValue {
    pub pos: Pos,
    pub key: Token,
    pub value: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub selection: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IndexExprNode {
    pub value: Box<ExprNode>,
    pub indexes: Vec<ExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InstanceExprNode {
    pub value: Box<ExprNode>,
    pub args: Vec<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedExprNode {
    pub value: Box<ExprNode>,
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

#[derive(Debug, PartialEq, Eq)]
pub struct LetStatementNode {
    pub pos: Pos,
    pub name: Token,
    pub kind: LetKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LetKind {
    TypeOnly { ty: TypeExprNode },
    TypeValue { ty: TypeExprNode, value: ExprNode },
    ValueOnly { value: ExprNode },
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStatementNode {
    pub pos: Pos,
    pub receiver: ExprNode,
    pub value: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatementNode {
    pub pos: Pos,
    pub statements: Vec<StatementNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStatementNode {
    pub pos: Pos,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
    pub else_node: Option<Box<StatementNode>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileStatementNode {
    pub pos: Pos,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStatementNode {
    pub pos: Pos,
    pub value: Option<ExprNode>,
}
