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
    pub name: Identifier,
    pub path: Token,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub value: String,
    pub pos: Pos,
}

impl From<Token> for Identifier {
    fn from(value: Token) -> Self {
        Self {
            value: value.value,
            pos: value.pos,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Identifier,
    pub type_params: Vec<TypeParameterNode>,
    pub fields: Vec<StructFieldNode>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructFieldNode {
    pub name: Identifier,
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalNode {
    pub pos: Pos,
    pub annotations: Vec<AnnotationNode>,
    pub name: Identifier,
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
    pub name: Identifier,
    pub type_params: Vec<TypeParameterNode>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<TypeExprNode>,
    pub end_pos: Pos,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AnnotationNode {
    pub pos: Pos,
    pub name: Identifier,
    pub arguments: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeParameterNode {
    pub name: Identifier,
}

impl From<Identifier> for TypeParameterNode {
    fn from(name: Identifier) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub pos: Pos,
    pub name: Identifier,
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeExprNode {
    Invalid(Pos),
    Path(PathNode),
    Ptr(PtrTypeNode),
    ArrayPtr(ArrayPtrTypeNode),
    Func(FuncTypeNode),
    Grouped(Box<TypeExprNode>),
}

impl TypeExprNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Invalid(pos) => *pos,
            Self::Path(node) => node.pos(),
            Self::Ptr(node) => node.pos,
            Self::ArrayPtr(node) => node.pos,
            Self::Func(node) => node.pos,
            Self::Grouped(node) => node.pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PathNode {
    // TODO: use type system to ensure the names is never empty
    pub names: Vec<Identifier>,
    pub args: Vec<TypeExprNode>,
}

impl PathNode {
    pub fn pos(&self) -> Pos {
        self.names.first().unwrap().pos
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PtrTypeNode {
    pub pos: Pos,
    pub ty: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayPtrTypeNode {
    pub pos: Pos,
    pub ty: Box<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncTypeNode {
    pub pos: Pos,
    pub params: Vec<TypeExprNode>,
    pub return_type: Option<Box<TypeExprNode>>,
    pub end_pos: Pos,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprNode {
    Path(PathNode),
    Number(Token),
    Null(Token),
    Bool(Token),
    Char(Token),
    String(Token),
    Binary(BinaryExprNode),
    Deref(DerefExprNode),
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Cast(CastExprNode),
    Struct(StructExprNode),
    Selection(SelectionExprNode),
    Index(IndexExprNode),
    Grouped(Box<ExprNode>),
}

impl ExprNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Number(tok) => tok.pos,
            Self::Null(tok) => tok.pos,
            Self::Bool(tok) => tok.pos,
            Self::Char(tok) => tok.pos,
            Self::String(tok) => tok.pos,
            Self::Binary(node) => node.a.pos(),
            Self::Deref(node) => node.value.pos(),
            Self::Unary(node) => node.value.pos(),
            Self::Call(node) => node.callee.pos(),
            Self::Cast(node) => node.value.pos(),
            Self::Struct(node) => node.pos,
            Self::Path(node) => node.pos(),
            Self::Selection(node) => node.value.pos(),
            Self::Index(node) => node.value.pos(),
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
    pub pos: Pos,
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
    pub key: Identifier,
    pub value: ExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub selection: Identifier,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IndexExprNode {
    pub value: Box<ExprNode>,
    pub index: Box<ExprNode>,
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
    Continue(Pos),
    Break(Pos),
    Return(ReturnStatementNode),
    Expr(ExprNode),
}

impl StatementNode {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Let(node) => node.pos,
            Self::Assign(node) => node.pos,
            Self::Block(node) => node.pos,
            Self::If(node) => node.pos,
            Self::While(node) => node.pos,
            Self::Continue(pos) => *pos,
            Self::Break(pos) => *pos,
            Self::Return(node) => node.pos,
            Self::Expr(node) => node.pos(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStatementNode {
    pub pos: Pos,
    pub name: Identifier,
    pub kind: LetKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum LetKind {
    Invalid,
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
