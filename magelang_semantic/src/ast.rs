use crate::error::ErrorAccumulator;
use magelang_syntax as syntax;
use magelang_syntax::{parse, Pos, TokenKind};
use std::fmt::Display;
use std::path::Path;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct PathId(usize);

impl From<usize> for PathId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<PathId> for usize {
    fn from(value: PathId) -> Self {
        value.0
    }
}

pub struct AstInfo {
    pub root: PackageNode,
    pub path: PathId,
    pub lines: Rc<[usize]>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Loc(PathId, Pos);

impl Loc {
    pub fn new(path: PathId, pos: Pos) -> Self {
        Self(path, pos)
    }
}

pub struct Location {
    pub path: Rc<Path>,
    pub line: usize,
    pub col: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{}:{}", self.path.as_os_str(), self.line, self.col)
    }
}

pub trait FromRawAst<T> {
    fn from_raw(value: T, path_id: PathId) -> Self;
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Rc<str>,
    pub loc: Loc,
}

impl FromRawAst<syntax::Token> for Token {
    fn from_raw(value: syntax::Token, path_id: PathId) -> Self {
        Self {
            kind: value.kind,
            value: value.value,
            loc: Loc::new(path_id, value.pos),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct PackageNode {
    pub items: Vec<Rc<ItemNode>>,
    pub comments: Vec<Token>,
}

impl FromRawAst<syntax::PackageNode> for PackageNode {
    fn from_raw(value: syntax::PackageNode, path_id: PathId) -> Self {
        Self {
            items: value
                .items
                .into_iter()
                .map(|node| Rc::new(ItemNode::from_raw(node, path_id)))
                .collect(),
            comments: value
                .comments
                .into_iter()
                .map(|t| Token::from_raw(t, path_id))
                .collect(),
        }
    }
}

impl PackageNode {
    pub fn imports(&self) -> impl Iterator<Item = &ImportNode> {
        self.items.iter().map(Rc::as_ref).filter_map(ItemNode::as_import)
    }
}

#[derive(PartialEq, Eq)]
pub enum ItemNode {
    Import(ImportNode),
    Struct(StructNode),
    Global(GlobalNode),
    Function(FunctionNode),
    NativeFunction(SignatureNode),
}

impl ItemNode {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Import(node) => node.loc,
            Self::Struct(node) => node.loc,
            Self::Global(node) => node.loc,
            Self::Function(node) => node.loc,
            Self::NativeFunction(node) => node.loc,
        }
    }
}

impl FromRawAst<syntax::ItemNode> for ItemNode {
    fn from_raw(value: syntax::ItemNode, path_id: PathId) -> Self {
        match value {
            syntax::ItemNode::Import(node) => Self::Import(ImportNode::from_raw(node, path_id)),
            syntax::ItemNode::Struct(node) => Self::Struct(StructNode::from_raw(node, path_id)),
            syntax::ItemNode::Global(node) => Self::Global(GlobalNode::from_raw(node, path_id)),
            syntax::ItemNode::Function(node) => Self::Function(FunctionNode::from_raw(node, path_id)),
            syntax::ItemNode::NativeFunction(node) => Self::NativeFunction(SignatureNode::from_raw(node, path_id)),
        }
    }
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

    pub fn is_native_function(&self) -> bool {
        self.as_native_function().is_some()
    }

    pub fn as_struct(&self) -> Option<&StructNode> {
        if let Self::Struct(node) = self {
            Some(node)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ImportNode {
    pub loc: Loc,
    pub name: Token,
    pub path: Token,
}

impl FromRawAst<syntax::ImportNode> for ImportNode {
    fn from_raw(value: syntax::ImportNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            path: Token::from_raw(value.path, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct StructNode {
    pub loc: Loc,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub fields: Vec<StructFieldNode>,
}

impl FromRawAst<syntax::StructNode> for StructNode {
    fn from_raw(value: syntax::StructNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            type_params: value
                .type_params
                .into_iter()
                .map(|t| TypeParameterNode::from_raw(t, path_id))
                .collect(),
            fields: value
                .fields
                .into_iter()
                .map(|s| StructFieldNode::from_raw(s, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct StructFieldNode {
    pub loc: Loc,
    pub name: Token,
    pub type_expr: ExprNode,
}

impl FromRawAst<syntax::StructFieldNode> for StructFieldNode {
    fn from_raw(value: syntax::StructFieldNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            type_expr: ExprNode::from_raw(value.type_expr, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct GlobalNode {
    pub loc: Loc,
    pub name: Token,
    pub ty: ExprNode,
    pub value: Option<ExprNode>,
}

impl FromRawAst<syntax::GlobalNode> for GlobalNode {
    fn from_raw(value: syntax::GlobalNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            ty: ExprNode::from_raw(value.ty, path_id),
            value: value.value.map(|e| ExprNode::from_raw(e, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct FunctionNode {
    pub loc: Loc,
    pub signature: SignatureNode,
    pub body: BlockStatementNode,
}

impl FromRawAst<syntax::FunctionNode> for FunctionNode {
    fn from_raw(value: syntax::FunctionNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            signature: SignatureNode::from_raw(value.signature, path_id),
            body: BlockStatementNode::from_raw(value.body, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct SignatureNode {
    pub loc: Loc,
    pub tags: Vec<TagNode>,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<ExprNode>,
}

impl FromRawAst<syntax::SignatureNode> for SignatureNode {
    fn from_raw(value: syntax::SignatureNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            tags: value.tags.into_iter().map(|t| TagNode::from_raw(t, path_id)).collect(),
            name: Token::from_raw(value.name, path_id),
            type_params: value
                .type_params
                .into_iter()
                .map(|t| TypeParameterNode::from_raw(t, path_id))
                .collect(),
            parameters: value
                .parameters
                .into_iter()
                .map(|p| ParameterNode::from_raw(p, path_id))
                .collect(),
            return_type: value.return_type.map(|e| ExprNode::from_raw(e, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct TagNode {
    pub loc: Loc,
    pub name: Token,
    pub arguments: Vec<Token>,
}

impl FromRawAst<syntax::TagNode> for TagNode {
    fn from_raw(value: syntax::TagNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            arguments: value
                .arguments
                .into_iter()
                .map(|t| Token::from_raw(t, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct TypeParameterNode {
    pub name: Token,
}

impl FromRawAst<syntax::TypeParameterNode> for TypeParameterNode {
    fn from_raw(value: syntax::TypeParameterNode, path_id: PathId) -> Self {
        Self {
            name: Token::from_raw(value.name, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ParameterNode {
    pub loc: Loc,
    pub name: Token,
    pub type_expr: ExprNode,
}

impl FromRawAst<syntax::ParameterNode> for ParameterNode {
    fn from_raw(value: syntax::ParameterNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            type_expr: ExprNode::from_raw(value.type_expr, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
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

impl StatementNode {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Let(node) => node.loc,
            Self::Assign(node) => node.loc,
            Self::Block(node) => node.loc,
            Self::If(node) => node.loc,
            Self::While(node) => node.loc,
            Self::Continue(node) => node.loc,
            Self::Break(node) => node.loc,
            Self::Return(node) => node.loc,
            Self::Expr(node) => node.get_loc(),
        }
    }
}

impl FromRawAst<syntax::StatementNode> for StatementNode {
    fn from_raw(value: syntax::StatementNode, path_id: PathId) -> Self {
        match value {
            syntax::StatementNode::Let(node) => Self::Let(LetStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::Assign(node) => Self::Assign(AssignStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::Block(node) => Self::Block(BlockStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::If(node) => Self::If(IfStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::While(node) => Self::While(WhileStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::Continue(node) => Self::Continue(Token::from_raw(node, path_id)),
            syntax::StatementNode::Break(node) => Self::Break(Token::from_raw(node, path_id)),
            syntax::StatementNode::Return(node) => Self::Return(ReturnStatementNode::from_raw(node, path_id)),
            syntax::StatementNode::Expr(node) => Self::Expr(ExprNode::from_raw(node, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct LetStatementNode {
    pub loc: Loc,
    pub name: Token,
    pub kind: LetKind,
}

impl FromRawAst<syntax::LetStatementNode> for LetStatementNode {
    fn from_raw(value: syntax::LetStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            name: Token::from_raw(value.name, path_id),
            kind: LetKind::from_raw(value.kind, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum LetKind {
    TypeOnly { ty: ExprNode },
    TypeValue { ty: ExprNode, value: ExprNode },
    ValueOnly { value: ExprNode },
}

impl FromRawAst<syntax::LetKind> for LetKind {
    fn from_raw(value: syntax::LetKind, path_id: PathId) -> Self {
        match value {
            syntax::LetKind::TypeOnly { ty } => Self::TypeOnly {
                ty: ExprNode::from_raw(ty, path_id),
            },
            syntax::LetKind::TypeValue { ty, value } => Self::TypeValue {
                ty: ExprNode::from_raw(ty, path_id),
                value: ExprNode::from_raw(value, path_id),
            },
            syntax::LetKind::ValueOnly { value } => Self::ValueOnly {
                value: ExprNode::from_raw(value, path_id),
            },
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct AssignStatementNode {
    pub loc: Loc,
    pub receiver: ExprNode,
    pub value: ExprNode,
}

impl FromRawAst<syntax::AssignStatementNode> for AssignStatementNode {
    fn from_raw(value: syntax::AssignStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            receiver: ExprNode::from_raw(value.receiver, path_id),
            value: ExprNode::from_raw(value.value, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct BlockStatementNode {
    pub loc: Loc,
    pub statements: Vec<StatementNode>,
}

impl FromRawAst<syntax::BlockStatementNode> for BlockStatementNode {
    fn from_raw(value: syntax::BlockStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            statements: value
                .statements
                .into_iter()
                .map(|s| StatementNode::from_raw(s, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct IfStatementNode {
    pub loc: Loc,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
    pub else_node: ElseNode,
}

impl FromRawAst<syntax::IfStatementNode> for IfStatementNode {
    fn from_raw(value: syntax::IfStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            condition: ExprNode::from_raw(value.condition, path_id),
            body: BlockStatementNode::from_raw(value.body, path_id),
            else_node: ElseNode::from_raw(value.else_node, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum ElseNode {
    None,
    ElseIf(Box<IfStatementNode>),
    Else(BlockStatementNode),
}

impl FromRawAst<syntax::ElseNode> for ElseNode {
    fn from_raw(value: syntax::ElseNode, path_id: PathId) -> Self {
        match value {
            syntax::ElseNode::None => Self::None,
            syntax::ElseNode::ElseIf(node) => Self::ElseIf(Box::new(IfStatementNode::from_raw(*node, path_id))),
            syntax::ElseNode::Else(node) => Self::Else(BlockStatementNode::from_raw(node, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ElseIfStatementNode {
    pub loc: Loc,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl FromRawAst<syntax::ElseIfStatementNode> for ElseIfStatementNode {
    fn from_raw(value: syntax::ElseIfStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            condition: ExprNode::from_raw(value.condition, path_id),
            body: BlockStatementNode::from_raw(value.body, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct WhileStatementNode {
    pub loc: Loc,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl FromRawAst<syntax::WhileStatementNode> for WhileStatementNode {
    fn from_raw(value: syntax::WhileStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            condition: ExprNode::from_raw(value.condition, path_id),
            body: BlockStatementNode::from_raw(value.body, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ReturnStatementNode {
    pub loc: Loc,
    pub value: Option<ExprNode>,
}

impl FromRawAst<syntax::ReturnStatementNode> for ReturnStatementNode {
    fn from_raw(value: syntax::ReturnStatementNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            value: value.value.map(|e| ExprNode::from_raw(e, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
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

impl ExprNode {
    pub fn get_loc(&self) -> Loc {
        match self {
            Self::Ident(node) => node.loc,
            Self::IntegerLiteral(node) => node.loc,
            Self::RealLiteral(node) => node.loc,
            Self::BooleanLit(node) => node.loc,
            Self::StringLit(node) => node.loc,
            Self::Binary(node) => node.a.get_loc(),
            Self::Deref(node) => node.loc,
            Self::Unary(node) => node.op.loc,
            Self::Call(node) => node.loc,
            Self::Cast(node) => node.value.get_loc(),
            Self::ArrayPtr(node) => node.loc,
            Self::StructLit(node) => node.loc,
            Self::Selection(node) => node.value.get_loc(),
            Self::Index(node) => node.value.get_loc(),
            Self::Grouped(node) => node.value.get_loc(),
        }
    }
}

impl FromRawAst<syntax::ExprNode> for ExprNode {
    fn from_raw(value: syntax::ExprNode, path_id: PathId) -> Self {
        match value {
            syntax::ExprNode::Ident(node) => Self::Ident(Token::from_raw(node, path_id)),
            syntax::ExprNode::IntegerLiteral(node) => Self::IntegerLiteral(Token::from_raw(node, path_id)),
            syntax::ExprNode::RealLiteral(node) => Self::RealLiteral(Token::from_raw(node, path_id)),
            syntax::ExprNode::BooleanLit(node) => Self::BooleanLit(Token::from_raw(node, path_id)),
            syntax::ExprNode::StringLit(node) => Self::StringLit(Token::from_raw(node, path_id)),
            syntax::ExprNode::Binary(node) => Self::Binary(BinaryExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Deref(node) => Self::Deref(DerefExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Unary(node) => Self::Unary(UnaryExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Call(node) => Self::Call(CallExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Cast(node) => Self::Cast(CastExprNode::from_raw(node, path_id)),
            syntax::ExprNode::ArrayPtr(node) => Self::ArrayPtr(ArrayPtrExprNode::from_raw(node, path_id)),
            syntax::ExprNode::StructLit(node) => Self::StructLit(StructLitNode::from_raw(node, path_id)),
            syntax::ExprNode::Selection(node) => Self::Selection(SelectionExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Index(node) => Self::Index(IndexExprNode::from_raw(node, path_id)),
            syntax::ExprNode::Grouped(node) => Self::Grouped(GroupedExprNode::from_raw(node, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct BinaryExprNode {
    pub a: Box<ExprNode>,
    pub op: Token,
    pub b: Box<ExprNode>,
}

impl FromRawAst<syntax::BinaryExprNode> for BinaryExprNode {
    fn from_raw(value: syntax::BinaryExprNode, path_id: PathId) -> Self {
        Self {
            a: Box::new(ExprNode::from_raw(*value.a, path_id)),
            op: Token::from_raw(value.op, path_id),
            b: Box::new(ExprNode::from_raw(*value.b, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct DerefExprNode {
    pub loc: Loc,
    pub value: Box<ExprNode>,
}

impl FromRawAst<syntax::DerefExprNode> for DerefExprNode {
    fn from_raw(value: syntax::DerefExprNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

impl FromRawAst<syntax::UnaryExprNode> for UnaryExprNode {
    fn from_raw(value: syntax::UnaryExprNode, path_id: PathId) -> Self {
        Self {
            op: Token::from_raw(value.op, path_id),
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct BuiltinCallExprNode {
    pub target: Token,
    pub arguments: Vec<ExprNode>,
}

impl FromRawAst<syntax::BuiltinCallExprNode> for BuiltinCallExprNode {
    fn from_raw(value: syntax::BuiltinCallExprNode, path_id: PathId) -> Self {
        Self {
            target: Token::from_raw(value.target, path_id),
            arguments: value
                .arguments
                .into_iter()
                .map(|e| ExprNode::from_raw(e, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct CallExprNode {
    pub loc: Loc,
    pub target: Box<ExprNode>,
    pub arguments: Vec<ExprNode>,
}

impl FromRawAst<syntax::CallExprNode> for CallExprNode {
    fn from_raw(value: syntax::CallExprNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            target: Box::new(ExprNode::from_raw(*value.target, path_id)),
            arguments: value
                .arguments
                .into_iter()
                .map(|e| ExprNode::from_raw(e, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub target: Box<ExprNode>,
}

impl FromRawAst<syntax::CastExprNode> for CastExprNode {
    fn from_raw(value: syntax::CastExprNode, path_id: PathId) -> Self {
        Self {
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
            target: Box::new(ExprNode::from_raw(*value.target, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct ArrayPtrExprNode {
    pub loc: Loc,
    pub element: Box<ExprNode>,
}

impl FromRawAst<syntax::ArrayPtrExprNode> for ArrayPtrExprNode {
    fn from_raw(value: syntax::ArrayPtrExprNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            element: Box::new(ExprNode::from_raw(*value.element, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct StructLitNode {
    pub loc: Loc,
    pub target: Box<ExprNode>,
    pub elements: Vec<KeyValue>,
}

impl FromRawAst<syntax::StructLitNode> for StructLitNode {
    fn from_raw(value: syntax::StructLitNode, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            target: Box::new(ExprNode::from_raw(*value.target, path_id)),
            elements: value
                .elements
                .into_iter()
                .map(|e| KeyValue::from_raw(e, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct KeyValue {
    pub loc: Loc,
    pub key: Token,
    pub value: Box<ExprNode>,
}

impl FromRawAst<syntax::KeyValue> for KeyValue {
    fn from_raw(value: syntax::KeyValue, path_id: PathId) -> Self {
        Self {
            loc: Loc::new(path_id, value.pos),
            key: Token::from_raw(value.key, path_id),
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub selection: Token,
}

impl FromRawAst<syntax::SelectionExprNode> for SelectionExprNode {
    fn from_raw(value: syntax::SelectionExprNode, path_id: PathId) -> Self {
        Self {
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
            selection: Token::from_raw(value.selection, path_id),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct IndexExprNode {
    pub value: Box<ExprNode>,
    pub index: Vec<ExprNode>,
}

impl FromRawAst<syntax::IndexExprNode> for IndexExprNode {
    fn from_raw(value: syntax::IndexExprNode, path_id: PathId) -> Self {
        Self {
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
            index: value
                .index
                .into_iter()
                .map(|e| ExprNode::from_raw(e, path_id))
                .collect(),
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct GroupedExprNode {
    pub value: Box<ExprNode>,
}

impl FromRawAst<syntax::GroupedExprNode> for GroupedExprNode {
    fn from_raw(value: syntax::GroupedExprNode, path_id: PathId) -> Self {
        Self {
            value: Box::new(ExprNode::from_raw(*value.value, path_id)),
        }
    }
}

pub trait AstDb: ErrorAccumulator {
    fn define_path(&self, path: Rc<Path>) -> PathId;
    fn get_path(&self, path_id: PathId) -> Rc<Path>;

    fn get_ast_by_path(&self, path_id: PathId) -> Rc<AstInfo>;

    fn get_location(&self, loc: Loc) -> Location {
        let ast_info = self.get_ast_by_path(loc.0);
        let path = self.get_path(ast_info.path);
        let offset: usize = loc.1.into();
        let partition = ast_info.lines.partition_point(|line| *line < offset);
        let line = partition + 1;
        let line_offset = if partition == 0 {
            0
        } else {
            ast_info
                .lines
                .get(partition-1)
                .or(ast_info.lines.last())
                .cloned()
                .unwrap_or_default()
        };
        let col = offset - line_offset;
        Location { path, line, col }
    }
}

pub fn get_ast_by_path(db: &impl AstDb, path_id: PathId) -> Rc<AstInfo> {
    let path = db.get_path(path_id);

    let text: Rc<[u8]> = match std::fs::read(&path) {
        Ok(content) => content.into(),
        Err(io_err) => {
            db.cannot_open_file(path_id, &io_err);
            Rc::new([])
        }
    };

    let mut lines = Vec::default();
    for (i, c) in text.iter().enumerate() {
        if *c == '\n' as u8 {
            lines.push(i);
        }
    }

    let parse_result = parse(&text);
    let root = PackageNode::from_raw(parse_result.root, path_id);

    Rc::new(AstInfo {
        root,
        path: path_id,
        lines: lines.into(),
    })
}
