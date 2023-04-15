use crate::tokens::Token;
use indexmap::IndexMap;
use magelang_common::{Span, SymbolId};

pub trait AstNode {
    fn get_span(&self) -> Span;
}

#[derive(Debug, PartialEq, Eq)]
pub struct PackageNode {
    pub span: Span,
    pub items: IndexMap<SymbolId, Vec<ItemNode>>,
    pub comments: Vec<Token>,
}

impl AstNode for PackageNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

impl PackageNode {
    pub fn imports(&self) -> impl Iterator<Item = &ImportNode> {
        self.items
            .values()
            .flat_map(|v| v.iter())
            .filter_map(ItemNode::as_import)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemNode {
    Import(ImportNode),
    Function(FunctionNode),
    NativeFunction(SignatureNode),
}

impl ItemNode {
    pub fn name(&self) -> &str {
        match self {
            Self::Import(node) => &node.name.value,
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
}

impl AstNode for ItemNode {
    fn get_span(&self) -> Span {
        match self {
            Self::Import(node) => node.get_span(),
            Self::Function(node) => node.get_span(),
            Self::NativeFunction(node) => node.get_span(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportNode {
    pub span: Span,
    pub name: Token,
    pub path: Token,
}

impl AstNode for ImportNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionNode {
    pub span: Span,
    pub signature: SignatureNode,
    pub body: BlockStatementNode,
}

impl AstNode for FunctionNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SignatureNode {
    pub span: Span,
    pub name: Token,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<ExprNode>,
}

impl AstNode for SignatureNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub span: Span,
    pub name: Token,
    pub type_expr: ExprNode,
}

impl AstNode for ParameterNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementNode {
    Let(LetStatementNode),
    Assign(AssignStatementNode),
    Block(BlockStatementNode),
    If(IfStatementNode),
    While(WhileStatementNode),
    Return(ReturnStatementNode),
    Expr(ExprNode),
}

impl AstNode for StatementNode {
    fn get_span(&self) -> Span {
        match self {
            Self::Let(node) => node.get_span(),
            Self::Assign(node) => node.get_span(),
            Self::Block(node) => node.get_span(),
            Self::If(node) => node.get_span(),
            Self::While(node) => node.get_span(),
            Self::Return(node) => node.get_span(),
            Self::Expr(node) => node.get_span(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LetStatementNode {
    pub span: Span,
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
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssignStatementNode {
    pub span: Span,
    pub receiver: ExprNode,
    pub value: ExprNode,
}

impl AstNode for AssignStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatementNode {
    pub span: Span,
    pub statements: Vec<StatementNode>,
}

impl AstNode for BlockStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfStatementNode {
    pub span: Span,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
    pub else_ifs: Vec<ElseIfStatementNode>,
    pub else_body: Option<BlockStatementNode>,
}

impl AstNode for IfStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ElseIfStatementNode {
    pub span: Span,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl AstNode for ElseIfStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct WhileStatementNode {
    pub span: Span,
    pub condition: ExprNode,
    pub body: BlockStatementNode,
}

impl AstNode for WhileStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReturnStatementNode {
    pub span: Span,
    pub value: Option<ExprNode>,
}

impl AstNode for ReturnStatementNode {
    fn get_span(&self) -> Span {
        self.span.clone()
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
    Unary(UnaryExprNode),
    Call(CallExprNode),
    Cast(CastExprNode),
    Selection(SelectionExprNode),
    Grouped(GroupedExprNode),
}

impl AstNode for ExprNode {
    fn get_span(&self) -> Span {
        match self {
            Self::Ident(val) => val.span.clone(),
            Self::IntegerLiteral(val) => val.span.clone(),
            Self::RealLiteral(val) => val.span.clone(),
            Self::BooleanLit(val) => val.span.clone(),
            Self::StringLit(val) => val.span.clone(),
            Self::Binary(val) => val.get_span(),
            Self::Unary(val) => val.get_span(),
            Self::Call(expr) => expr.span.clone(),
            Self::Cast(val) => val.get_span(),
            Self::Selection(val) => val.get_span(),
            Self::Grouped(val) => val.get_span(),
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
    fn get_span(&self) -> Span {
        let mut s = self.a.get_span();
        s.union(&self.b.get_span());
        s
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnaryExprNode {
    pub op: Token,
    pub value: Box<ExprNode>,
}

impl AstNode for UnaryExprNode {
    fn get_span(&self) -> Span {
        let mut s = self.op.span.clone();
        s.union(&self.value.get_span());
        s
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallExprNode {
    pub span: Span,
    pub target: Box<ExprNode>,
    pub arguments: Vec<ExprNode>,
}

impl AstNode for CallExprNode {
    fn get_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CastExprNode {
    pub value: Box<ExprNode>,
    pub target: Box<ExprNode>,
}

impl AstNode for CastExprNode {
    fn get_span(&self) -> Span {
        let mut s = self.value.get_span();
        s.union(&self.target.get_span());
        s
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct SelectionExprNode {
    pub value: Box<ExprNode>,
    pub selection: Token,
}

impl AstNode for SelectionExprNode {
    fn get_span(&self) -> Span {
        let mut s = self.value.get_span();
        s.union(&self.selection.span);
        s
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct GroupedExprNode {
    pub value: Box<ExprNode>,
}

impl AstNode for GroupedExprNode {
    fn get_span(&self) -> Span {
        self.value.get_span()
    }
}
