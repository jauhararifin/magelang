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
    Block(BlockStatementNode),
    Return(ReturnStatementNode),
    Expr(ExprNode),
}

impl AstNode for StatementNode {
    fn get_span(&self) -> Span {
        match self {
            Self::Block(node) => node.get_span(),
            Self::Return(node) => node.get_span(),
            Self::Expr(node) => node.get_span(),
        }
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
    Call(CallExprNode),
}

impl AstNode for ExprNode {
    fn get_span(&self) -> Span {
        match self {
            Self::Ident(val) => val.span.clone(),
            Self::IntegerLiteral(val) => val.span.clone(),
            Self::Call(expr) => expr.span.clone(),
        }
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
