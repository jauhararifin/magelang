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
}

#[derive(Debug, PartialEq, Eq)]
pub struct ImportNode {
    pub pos: Pos,
    pub name: Token,
    pub path: Token,
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
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalNode {
    pub pos: Pos,
    pub name: Token,
    pub ty: TypeExprNode,
    pub value: Option<ValueExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionNode {
    pub pos: Pos,
    pub signature: SignatureNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SignatureNode {
    pub pos: Pos,
    pub tags: Vec<TagNode>,
    pub name: Token,
    pub type_params: Vec<TypeParameterNode>,
    pub parameters: Vec<ParameterNode>,
    pub return_type: Option<TypeExprNode>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TagNode {
    pub pos: Pos,
    pub name: Token,
    pub arguments: Vec<Token>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParameterNode {
    pub pos: Pos,
    pub name: Token,
    pub ty: TypeExprNode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeParameterNode {
    pub name: Token,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeExprNode {}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueExprNode {}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementNode {
    Block(BlockStatementNode),
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockStatementNode {
    pub pos: Pos,
    pub statements: Vec<StatementNode>,
}
