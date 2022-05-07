use crate::token::Token;

use super::{statement::BlockStatementNode, TypeNode};

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
pub struct ParamNode {
    pub name: Token,
    pub typ: TypeNode,
}

