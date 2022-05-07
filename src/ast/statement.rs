use crate::{token::Token, pos::Pos};

use super::{TypeNode, ExprNode};

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

impl StatementNode {
    pub fn is_var(&self) -> bool {
        if let StatementNode::Var(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_assign(&self) -> bool {
        if let StatementNode::Assign(_) = self {
            true
        } else {
            false
        }
    }

    pub fn pos(&self) -> Pos {
        todo!();
    }
}

#[derive(Debug, Clone)]
pub struct VarNode {
    pub name: Token,
    pub typ: TypeNode,
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct AssignNode {
    pub receiver: ExprNode,
    pub op: Token,
    pub value: ExprNode,
}

#[derive(Debug, Clone)]
pub struct ReturnNode {
    pub return_token: Token,
    pub value: Option<ExprNode>,
}

#[derive(Debug, Clone)]
pub struct IfNode {
    pub if_token: Token,
    pub cond: ExprNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone)]
pub struct WhileNode {
    pub while_token: Token,
    pub cond: ExprNode,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone)]
pub struct BlockStatementNode {
    pub body: Vec<StatementNode>,
}
