use crate::token::Token;

#[derive(Debug, Clone)]
pub enum TypeNode {
    Empty,
    Primitive(Token),
    Array(ArrayTypeNode),
}

impl TypeNode {
    pub fn is_empty(&self) -> bool {
        if let TypeNode::Empty = self {
            true
        } else {
            false
        }
    }

    pub fn is_primitive(&self) -> bool {
        if let TypeNode::Primitive(_) = self {
            true
        } else {
            false
        }
    }

    pub fn try_unwrap_primitive(&self) -> Option<&Token> {
        if let TypeNode::Primitive(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn unwrap_primitive(&self) -> &Token {
        self.try_unwrap_primitive().unwrap()
    }

    pub fn is_array(&self) -> bool {
        if let TypeNode::Array(_) = self {
            true
        } else {
            false
        }
    }

    pub fn try_unwrap_array(&self) -> Option<&ArrayTypeNode> {
        if let TypeNode::Array(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn unwrap_array(&self) -> &ArrayTypeNode {
        self.try_unwrap_array().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode {
    pub open_brack: Token,
    pub elem: Box<TypeNode>,
}

