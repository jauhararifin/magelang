use magelang_syntax::{
    BinaryExprNode, BinaryOp, CastExprNode, ExprNode, Number, UnaryExprNode, UnaryOp,
};

#[derive(Eq, PartialEq)]
pub(crate) enum Constant {
    Number(Number),
    Boolean(bool),
    Null,
}

pub(crate) fn get_const_from_node(node: &ExprNode) -> Option<Constant> {
    match node {
        ExprNode::Number(num_lit) => Some(Constant::Number(num_lit.value.clone())),
        ExprNode::Null(..) => Some(Constant::Null),
        ExprNode::Bool(token) => Some(Constant::Boolean(token.value)),
        ExprNode::Char(char_lit) => Some(Constant::Number(Number::from_int(char_lit.value as i32))),
        ExprNode::Binary(node) => get_const_from_binary_node(node),
        ExprNode::Unary(node) => get_const_from_unary_node(node),
        ExprNode::Cast(node) => get_const_from_cast_node(node),
        ExprNode::Grouped(node) => get_const_from_node(node),
        ExprNode::Path(..) => None,
        ExprNode::String(..) => None,
        ExprNode::Deref(..) => None,
        ExprNode::Call(..) => None,
        ExprNode::Struct(..) => None,
        ExprNode::Selection(..) => None,
        ExprNode::Index(..) => None,
    }
}

fn get_const_from_binary_node(node: &BinaryExprNode) -> Option<Constant> {
    let a = get_const_from_node(&node.a)?;
    let b = get_const_from_node(&node.b)?;

    match node.op {
        BinaryOp::Add => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Number(va + vb))
        }
        BinaryOp::Sub => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Number(va - vb))
        }
        BinaryOp::Mul => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Number(va * vb))
        }
        BinaryOp::Div => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Number(va / vb))
        }
        BinaryOp::Mod => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va & vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::BitOr => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va | vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::BitAnd => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va & vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::BitXor => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va ^ vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::ShiftLeft => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va << vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::ShiftRight => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            let Some(result) = va >> vb else {
                return None;
            };
            Some(Constant::Number(result))
        }
        BinaryOp::And => {
            let Constant::Boolean(va) = a else {
                return None;
            };
            let Constant::Boolean(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va && vb))
        }
        BinaryOp::Or => {
            let Constant::Boolean(va) = a else {
                return None;
            };
            let Constant::Boolean(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va || vb))
        }
        BinaryOp::Eq => Some(Constant::Boolean(a == b)),
        BinaryOp::NEq => Some(Constant::Boolean(a != b)),
        BinaryOp::Gt => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va > vb))
        }
        BinaryOp::GEq => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va >= vb))
        }
        BinaryOp::Lt => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va < vb))
        }
        BinaryOp::LEq => {
            let Constant::Number(va) = a else {
                return None;
            };
            let Constant::Number(vb) = b else {
                return None;
            };
            Some(Constant::Boolean(va <= vb))
        }
    }
}

fn get_const_from_unary_node(node: &UnaryExprNode) -> Option<Constant> {
    let v = get_const_from_node(&node.value)?;

    match node.op {
        UnaryOp::BitNot => {
            let Constant::Number(v) = v else {
                return None;
            };
            let Some(result) = !v else {
                // If !v fails, it means v is a not an integer,
                // but a fraction, and we don't allow bit manipulation
                // on float.
                return None;
            };
            Some(Constant::Number(result))
        }
        UnaryOp::Sub => {
            let Constant::Number(v) = v else {
                return None;
            };
            Some(Constant::Number(-v))
        }
        UnaryOp::Add => {
            let Constant::Number(v) = v else {
                return None;
            };
            Some(Constant::Number(v))
        }
        UnaryOp::Not => {
            let Constant::Boolean(v) = v else {
                return None;
            };
            Some(Constant::Boolean(!v))
        }
    }
}

pub(crate) fn get_const_from_cast_node(_: &CastExprNode) -> Option<Constant> {
    // TODO: casting should be able to be simulated during compile time
    None
}
