use std::rc::Rc;

use crate::{
    ast::{FnHeaderNode, TypeNode},
    semantic::{Argument, FloatType, FnType, IntType, Type},
    token::{Token, TokenKind},
};

pub struct TypeHelper();

impl TypeHelper {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeHelper {
    pub fn get(&self, typ: &TypeNode) -> Type {
        match &typ {
            TypeNode::Primitive(token) => self.get_type_kind_from_primitive(token),
        }
    }

    pub fn get_fn(&self, header: &FnHeaderNode) -> Type {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let type_kind = self.get(&arg.typ);
            arguments.push(Argument {
                index,
                name: arg.name.unwrap_value().clone(),
                type_kind,
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            Some(Rc::new(self.get(t)))
        } else {
            None
        };

        let native = header.native_token.is_some();

        Type::Fn(FnType {
            native,
            arguments,
            return_type,
        })
    }
}

impl TypeHelper {
    fn get_type_kind_from_primitive(&self, token: &Token) -> Type {
        match &token.kind {
            TokenKind::I8 => Type::Int(IntType::signed(8)),
            TokenKind::I16 => Type::Int(IntType::signed(16)),
            TokenKind::I32 => Type::Int(IntType::signed(32)),
            TokenKind::I64 => Type::Int(IntType::signed(64)),
            TokenKind::U8 => Type::Int(IntType::unsigned(8)),
            TokenKind::U16 => Type::Int(IntType::unsigned(16)),
            TokenKind::U32 => Type::Int(IntType::unsigned(32)),
            TokenKind::U64 => Type::Int(IntType::unsigned(64)),
            TokenKind::F32 => Type::Float(FloatType { size: 32 }),
            TokenKind::F64 => Type::Float(FloatType { size: 64 }),
            TokenKind::Bool => Type::Bool,
            _ => unreachable!(),
        }
    }
}
