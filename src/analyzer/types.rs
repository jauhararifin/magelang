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
    pub fn get(&self, typ: &TypeNode) -> Rc<Type> {
        match &typ {
            TypeNode::Primitive(token) => self.get_type_from_primitive(token),
        }
    }

    pub fn get_fn(&self, header: &FnHeaderNode) -> Rc<Type> {
        let arguments = header
            .params
            .iter()
            .enumerate()
            .map(|(index, arg)| Argument {
                index,
                name: arg.name.clone_value(),
                typ: self.get(&arg.typ),
            })
            .collect();

        let return_type = header.ret_type.as_ref().map(|t| self.get(t));
        let native = header.native_token.is_some();

        Rc::new(Type::Fn(Rc::new(FnType {
            native,
            arguments,
            return_type,
        })))
    }

    fn get_type_from_primitive(&self, token: &Token) -> Rc<Type> {
        Rc::new(match &token.kind {
            TokenKind::I8 => IntType::signed(8).into(),
            TokenKind::I16 => IntType::signed(16).into(),
            TokenKind::I32 => IntType::signed(32).into(),
            TokenKind::I64 => IntType::signed(64).into(),
            TokenKind::U8 => IntType::unsigned(8).into(),
            TokenKind::U16 => IntType::unsigned(16).into(),
            TokenKind::U32 => IntType::unsigned(32).into(),
            TokenKind::U64 => IntType::unsigned(64).into(),
            TokenKind::F32 => FloatType::new(32).into(),
            TokenKind::F64 => FloatType::new(64).into(),
            TokenKind::Bool => Type::Bool,
            _ => unreachable!(),
        })
    }
}
