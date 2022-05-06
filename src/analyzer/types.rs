use std::rc::Rc;

use crate::{
    ast::{ArrayTypeNode, FnHeaderNode, TypeNode},
    errors::Error,
    semantic::{Argument, ArrayType, FloatType, FnType, IntType, Type},
    token::{Token, TokenKind},
};

pub struct TypeHelper();

impl TypeHelper {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeHelper {
    pub fn get(&self, typ: &TypeNode) -> Result<Rc<Type>, Error> {
        match &typ {
            TypeNode::Primitive(token) => self.get_type_from_primitive(token),
            TypeNode::Array(array_type) => self.get_type_from_array(array_type),
        }
    }

    pub fn get_fn(&self, header: &FnHeaderNode) -> Result<Rc<Type>, Error> {
        let mut arguments = Vec::new();
        for (index, arg) in header.params.iter().enumerate() {
            arguments.push(Argument {
                index,
                name: arg.name.clone_value(),
                typ: self.get(&arg.typ)?,
            });
        }

        let return_type = if let Some(typ) = header.ret_type.as_ref() {
            Some(self.get(typ)?)
        } else {
            None
        };

        let native = header.native_token.is_some();

        Ok(Rc::new(Type::Fn(Rc::new(FnType {
            native,
            arguments,
            return_type,
        }))))
    }

    fn get_type_from_primitive(&self, token: &Token) -> Result<Rc<Type>, Error> {
        Ok(Rc::new(match &token.kind {
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
        }))
    }

    fn get_type_from_array(&self, array_type: &ArrayTypeNode) -> Result<Rc<Type>, Error> {
        let size: usize = array_type
            .size
            .unwrap_value()
            .parse()
            .map_err(|err| Error::InvalidIntLit {
                token: array_type.size.clone(),
                err,
            })?;

        let elem_type = self.get(array_type.elem.as_ref())?;

        Ok(Rc::new(Type::Array(Rc::new(ArrayType { size, elem_type }))))
    }
}
