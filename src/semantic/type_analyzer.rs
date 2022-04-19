use crate::token;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;

use super::cycle;
use super::error::Error;
use super::semantic::{FloatType, IntType, Ptr, StructField, StructType, Type, TypeKind};

pub struct TypeAnalyzer<'a> {
    root: &'a ast::Root,
    type_to_ast: HashMap<String, &'a ast::Type>,
    types: HashMap<String, Rc<Type>>,

    bool_type: Rc<Type>,
    i8_type: Rc<Type>,
    i16_type: Rc<Type>,
    i32_type: Rc<Type>,
    i64_type: Rc<Type>,
    u8_type: Rc<Type>,
    u16_type: Rc<Type>,
    u32_type: Rc<Type>,
    u64_type: Rc<Type>,
    f32_type: Rc<Type>,
    f64_type: Rc<Type>,
}

impl<'a> TypeAnalyzer<'a> {
    pub fn new(root: &'a ast::Root) -> Self {
        Self {
            root,
            type_to_ast: HashMap::new(),
            types: HashMap::new(),

            bool_type: Self::int_type(false, 1),
            i8_type: Self::int_type(false, 1),
            i16_type: Self::int_type(false, 2),
            i32_type: Self::int_type(false, 4),
            i64_type: Self::int_type(false, 8),
            u8_type: Self::int_type(false, 1),
            u16_type: Self::int_type(false, 2),
            u32_type: Self::int_type(false, 4),
            u64_type: Self::int_type(false, 8),
            f32_type: Self::float_type(4),
            f64_type: Self::float_type(8),
        }
    }

    fn int_type(signed: bool, size: usize) -> Rc<Type> {
        Rc::new(Type {
            kind: TypeKind::Int(IntType { signed, size }),
            size,
        })
    }

    fn float_type(size: usize) -> Rc<Type> {
        Rc::new(Type {
            kind: TypeKind::Float(FloatType { size }),
            size,
        })
    }

    pub fn analyze(&mut self) -> Result<HashMap<String, Rc<Type>>, Error<'a>> {
        cycle::analyze(self.root)?;

        let mut to_analyze = Vec::new();

        for decl in self.root.declarations.iter() {
            if let ast::Declaration::Type(type_decl) = decl {
                let name = type_decl.name.value.as_ref().unwrap();
                self.type_to_ast.insert(name.clone(), &type_decl.typ);
                to_analyze.push((name, &type_decl.typ));
            }
        }

        let mut result = HashMap::<String, Rc<Type>>::new();
        for (name, typ) in to_analyze.into_iter() {
            let typ = self.analyze_type(typ);
            result.insert(name.clone(), typ);
        }

        Ok(result)
    }

    pub fn analyze_type(&mut self, typ: &'a ast::Type) -> Rc<Type> {
        match typ {
            ast::Type::Ident(token) => self.analyze_ident_type(token),
            ast::Type::Pointer(ptr_type) => self.analyze_pointer_type(ptr_type),
            ast::Type::Struct(strct) => self.analyze_struct(strct),
            ast::Type::Primitive(primitive) => self.analyze_primitive(primitive),
        }
    }

    fn analyze_ident_type(&mut self, ident_type: &'a token::Token) -> Rc<Type> {
        let name = ident_type.value.as_ref().unwrap();
        if let Some(typ) = self.types.get(name) {
            return Rc::clone(typ);
        }

        let type_ast = *self.type_to_ast.get(name).unwrap();
        let result = self.analyze_type(type_ast);
        self.types.insert(name.clone(), Rc::clone(&result));
        Rc::clone(&result)
    }

    fn analyze_pointer_type(&mut self, pointer_type: &'a ast::Pointer) -> Rc<Type> {
        Rc::new(Type {
            kind: TypeKind::Ptr(Ptr {
                elem: self.analyze_type(&pointer_type.elem),
            }),
            size: 8, // TODO: maybe check the architecture.
        })
    }

    fn analyze_struct(&mut self, strct: &'a ast::Struct) -> Rc<Type> {
        let mut size = 0;
        let mut fields = Vec::new();
        let mut offset = 0;

        // TODO: add better offset alignment
        for param in strct.fields.iter() {
            let name = param.name.value.as_ref().unwrap().clone();
            let typ = self.analyze_type(&param.typ);

            fields.push(StructField {
                name,
                offset,
                typ: Rc::clone(&typ),
            });
            offset += typ.size;
            size += typ.size;
        }

        Rc::new(Type {
            kind: TypeKind::Struct(StructType { fields }),
            size,
        })
    }

    fn analyze_primitive(&self, typ: &'a token::Token) -> Rc<Type> {
        match typ.kind {
            token::TokenKind::Bool => Rc::clone(&self.bool_type),
            token::TokenKind::I8 => Rc::clone(&self.i8_type),
            token::TokenKind::I16 => Rc::clone(&self.i16_type),
            token::TokenKind::I32 => Rc::clone(&self.i32_type),
            token::TokenKind::I64 => Rc::clone(&self.i64_type),
            token::TokenKind::U8 => Rc::clone(&self.u8_type),
            token::TokenKind::U16 => Rc::clone(&self.u16_type),
            token::TokenKind::U32 => Rc::clone(&self.u32_type),
            token::TokenKind::U64 => Rc::clone(&self.u64_type),
            token::TokenKind::F32 => Rc::clone(&self.f32_type),
            token::TokenKind::F64 => Rc::clone(&self.f64_type),
            _ => panic!("{:?} is not primitive kind", typ.kind),
        }
    }
}
