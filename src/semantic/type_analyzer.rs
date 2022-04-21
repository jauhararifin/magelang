use crate::token;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;

use super::cycle;
use super::error::Error;
use super::repr::{
    FloatType, FnType, IntType, Ptr, StructField, StructType, Type, TypeKind, Var,
};

pub struct TypeAnalyzer<'a> {
    root: &'a ast::Root,

    type_ast: HashMap<String, &'a ast::Type>,

    // map from type alias to its actual type.
    types: HashMap<String, Rc<Type>>,
    // map from global identifier to its type.
    global_types: HashMap<String, Rc<Type>>,

    pub bool_type: Rc<Type>,
    pub i8_type: Rc<Type>,
    pub i16_type: Rc<Type>,
    pub i32_type: Rc<Type>,
    pub i64_type: Rc<Type>,
    pub u8_type: Rc<Type>,
    pub u16_type: Rc<Type>,
    pub u32_type: Rc<Type>,
    pub u64_type: Rc<Type>,
    pub f32_type: Rc<Type>,
    pub f64_type: Rc<Type>,
}

impl<'a> TypeAnalyzer<'a> {
    pub fn new(root: &'a ast::Root) -> Self {
        Self {
            root,
            type_ast: HashMap::new(),

            types: HashMap::new(),
            global_types: HashMap::new(),

            bool_type: Rc::new(Type {
                kind: TypeKind::Bool,
                size: 1,
            }),
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

    // TODO: maybe accept a Token instead of string. and maybe return Result instead of Option.
    pub fn get_type(&self, identifier: &String) -> Option<Rc<Type>> {
        let typ = self.global_types.get(identifier);
        if let Some(typ) = typ {
            return Some(Rc::clone(typ));
        }

        None
    }

    pub fn get_type_from_token(&self, ident: &'a token::Token) -> Result<Rc<Type>, Error<'a>> {
        if let Some(typ) = self.get_type(ident.value.as_ref().unwrap()) {
            return Ok(Rc::clone(&typ));
        }

        Err(Error::UndefinedIdent { token: ident })
    }

    pub fn analyze(&mut self) -> Result<HashMap<String, Rc<Type>>, Error<'a>> {
        cycle::analyze(self.root)?;

        let mut type_to_analyze = Vec::new();

        for decl in self.root.declarations.iter() {
            match decl {
                ast::Declaration::Type(type_decl) => {
                    let name = type_decl.name.value.as_ref().unwrap();
                    self.type_ast.insert(name.clone(), &type_decl.typ);
                    type_to_analyze.push((name, &type_decl.typ));
                }
                ast::Declaration::Fn(fn_decl) => self.analyze_func(fn_decl)?,
                _ => unimplemented!(),
            }
        }

        let mut result = HashMap::<String, Rc<Type>>::new();
        for (name, typ) in type_to_analyze.into_iter() {
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

        let type_ast = *self.type_ast.get(name).unwrap();
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

    fn analyze_func(&mut self, func: &'a ast::FnDecl) -> Result<(), Error<'a>> {
        let name = func.name.value.as_ref().unwrap().clone();
        if self.global_types.contains_key(&name) || self.type_ast.contains_key(&name) {
            return Err(Error::RedeclaredSymbol { symbol: &func.name });
        }

        let typ = self.get_func_type(func);
        self.global_types.insert(name, typ);
        Ok(())
    }

    fn get_func_type(&mut self, func: &'a ast::FnDecl) -> Rc<Type> {
        let ret_type = if let Some(typ) = func.ret_type.as_ref() {
            self.analyze_type(typ)
        } else {
            Rc::new(Type {
                kind: TypeKind::Void,
                size: 0,
            })
        };

        let mut params = Vec::new();
        for param in func.param.iter() {
            let typ = self.analyze_type(&param.typ);
            params.push(Rc::new(Var {
                name: param.name.value.as_ref().unwrap().clone(),
                typ,
            }));
        }

        Rc::new(Type {
            kind: TypeKind::Fn(FnType { params, ret_type }),
            size: 8, // TODO: consider the arch
        })
    }
}
