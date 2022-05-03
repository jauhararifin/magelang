use std::collections::HashMap;

use crate::{
    ast,
    semantic::{Argument, ConcreteType, Field, FloatType, FnType, Header, IntType, Name, Struct, Type, TypePtr},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Error {
    EmptyPackage,
    MissingPackage,
    ImportCycle,
    RedeclaredSymbol,
    UndeclaredSymbol,
    UndeclaredField,
    UnresolvedType,
    MismatchType,
    CyclicType,
    NotAStruct,
    NotAFn,
    UnsupportedOperationInConstant,
    FnCallArgNumMismatch,
}

pub trait ITypeHelper<'a> {
    fn add_type(&mut self, name: &'a Name, typ: &Type);
    fn get(&self, typ: &ast::Type) -> Option<Type>;
    fn get_fn(&self, fn_header: &ast::FnHeader) -> Option<Type>;
    fn get_selector(&self, selector: &ast::Selector) -> Option<Type>;
    fn get_qual(&self, typ: &Name) -> Option<Type>;
    fn get_by_name(&self, name: &'a str) -> Option<Type>;

    fn get_i32(&self) -> Type;
    fn get_f64(&self) -> Type;
    fn get_bool(&self) -> Type;
    fn get_void(&self) -> Type;
}

// TODO: this TypeProcessor is so ugly, refactor it!!!
pub struct TypeHelper<'a> {
    package_name: String,
    dependencies: HashMap<&'a Name, &'a Type>,

    type_i8: Type,
    type_i16: Type,
    type_i32: Type,
    type_i64: Type,
    type_u8: Type,
    type_u16: Type,
    type_u32: Type,
    type_u64: Type,
    type_f32: Type,
    type_f64: Type,
    type_bool: Type,
    type_void: Type,

    type_alias: HashMap<&'a Name, Type>,
}

impl<'a> ITypeHelper<'a> for TypeHelper<'a> {
    fn get(&self, typ: &ast::Type) -> Option<Type> {
        self.get_type(typ)
    }

    fn get_qual(&self, name: &Name) -> Option<Type> {
        self.get_type_by_name(name)
    }

    fn get_by_name(&self, name: &'a str) -> Option<Type> {
        self.get_qual(&Name {
            package: self.package_name.clone(),
            name: String::from(name),
        })
    }

    fn add_type(&mut self, name: &'a Name, typ: &Type) {
        self.add_type_alias(name, typ)
    }

    fn get_fn(&self, fn_header: &ast::FnHeader) -> Option<Type> {
        self.get_fn_type(fn_header)
    }

    fn get_selector(&self, selector: &ast::Selector) -> Option<Type> {
        self.get_selector(selector)
    }

    fn get_i32(&self) -> Type {
        self.type_i32.clone()
    }

    fn get_f64(&self) -> Type {
        self.type_f64.clone()
    }

    fn get_bool(&self) -> Type {
        self.type_bool.clone()
    }

    fn get_void(&self) -> Type {
        self.type_void.clone()
    }
}

impl<'a> TypeHelper<'a> {
    pub fn empty(package_name: &'a str) -> Self {
        Self::new(String::from(package_name), HashMap::new())
    }

    pub fn from_headers(package_name: &'a str, headers: &'a [Header]) -> Self {
        Self::new(
            String::from(package_name),
            headers
                .iter()
                .flat_map(|h| &h.types)
                .map(|t| (&t.name, &t.typ))
                .collect(),
        )
    }

    fn new(package_name: String, dependencies: HashMap<&'a Name, &'a Type>) -> Self {
        Self {
            package_name,
            dependencies,

            type_i8: Type::from_concrete(ConcreteType::Int(IntType { signed: true, size: 8 })),
            type_i16: Type::from_concrete(ConcreteType::Int(IntType { signed: true, size: 16 })),
            type_i32: Type::from_concrete(ConcreteType::Int(IntType { signed: true, size: 32 })),
            type_i64: Type::from_concrete(ConcreteType::Int(IntType { signed: true, size: 64 })),
            type_u8: Type::from_concrete(ConcreteType::Int(IntType { signed: false, size: 8 })),
            type_u16: Type::from_concrete(ConcreteType::Int(IntType {
                signed: false,
                size: 16,
            })),
            type_u32: Type::from_concrete(ConcreteType::Int(IntType {
                signed: false,
                size: 32,
            })),
            type_u64: Type::from_concrete(ConcreteType::Int(IntType {
                signed: false,
                size: 64,
            })),
            type_f32: Type::from_concrete(ConcreteType::Float(FloatType { size: 32 })),
            type_f64: Type::from_concrete(ConcreteType::Float(FloatType { size: 64 })),
            type_bool: Type::from_concrete(ConcreteType::Bool),
            type_void: Type::from_concrete(ConcreteType::Void),

            type_alias: HashMap::new(),
        }
    }

    pub fn add_type_alias(&mut self, name: &'a Name, typ: &Type) {
        self.type_alias.insert(name, typ.clone());
    }

    pub fn get_type(&self, typ: &ast::Type) -> Option<Type> {
        match &typ {
            ast::Type::Primitive(token) => Some(self.get_type_from_primitive(token)),
            ast::Type::Ident(token) => self.get_type_from_ident(token),
            ast::Type::Struct(strct) => Some(self.get_struct(strct)),
            ast::Type::Selector(selector) => self.get_selector(selector),
        }
    }

    fn get_type_from_ident(&self, token: &Token) -> Option<Type> {
        // TODO: instead of cloning the package and name like this, can we borrow instead? or maybe
        // move the value temporarily?

        let name = Name {
            package: self.package_name.clone(),
            name: token.clone_value(),
        };
        self.type_alias.get(&name).map(|t| t.clone())
    }

    fn get_type_from_primitive(&self, token: &Token) -> Type {
        match &token.kind {
            TokenKind::Bool => self.type_bool.clone(),
            TokenKind::I8 => self.type_i8.clone(),
            TokenKind::I16 => self.type_i16.clone(),
            TokenKind::I32 => self.type_i32.clone(),
            TokenKind::I64 => self.type_i64.clone(),
            TokenKind::U8 => self.type_u8.clone(),
            TokenKind::U16 => self.type_u16.clone(),
            TokenKind::U32 => self.type_u32.clone(),
            TokenKind::U64 => self.type_u64.clone(),
            TokenKind::F32 => self.type_f32.clone(),
            TokenKind::F64 => self.type_f64.clone(),
            _ => unreachable!(),
        }
    }

    fn get_struct(&self, strct: &ast::Struct) -> Type {
        let mut fields: HashMap<String, Field> = HashMap::new();

        for (index, field) in strct.fields.iter().enumerate() {
            let name = field.name.unwrap_value();
            let typ = self.get_type(&field.typ);
            let typ = typ.map(|t| t.downgrade()).unwrap_or(TypePtr::new());
            fields.insert(name.clone(), Field { index, typ });
        }

        Type::from_concrete(ConcreteType::Struct(Struct { fields }))
    }

    pub fn get_selector(&self, selector: &ast::Selector) -> Option<Type> {
        let pkg_name = if let ast::ExprKind::Ident(pkg) = &selector.source.kind {
            pkg.unwrap_str()
        } else {
            return None;
        };

        let name = Name {
            package: String::from(pkg_name),
            name: String::from(selector.selection.unwrap_str()),
        };
        let dep_header = self.dependencies.get(&name);

        dep_header.map(|t| (*t).clone())
    }

    fn get_type_by_name(&self, name: &Name) -> Option<Type> {
        self.type_alias.get(name).cloned()
    }

    fn get_fn_type(&self, header: &ast::FnHeader) -> Option<Type> {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let typ = self.get_type(&arg.typ);
            if typ.is_none() {
                return None;
            }
            let typ = typ.unwrap();

            arguments.push(Argument {
                index,
                name: arg.name.unwrap_value().clone(),
                typ: typ.downgrade(),
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            if let Some(typ) = self.get_type(t) {
                Some(typ.downgrade())
            } else {
                None
            }
        } else {
            None
        };

        let native = header.native;

        Some(Type::from_concrete(ConcreteType::Fn(FnType {
            native,
            arguments,
            return_type,
        })))
    }
}
