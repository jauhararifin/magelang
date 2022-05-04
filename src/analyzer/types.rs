use std::collections::HashMap;

use crate::{
    ast::{self, Selector},
    semantic::{Argument, Field, FloatType, FnType, Header, IntType, Name, Ptr, Struct, Type, TypeKind},
    token::{Token, TokenKind},
};

pub struct AstContext<'a> {
    imports: HashMap<&'a str, &'a str>,
}

impl<'a> AstContext<'a> {
    pub fn new(root: &'a ast::Root) -> Self {
        let imports = root
            .imports
            .iter()
            .map(|import| (import.name.unwrap_str(), import.package_name.unwrap_str()))
            .collect();
        Self { imports }
    }
}

pub struct TypeHelper {
    package_name: String,
    types: HashMap<Name, Type>,
}

impl TypeHelper {
    pub fn empty(package_name: &str) -> Self {
        Self::new(String::from(package_name), HashMap::new())
    }

    pub fn from_headers(package_name: &str, headers: &[Header]) -> Self {
        Self::new(
            String::from(package_name),
            headers
                .iter()
                .flat_map(|h| &h.types)
                .filter(|t| t.name.is_some())
                .map(|t| (t.name.as_ref().unwrap().clone(), t.clone()))
                .collect(),
        )
    }

    fn new(package_name: String, types: HashMap<Name, Type>) -> Self {
        Self {
            package_name,
            types: types.into_iter().map(|(name, typ)| (name, typ)).collect(),
        }
    }
}

impl TypeHelper {
    pub fn add(&mut self, typ: Type) {
        if let Some(name) = &typ.name {
            self.types.insert(name.clone(), typ.clone());
        }
    }

    pub fn get(&self, ctx: &AstContext, typ: &ast::Type) -> TypeKind {
        match &typ {
            ast::Type::Primitive(token) => self.get_type_kind_from_primitive(token),
            ast::Type::Ident(token) => self.get_type_kind_from_ident(token),
            ast::Type::Struct(strct) => self.get_type_kind_from_struct(ctx, strct),
            ast::Type::Selector(selector) => self.get_type_kind_from_selector(ctx, selector),
        }
    }

    pub fn get_fn(&self, ctx: &AstContext, header: &ast::FnHeader) -> TypeKind {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let type_kind = self.get(ctx, &arg.typ);
            if type_kind.is_invalid() {
                return TypeKind::Invalid;
            }

            arguments.push(Argument {
                index,
                name: arg.name.unwrap_value().clone(),
                type_kind,
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            Some(Box::new(self.get(ctx, t)))
        } else {
            None
        };

        let native = header.native;

        TypeKind::Fn(FnType {
            native,
            arguments,
            return_type,
        })
    }

    pub fn get_by_name(&self, name: &Name) -> TypeKind {
        self.types.get(&name).map(|t| t.kind.clone()).unwrap_or(TypeKind::Invalid)
    }

    pub fn get_by_selector(&self, ctx: &AstContext, selector: &Selector) -> TypeKind {
        self.get_type_kind_from_selector(ctx, selector)
    }
}

impl TypeHelper {
    fn get_type_kind_from_ident(&self, token: &Token) -> TypeKind {
        let name = Name {
            package: self.package_name.clone(),
            name: token.clone_value(),
        };

        // TODO: return Type::Ptr here if we are inside a struct.
        self.types
            .get(&name)
            .map(|t| {
                TypeKind::Ptr(Ptr {
                    name: t.name.as_ref().unwrap().clone(),
                })
            })
            .unwrap_or(TypeKind::Invalid)
    }

    fn get_type_kind_from_primitive(&self, token: &Token) -> TypeKind {
        match &token.kind {
            TokenKind::I8 => TypeKind::Int(IntType::signed(8)),
            TokenKind::I16 => TypeKind::Int(IntType::signed(16)),
            TokenKind::I32 => TypeKind::Int(IntType::signed(32)),
            TokenKind::I64 => TypeKind::Int(IntType::signed(64)),
            TokenKind::U8 => TypeKind::Int(IntType::unsigned(8)),
            TokenKind::U16 => TypeKind::Int(IntType::unsigned(16)),
            TokenKind::U32 => TypeKind::Int(IntType::unsigned(32)),
            TokenKind::U64 => TypeKind::Int(IntType::unsigned(64)),
            TokenKind::F32 => TypeKind::Float(FloatType { size: 32 }),
            TokenKind::F64 => TypeKind::Float(FloatType { size: 64 }),
            TokenKind::Bool => TypeKind::Bool,
            _ => unreachable!(),
        }
    }

    fn get_type_kind_from_struct(&self, ctx: &AstContext, strct: &ast::Struct) -> TypeKind {
        let mut fields: HashMap<String, Field> = HashMap::new();

        for (index, field) in strct.fields.iter().enumerate() {
            let name = field.name.unwrap_value();
            let type_kind = self.get(ctx, &field.typ);
            fields.insert(name.clone(), Field { index, type_kind });
        }

        TypeKind::Struct(Struct { fields })
    }

    pub fn get_type_kind_from_selector(&self, ctx: &AstContext, selector: &ast::Selector) -> TypeKind {
        let pkg_id = if let ast::ExprKind::Ident(pkg) = &selector.source.kind {
            pkg.unwrap_str()
        } else {
            // TODO: this one should check if the `selector.source` is a struct.
            // don't just return Type::Invalid.
            // return TypeKind::Invalid;
            todo!();
        };

        let package_name = ctx.imports.get(pkg_id);
        let package_name = if let Some(package_name) = package_name {
            package_name
        } else {
            return TypeKind::Invalid;
        };

        let name = Name {
            package: String::from(*package_name),
            name: String::from(selector.selection.unwrap_str()),
        };
        let dep_header = self.types.get(&name);

        dep_header
            .map(|t| {
                TypeKind::Ptr(Ptr {
                    name: t.name.as_ref().unwrap().clone(),
                })
            })
            .unwrap_or(TypeKind::Invalid)
    }
}
