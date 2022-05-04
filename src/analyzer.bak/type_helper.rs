use std::collections::HashMap;

use crate::{
    ast,
    semantic::{Argument, Field, FloatType, FnType, Header, IntType, Name, Ptr, Struct, Type, TypeKind},
    token::{Token, TokenKind},
};

pub struct AstContext<'a> {
    imports: HashMap<&'a str, &'a str>, // map from the package identifier to it's actual name.
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
//
// impl<'a> ITypeHelper<'a> for TypeHelper<'a> {
//     fn add_type(&mut self, name: Name, typ: &Type) {
//         self.add_type_alias(name, typ)
//     }
//
//     fn get(&self, ctx: &TypeContext, typ: &ast::Type) -> Type {
//         self.get_type(ctx, typ)
//     }
//
//     fn get_qual(&self, ctx: &TypeContext, name: &Name) -> Type {
//         self.get_type_by_name(ctx, name)
//     }
//
//     fn get_by_name(&self, ctx: &TypeContext, name: &'a str) -> Type {
//         self.get_qual(
//             ctx,
//             &Name {
//                 package: self.package_name.clone(),
//                 name: String::from(name),
//             },
//         )
//     }
//
//     fn get_fn(&self, ctx: &TypeContext, fn_header: &ast::FnHeader) -> Type {
//         self.get_fn_type(ctx, fn_header)
//     }
//
//     fn get_selector(&self, ctx: &TypeContext, selector: &ast::Selector) -> Type {
//         self.get_selector(ctx, selector)
//     }
//
//     fn get_i32(&self) -> Type {
//         self.type_i32.clone()
//     }
//
//     fn get_f64(&self) -> Type {
//         self.type_f64.clone()
//     }
//
//     fn get_bool(&self) -> Type {
//         self.type_bool.clone()
//     }
//
//     fn get_void(&self) -> Type {
//         self.type_void.clone()
//     }
// }

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
                .map(|t| (t.name.unwrap().clone(), t.clone()))
                .collect(),
        )
    }

    fn new(package_name: String, types: HashMap<Name, Type>) -> Self {
        Self { package_name, types }
    }

    pub fn add_type_alias(&mut self, name: Name, typ: &Type) {
        self.type_alias.insert(name.clone(), typ.clone());
        self.type_alias_id.insert(name, self.type_alias_last_id);
        self.type_alias_last_id += 1;
    }

    pub fn get_type(&self, ctx: &AstContext, typ: &ast::Type) -> Type {
        match &typ {
            ast::Type::Primitive(token) => self.get_type_from_primitive(token),
            ast::Type::Ident(token) => self.get_type_from_ident(token),
            ast::Type::Struct(strct) => self.get_struct(ctx, strct),
            ast::Type::Selector(selector) => self.get_selector(ctx, selector),
        }
    }

    fn get_type_kind(&self, ctx: &AstContext, typ: &ast::Type) -> TypeKind {
        match &typ {
            ast::Type::Primitive(token) => self.get_type_kind_from_primitive(token),
            ast::Type::Ident(token) => self.get_type_from_ident(token),
            ast::Type::Struct(strct) => self.get_struct(ctx, strct),
            ast::Type::Selector(selector) => self.get_selector(ctx, selector),
        }
    }

    fn get_type_from_ident(&self, token: &Token) -> Type {
        let name = Name {
            package: self.package_name.clone(),
            name: token.clone_value(),
        };
        // TODO: return Type::Ptr here if we are inside a struct.
        self.type_alias.get(&name).map(|t| t.clone()).unwrap_or(Type::invalid())
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

    fn get_struct_kind(&self, ctx: &AstContext, strct: &ast::Struct) -> TypeKind {
        let mut fields: HashMap<String, Field> = HashMap::new();

        for (index, field) in strct.fields.iter().enumerate() {
            let name = field.name.unwrap_value();
            let typ = self.get_type(ctx, &field.typ);
            fields.insert(name.clone(), Field { index, typ });
        }

        TypeKind::Struct(Struct { fields })
    }

    pub fn get_selector(&self, ctx: &AstContext, selector: &ast::Selector) -> Type {
        let pkg_id = if let ast::ExprKind::Ident(pkg) = &selector.source.kind {
            pkg.unwrap_str()
        } else {
            // TODO: this one should check if the `selector.source` is a struct.
            // don't just return Type::Invalid.
            return Type::Invalid;
        };

        let package_name = ctx.imports.get(pkg_id);
        let package_name = if let Some(package_name) = package_name {
            package_name
        } else {
            return Type::Invalid;
        };

        let name = Name {
            package: String::from(*package_name),
            name: String::from(selector.selection.unwrap_str()),
        };
        let dep_header = self.dependencies.get(&name);

        dep_header.map(|t| (*t).clone()).unwrap_or(Type::Invalid)
    }

    pub fn get_selector_kind(&self, ctx: &AstContext, selector: &ast::Selector) -> TypeKind {
        let pkg_id = if let ast::ExprKind::Ident(pkg) = &selector.source.kind {
            pkg.unwrap_str()
        } else {
            // TODO: this one should check if the `selector.source` is a struct.
            // don't just return Type::Invalid.
            return TypeKind::Invalid;
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
        let dep_header = self.dependencies.get(&name);
        if dep_header.is_none() {
            return TypeKind::Invalid;
        }

        TypeKind::Ptr(Ptr { name })
    }

    fn get_type_by_name(&self, _: &AstContext, name: &Name) -> Type {
        self.type_alias.get(name).map(|t| t.clone()).unwrap_or(Type::invalid())
    }

    fn get_fn_type(&self, ctx: &AstContext, header: &ast::FnHeader) -> Type {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let typ = self.get_type(ctx, &arg.typ);
            if typ.is_invalid() {
                return Type::Invalid;
            }

            arguments.push(Argument {
                index,
                name: arg.name.unwrap_value().clone(),
                typ,
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            Some(Box::new(self.get_type(ctx, t)))
        } else {
            None
        };

        let native = header.native;

        Type::Fn(FnType {
            native,
            arguments,
            return_type,
        })
    }
}
