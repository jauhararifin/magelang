use crate::ast;
use crate::semantic::cycle;
use crate::token;

use super::{Error, Program, StructField, Type, TypeKind};
use super::{FloatType, IntType, Ptr, StructType};

pub struct SimpleAnalyzer {}

impl SimpleAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze<'a>(&mut self, root_ast: &'a ast::Root) -> Result<Program, Error<'a>> {
        self.analyze_types(root_ast)?;
        unimplemented!();
    }

    fn analyze_types<'a>(&mut self, root_ast: &'a ast::Root) -> Result<(), Error<'a>> {
        cycle::analyze(root_ast)?;

        for decl in root_ast.declarations.iter() {}

        unimplemented!();
    }

    fn analyze_declaration(&self, decl: &ast::Declaration) {
        match decl {
            ast::Declaration::Fn(fn_decl) => unimplemented!("func is not supported yet"),
            ast::Declaration::Var(var_decl) => unimplemented!("var is not supported yet"),
            ast::Declaration::Type(type_decl) => unimplemented!("type is not supported yet"),
        }
    }

    fn analyze_type_decl(&self, type_decl: &ast::TypeDecl) {
        unimplemented!();
    }

    fn analyze_type(&self, typ: &ast::Type) -> Type {
        match typ {
            ast::Type::Primitive(primitive) => self.analyze_primitive(primitive),
            ast::Type::Ident(_) => unimplemented!("ident type is not supported yet"),
            ast::Type::Struct(strct) => self.analyze_struct(strct),
            ast::Type::Pointer(ptr_type) => self.analyze_pointer_type(ptr_type),
        }
    }

    fn analyze_pointer_type(&self, pointer_type: &ast::Pointer) -> Type {
        Type {
            kind: TypeKind::Ptr(Ptr {
                elem: Box::new(self.analyze_type(&pointer_type.elem)),
            }),
            size: 8, // TODO: maybe check the architecture.
        }
    }

    fn analyze_struct(&self, strct: &ast::Struct) -> Type {
        let mut size = 0;
        let mut fields = Vec::new();
        let mut current_offset = 0;

        // TODO: add better offset alignment
        for param in strct.fields.iter() {
            let name = if let token::TokenKind::Ident(name) = &param.name.kind {
                name.clone()
            } else {
                panic!();
            };

            fields.push(StructField {
                name,
                offset: current_offset,
                typ: self.analyze_type(&param.typ),
            });
            current_offset += 8;
            size += fields.last().unwrap().typ.size;
        }

        Type {
            kind: TypeKind::Struct(StructType { fields: vec![] }),
            size: size,
        }
    }

    fn analyze_primitive(&self, typ: &token::Token) -> Type {
        match typ.kind {
            token::TokenKind::Bool => Type {
                kind: TypeKind::Int(IntType {
                    signed: true,
                    size: 1,
                }),
                size: 1,
            },
            token::TokenKind::I8 => Type {
                kind: TypeKind::Int(IntType {
                    signed: true,
                    size: 1,
                }),
                size: 1,
            },
            token::TokenKind::I16 => Type {
                kind: TypeKind::Int(IntType {
                    signed: true,
                    size: 2,
                }),
                size: 2,
            },
            token::TokenKind::I32 => Type {
                kind: TypeKind::Int(IntType {
                    signed: true,
                    size: 4,
                }),
                size: 4,
            },
            token::TokenKind::I64 => Type {
                kind: TypeKind::Int(IntType {
                    signed: true,
                    size: 8,
                }),
                size: 8,
            },
            token::TokenKind::U8 => Type {
                kind: TypeKind::Int(IntType {
                    signed: false,
                    size: 1,
                }),
                size: 1,
            },
            token::TokenKind::U16 => Type {
                kind: TypeKind::Int(IntType {
                    signed: false,
                    size: 2,
                }),
                size: 2,
            },
            token::TokenKind::U32 => Type {
                kind: TypeKind::Int(IntType {
                    signed: false,
                    size: 4,
                }),
                size: 4,
            },
            token::TokenKind::U64 => Type {
                kind: TypeKind::Int(IntType {
                    signed: false,
                    size: 8,
                }),
                size: 8,
            },
            token::TokenKind::F32 => Type {
                kind: TypeKind::Float(FloatType { size: 4 }),
                size: 4,
            },
            token::TokenKind::F64 => Type {
                kind: TypeKind::Float(FloatType { size: 1 }),
                size: 8,
            },
            _ => panic!("{:?} is not primitive kind", typ.kind),
        }
    }
}
