use crate::token;
use std::collections::HashMap;

use crate::ast;

use super::cycle;
use super::error::Error;
use super::semantic::{
    AssignStmt, BlockStmt, Def, ExprStmt, FloatType, FnDef, FnParam, FnType, IfStmt, IntType,
    Program, Ptr, ReturnStmt, Statement, StructField, StructType, Type, TypeKind, VarStmt,
    WhileStmt,
};

pub struct SimpleAnalyzer<'a> {
    type_to_ast: HashMap<&'a String, &'a ast::Declaration>,
    types: HashMap<&'a String, Type>,

    functions: Vec<FnDef>,
}

impl<'a> SimpleAnalyzer<'a> {
    pub fn new() -> Self {
        Self {
            type_to_ast: HashMap::new(),
            types: HashMap::new(),

            functions: Vec::new(),
        }
    }

    pub fn analyze(&mut self, root_ast: &'a ast::Root) -> Result<Program, Error<'a>> {
        self.analyze_types(root_ast)?;
        self.analyze_functions(root_ast)?;

        let mut definitions = HashMap::new();
        for (name, typ) in std::mem::replace(&mut self.types, HashMap::new()).into_iter() {
            definitions.insert(name.clone(), Def::TypeDef(typ));
        }
        for func in std::mem::replace(&mut self.functions, Vec::new()).into_iter() {
            definitions.insert(func.name.clone(), Def::FnDef(func));
        }

        Ok(Program { definitions })
    }

    fn analyze_functions(&mut self, root_ast: &'a ast::Root) -> Result<(), Error<'a>> {
        for decl in root_ast.declarations.iter() {
            if let ast::Declaration::Fn(fn_decl) = decl {
                let func = self.analyze_function(fn_decl)?;
                self.functions.push(func);
            }
        }

        Ok(())
    }

    fn analyze_function(&self, fn_decl: &'a ast::FnDecl) -> Result<FnDef, Error<'a>> {
        let name = fn_decl.name.value.as_ref().unwrap().clone();

        let ret_type = if let Some(typ) = fn_decl.ret_type.as_ref() {
            self.analyze_type(typ)
        } else {
            Type {
                kind: TypeKind::Void,
                size: 0,
            }
        };
        let ret_type = Box::new(ret_type);

        let mut params = Vec::new();
        for param in fn_decl.param.iter() {
            let typ = self.analyze_type(&param.typ);
            params.push(FnParam {
                name: param.name.value.as_ref().unwrap().clone(),
                typ,
            });
        }

        let fn_type = FnType { params, ret_type };
        let typ = Type {
            kind: TypeKind::Fn(fn_type),
            size: 8, // TODO: consider the architecture.
        };

        Ok(FnDef {
            name,
            typ,
            body: Statement::Block(BlockStmt { statements: vec![] }),
        })
    }

    fn analyze_statement(&self, stmt: &'a ast::Statement) -> Result<Statement, Error<'a>> {
        Ok(match stmt {
            ast::Statement::Block(s) => Statement::Block(self.analyze_block_statement(s)?),
            ast::Statement::Assign(s) => Statement::Assign(self.analyze_assign_statement(s)?),
            ast::Statement::If(s) => Statement::If(self.analyze_if_statement(s)?),
            ast::Statement::While(s) => Statement::While(self.analyze_while_statement(s)?),
            ast::Statement::Return(s) => Statement::Return(self.analyze_return_statement(s)?),
            ast::Statement::Expr(s) => Statement::Expr(self.analyze_expr_statement(s)?),
            ast::Statement::Var(s) => Statement::Var(self.analyze_var_statement(s)?),
        })
    }

    fn analyze_var_statement(&self, expr_stmt: &'a ast::Var) -> Result<VarStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_expr_statement(&self, expr_stmt: &'a ast::Expr) -> Result<ExprStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_return_statement(&self, ret_stmt: &'a ast::Return) -> Result<ReturnStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_while_statement(&self, while_stmt: &'a ast::While) -> Result<WhileStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_if_statement(&self, if_stmt: &'a ast::If) -> Result<IfStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_assign_statement(
        &self,
        assign_stmt: &'a ast::Assign,
    ) -> Result<AssignStmt, Error<'a>> {
        unimplemented!();
    }

    fn analyze_block_statement(
        &self,
        block_stmt: &'a ast::BlockStatement,
    ) -> Result<BlockStmt, Error<'a>> {
        let mut statements = Vec::new();
        for body in block_stmt.body.iter() {
            statements.push(self.analyze_statement(body)?);
        }

        Ok(BlockStmt { statements })
    }

    fn analyze_types(&mut self, root_ast: &'a ast::Root) -> Result<(), Error<'a>> {
        cycle::analyze(root_ast)?;

        for decl in root_ast.declarations.iter() {
            if let ast::Declaration::Type(type_decl) = decl {
                let name = &type_decl.name.value.as_ref().unwrap();
                self.type_to_ast.insert(name, decl);
            }
        }

        for name in self.type_to_ast.keys() {
            let typ = self.analyze_type_name(name);
            self.types.insert(name, typ);
        }

        Ok(())
    }

    fn analyze_type_name(&self, name: &'a String) -> Type {
        let typ = *self.type_to_ast.get(name).unwrap();
        if let ast::Declaration::Type(typ) = typ {
            let result = self.analyze_type(&typ.typ);
            return result;
        }
        panic!();
    }

    fn analyze_type(&self, typ: &'a ast::Type) -> Type {
        match typ {
            ast::Type::Primitive(primitive) => self.analyze_primitive(primitive),
            ast::Type::Ident(token) => self.analyze_type_name(token.value.as_ref().unwrap()),
            ast::Type::Struct(strct) => self.analyze_struct(strct),
            ast::Type::Pointer(ptr_type) => self.analyze_pointer_type(ptr_type),
        }
    }

    fn analyze_pointer_type(&self, pointer_type: &'a ast::Pointer) -> Type {
        Type {
            kind: TypeKind::Ptr(Ptr {
                elem: Box::new(self.analyze_type(&pointer_type.elem)),
            }),
            size: 8, // TODO: maybe check the architecture.
        }
    }

    fn analyze_struct(&self, strct: &'a ast::Struct) -> Type {
        let mut size = 0;
        let mut fields = Vec::new();
        let mut current_offset = 0;

        // TODO: add better offset alignment
        for param in strct.fields.iter() {
            let name = param.name.value.as_ref().unwrap().clone();
            fields.push(StructField {
                name,
                offset: current_offset,
                typ: self.analyze_type(&param.typ),
            });
            current_offset += fields.last().unwrap().typ.size;
            size += fields.last().unwrap().typ.size;
        }

        Type {
            kind: TypeKind::Struct(StructType { fields }),
            size,
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
