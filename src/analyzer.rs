use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::{Rc, Weak},
};

use crate::{
    ast::{self, Expr, ExprKind, FnHeader, TypeDecl},
    parser,
    pos::Pos,
    semantic::{Argument, Field, Root, Type, TypePtr, BOOL, F32, F64, I16, I32, I64, I8, U16, U32, U64, U8},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub enum Error {
    ParseError(parser::Error),
    CyclicTypeDef(Vec<String>),
    RedeclaredSymbol(Token),
    UndeclaredSymbol(Token),
    UndeclaredType, // TODO: put position.
    CannotAssignToFunction(Token),
    CannotAssignTo(Type, Pos),
    HasNoField(Type, Pos),
    UndefinedField { typ: Type, field: Token },
    MismatchType { expected: Type, got: Type, pos: Pos },
    ReturnOnVoidFunc(Pos),
    MissingReturnValue(Pos),
    UnsupportedOperationInConstant(Pos),
    MissingMain,
}

pub trait Analyzer {
    fn analyze(&mut self) -> Result<Root, Error>;
}

pub struct SimpleAnalyzer {
    root_ast: ast::Root,
}

impl SimpleAnalyzer {
    pub fn new(root_ast: ast::Root) -> Self {
        Self { root_ast }
    }
}

impl Analyzer for SimpleAnalyzer {
    fn analyze(&mut self) -> Result<Root, Error> {
        let mut type_processor = TypeProcessor::new(&self.root_ast);
        type_processor.setup()?;

        let mut value_processor = ValueProcessor::new(&self.root_ast, &type_processor);
        value_processor.setup()?;

        todo!();
    }
}

struct TypeProcessor<'a> {
    root_ast: &'a ast::Root,
    type_alias: HashMap<&'a String, Rc<Type>>,

    type_i8: Rc<Type>,
    type_i16: Rc<Type>,
    type_i32: Rc<Type>,
    type_i64: Rc<Type>,
    type_u8: Rc<Type>,
    type_u16: Rc<Type>,
    type_u32: Rc<Type>,
    type_u64: Rc<Type>,
    type_f32: Rc<Type>,
    type_f64: Rc<Type>,
    type_bool: Rc<Type>,
}

impl<'a> TypeProcessor<'a> {
    pub fn new(root_ast: &'a ast::Root) -> Self {
        Self {
            root_ast,
            type_alias: HashMap::new(),

            type_i8: Rc::new(I8),
            type_i16: Rc::new(I16),
            type_i32: Rc::new(I32),
            type_i64: Rc::new(I64),
            type_u8: Rc::new(U8),
            type_u16: Rc::new(U16),
            type_u32: Rc::new(U32),
            type_u64: Rc::new(U64),
            type_f32: Rc::new(F32),
            type_f64: Rc::new(F64),
            type_bool: Rc::new(BOOL),
        }
    }

    fn setup(&mut self) -> Result<(), Error> {
        let plan = self.build_setup_plan()?;
        self.setup_types(&plan);
        println!("types: {:?}", self.type_alias); // TODO: REMOVE
        Ok(())
    }

    fn build_setup_plan(&mut self) -> Result<Vec<String>, Error> {
        let type_decls: Vec<&ast::TypeDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Type(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        let mut name_to_type = HashMap::new();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap().clone();
            if name_to_type.contains_key(&name) {
                return Err(Error::RedeclaredSymbol(type_decl.name.clone()));
            }
            name_to_type.insert(name, type_decl.typ.clone());
        }

        let mut plan: Vec<String> = vec![];
        let mut visited = HashSet::new();

        for type_decl in type_decls.iter() {
            if visited.contains(type_decl.name.value.as_ref().unwrap()) {
                continue;
            }

            let chain = self.traverse_type_alias(&name_to_type, *type_decl)?;

            for item in chain.into_iter().rev() {
                if !visited.contains(&item) {
                    plan.push(item.clone());
                    visited.insert(item);
                }
            }
        }

        Ok(plan)
    }

    fn traverse_type_alias(
        &self,
        name_to_type: &HashMap<String, ast::Type>,
        type_decl: &TypeDecl,
    ) -> Result<Vec<String>, Error> {
        let mut chain: Vec<String> = vec![];
        let mut chain_set = HashSet::new();

        let mut current_type = &type_decl.name;
        loop {
            let name = current_type.value.as_ref().unwrap();

            if chain_set.contains(name) {
                let i = chain.iter().position(|v: &String| v == name).unwrap();
                let result = chain.into_iter().skip(i).map(|s| s.clone()).collect();
                return Err(Error::CyclicTypeDef(result));
            }

            chain.push(name.clone());
            chain_set.insert(name);

            let current_type_decl = name_to_type
                .get(name)
                .ok_or(Error::UndeclaredSymbol(current_type.clone()))?;

            match &current_type_decl {
                ast::Type::Primitive(_) | ast::Type::Struct(_) => break,
                ast::Type::Ident(token) => current_type = token,
            }
        }

        Ok(chain)
    }

    fn setup_types(&mut self, plan: &Vec<String>) {
        let mut type_decls: HashMap<&String, &ast::TypeDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Type(t) = decl {
                    let name = t.name.value.as_ref().unwrap();
                    Some((name, t))
                } else {
                    None
                }
            })
            .collect();
        let type_decls: Vec<&ast::TypeDecl> = plan.iter().map(|v| type_decls.remove(v).unwrap()).collect();

        self.populate_types(&type_decls);
        self.repopulate_struct_fields(&type_decls);
    }

    fn populate_types(&mut self, type_decls: &Vec<&'a ast::TypeDecl>) {
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            let typ = self.get_type(&type_decl.typ).unwrap();
            self.type_alias.insert(name, typ);
        }
    }

    fn repopulate_struct_fields(&self, type_decls: &Vec<&ast::TypeDecl>) {
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap().clone();
            let ast_type = &type_decl.typ;
            let typ = self.type_alias.borrow().get(&name).unwrap().clone();

            let ast_fields = if let ast::Type::Struct(t) = ast_type {
                &t.fields
            } else {
                continue;
            };

            let fields = if let Type::Struct { fields } = typ.deref() {
                fields
            } else {
                unreachable!();
            };

            for ast_field in ast_fields.iter() {
                let name = ast_field.name.value.as_ref().unwrap();
                let mut typ = fields.get(name).unwrap().typ.0.borrow_mut();
                let ast_type = &ast_field.typ;

                if typ.upgrade().is_none() {
                    let t = self.get_type(ast_type).unwrap();
                    let t = Rc::downgrade(&t);
                    *typ = t;
                }
            }
        }
    }

    fn get_type(&self, typ: &ast::Type) -> Option<Rc<Type>> {
        match &typ {
            ast::Type::Primitive(token) => Some(self.get_type_from_primitive(token)),
            ast::Type::Ident(token) => self.get_type_from_ident(token),
            ast::Type::Struct(strct) => Some(self.get_struct(strct)),
        }
    }

    fn get_type_from_ident(&self, token: &Token) -> Option<Rc<Type>> {
        let name = token.value.as_ref().unwrap();
        self.type_alias.borrow().get(name).map(Rc::clone)
    }

    fn get_type_from_primitive(&self, token: &Token) -> Rc<Type> {
        match &token.kind {
            TokenKind::Bool => Rc::clone(&self.type_bool),
            TokenKind::I8 => Rc::clone(&self.type_i8),
            TokenKind::I16 => Rc::clone(&self.type_i16),
            TokenKind::I32 => Rc::clone(&self.type_i32),
            TokenKind::I64 => Rc::clone(&self.type_i64),
            TokenKind::U8 => Rc::clone(&self.type_u8),
            TokenKind::U16 => Rc::clone(&self.type_u16),
            TokenKind::U32 => Rc::clone(&self.type_u32),
            TokenKind::U64 => Rc::clone(&self.type_u64),
            TokenKind::F32 => Rc::clone(&self.type_f32),
            TokenKind::F64 => Rc::clone(&self.type_f64),
            _ => unreachable!(),
        }
    }

    fn get_struct(&self, strct: &ast::Struct) -> Rc<Type> {
        let mut fields: HashMap<String, Field> = HashMap::new();

        for (index, field) in strct.fields.iter().enumerate() {
            let name = field.name.value.as_ref().unwrap().clone();
            let typ = self.get_type(&field.typ);
            let typ = if let Some(typ) = typ {
                Rc::downgrade(&typ)
            } else {
                Weak::new()
            };
            let typ = TypePtr(RefCell::new(typ));
            fields.insert(name, Field { index, typ });
        }

        Rc::new(Type::Struct { fields })
    }

    fn get_type_by_name(&self, name: &String) -> Option<Rc<Type>> {
        self.type_alias.get(name).cloned()
    }

    fn get_fn_type(&self, header: &FnHeader) -> Option<Rc<Type>> {
        let mut arguments = Vec::new();

        for (index, arg) in header.params.iter().enumerate() {
            let typ = self.get_type(&arg.typ);
            if typ.is_none() {
                return None;
            }
            let typ = typ.unwrap();

            arguments.push(Argument {
                index,
                name: arg.name.value.as_ref().unwrap().clone(),
                typ: RefCell::new(Rc::downgrade(&typ)),
            });
        }

        let return_type = if let Some(t) = &header.ret_type {
            if let Some(typ) = self.get_type(t) {
                Some(TypePtr(RefCell::new(Rc::downgrade(&typ))))
            } else {
                None
            }
        } else {
            None
        };

        Some(Rc::new(Type::Fn { arguments, return_type }))
    }
}

struct ValueProcessor<'a, 'b> {
    root_ast: &'a ast::Root,
    type_processor: &'b TypeProcessor<'a>,

    symbol_table: Vec<HashMap<String, Symbol>>,
}

#[derive(Clone, Debug)]
struct Symbol {
    typ: Rc<Type>,
}

impl<'a, 'b> ValueProcessor<'a, 'b> {
    fn new(root_ast: &'a ast::Root, type_alias_processor: &'b TypeProcessor<'a>) -> Self {
        Self {
            root_ast,
            type_processor: type_alias_processor,
            symbol_table: Vec::new(),
        }
    }

    fn setup(&mut self) -> Result<(), Error> {
        self.setup_values()?;
        self.setup_functions()?;
        println!("symbol_tables: {:?}", self.symbol_table); // TODO: REMOVE!!
        Ok(())
    }

    fn setup_values(&mut self) -> Result<(), Error> {
        let type_decls: Vec<&ast::Var> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Var(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        self.add_symbol_table_block();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            let typ = self
                .type_processor
                .get_type(&type_decl.typ)
                .ok_or(Error::UndeclaredSymbol(type_decl.name.clone()))?; // TODO: fix the error reporting
            if self.find_symbol(name).is_some() {
                return Err(Error::RedeclaredSymbol(type_decl.name.clone()));
            }

            if let Some(val) = &type_decl.value {
                let value_type = self.analyze_constant_expr(val, &typ)?;
                if &value_type != &typ {
                    return Err(Error::MismatchType {
                        expected: typ.as_ref().clone(),
                        got: value_type.as_ref().clone(),
                        pos: val.pos,
                    });
                }
            }

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn analyze_constant_expr(&self, expr: &Expr, expected: &Rc<Type>) -> Result<Rc<Type>, Error> {
        match &expr.kind {
            ExprKind::Ident(token) => {
                let name = token.value.as_ref().unwrap();
                let symbol = self.find_symbol(name);
                if let Some(sym) = symbol {
                    Ok(Rc::clone(&sym.typ))
                } else {
                    Err(Error::UndeclaredSymbol(token.clone()))
                }
            }
            ExprKind::StructLit(struct_lit) => {
                let name = struct_lit.name.value.as_ref().unwrap();
                let typ = self.type_processor.get_type_by_name(name);
                if let Some(t) = typ {
                    Ok(t)
                } else {
                    Err(Error::UndeclaredSymbol(struct_lit.name.clone()))
                }
            }
            ExprKind::IntegerLit(_) => {
                if let Type::Int { signed: _, size: _ } = expected.deref() {
                    Ok(Rc::clone(expected))
                } else {
                    Ok(Rc::clone(&self.type_processor.type_i32))
                }
            }
            ExprKind::FloatLit(_) => {
                if let Type::Float { size: _ } = expected.deref() {
                    Ok(Rc::clone(expected))
                } else {
                    Ok(Rc::clone(&self.type_processor.type_f64))
                }
            }
            ExprKind::StringLit(_) => todo!(),
            ExprKind::BoolLit(_) => Ok(Rc::clone(&self.type_processor.type_bool)),
            ExprKind::Binary(binary) => {
                let a = self.analyze_constant_expr(binary.a.borrow(), expected)?;
                let b = self.analyze_constant_expr(binary.b.borrow(), expected)?;

                let matched = match binary.op.kind {
                    TokenKind::Eq | TokenKind::NotEq => a == b,
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq => a == b && a.is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => a == b && a.is_int(),
                    TokenKind::Shl | TokenKind::Shr => a.is_int() && b.is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a.is_bool() {
                            return Err(Error::MismatchType {
                                expected: Type::Bool,
                                got: a.as_ref().clone(),
                                pos: binary.a.pos,
                            });
                        }
                        if !b.is_bool() {
                            return Err(Error::MismatchType {
                                expected: Type::Bool,
                                got: b.as_ref().clone(),
                                pos: binary.a.pos,
                            });
                        }
                        true
                    }
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType {
                        expected: a.as_ref().clone(),
                        got: b.as_ref().clone(),
                        pos: binary.b.pos,
                    });
                }

                Ok(match binary.op.kind {
                    TokenKind::Eq
                    | TokenKind::NotEq
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq
                    | TokenKind::And
                    | TokenKind::Or => Rc::clone(&self.type_processor.type_bool),
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::Mod
                    | TokenKind::BitAnd
                    | TokenKind::BitOr
                    | TokenKind::BitXor
                    | TokenKind::Shl
                    | TokenKind::Shr => Rc::clone(&a),
                    _ => unreachable!(),
                })
            }
            ExprKind::Unary(unary) => {
                let val_type = self.analyze_constant_expr(unary.val.borrow(), expected)?;

                let matched = match unary.op.kind {
                    TokenKind::Not => val_type.is_bool(),
                    TokenKind::BitNot => val_type.is_int(),
                    TokenKind::Plus | TokenKind::Minus => val_type.is_number(),
                    _ => unreachable!(),
                };

                let expect = match unary.op.kind {
                    TokenKind::Not => BOOL.clone(),
                    TokenKind::BitNot => I32.clone(),
                    TokenKind::Plus | TokenKind::Minus => I32.clone(),
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType {
                        expected: expect,
                        got: val_type.as_ref().clone(),
                        pos: unary.val.pos,
                    });
                }

                Ok(match unary.op.kind {
                    TokenKind::Not => Rc::clone(&self.type_processor.type_bool),
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => Rc::clone(&val_type),
                    _ => unreachable!(),
                })
            }
            ExprKind::FunctionCall(_) => Err(Error::UnsupportedOperationInConstant(expr.pos)),
            ExprKind::Cast(cast) => {
                let typ = self.type_processor.get_type(&cast.target);
                if let Some(typ) = typ {
                    self.analyze_constant_expr(cast.val.borrow(), typ.borrow())
                } else {
                    Err(Error::UndeclaredType)
                }
            }
            ExprKind::Selector(selector) => {
                let typ = self.analyze_constant_expr(selector.source.as_ref(), expected)?;
                if let Type::Struct { fields } = typ.as_ref() {
                    if let Some(field) = fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Rc::clone(&field.typ.0.borrow().upgrade().unwrap()))
                    } else {
                        Err(Error::HasNoField(typ.as_ref().clone(), selector.source.pos))
                    }
                } else {
                    Err(Error::HasNoField(typ.as_ref().clone(), selector.source.pos))
                }
            }
        }
    }

    fn setup_functions(&mut self) -> Result<(), Error> {
        let type_decls: Vec<&ast::FnDecl> = self
            .root_ast
            .declarations
            .iter()
            .filter_map(|decl| {
                if let ast::Declaration::Fn(t) = decl {
                    Some(t)
                } else {
                    None
                }
            })
            .collect();

        self.add_symbol_table_block();
        for type_decl in type_decls.iter() {
            let name = type_decl.name.value.as_ref().unwrap();
            if self.find_symbol(name).is_some() {
                return Err(Error::RedeclaredSymbol(type_decl.name.clone()));
            }

            let typ = self
                .type_processor
                .get_fn_type(&type_decl.header)
                .ok_or(Error::UndeclaredSymbol(type_decl.name.clone()))?; // TODO: fix the error reporting

            self.add_symbol(name, typ);
        }

        Ok(())
    }

    fn add_symbol_table_block(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    fn add_symbol(&mut self, name: &String, typ: Rc<Type>) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .insert(name.clone(), Symbol { typ: Rc::clone(&typ) });
    }

    fn find_symbol(&self, name: &String) -> Option<Symbol> {
        for table in self.symbol_table.iter().rev() {
            if let Some(v) = table.get(name) {
                return Some(v.clone());
            }
        }
        None
    }
}
