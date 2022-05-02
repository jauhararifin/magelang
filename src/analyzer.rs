use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::{Rc, Weak},
};

use crate::{
    ast::{self, FnHeader, TypeDecl},
    parser,
    pos::Pos,
    semantic::{
        Argument, Assign, AssignOp, BinOp, Binary, BlockStatement, Expr, ExprKind, Field, FieldValue, FnDecl, FnType,
        FunctionCall, If, Return, Unit, Selector, Statement, StructLit, Type, TypePtr, Unary, UnaryOp, Var, While,
        BOOL, F32, F64, I16, I32, I64, I8, U16, U32, U64, U8, VOID,
    },
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
    FuncCallArgNumMismatch,
    IsNotAFunction,
    MissingMain,
}

pub trait Analyzer {
    fn analyze(&mut self) -> Result<Unit, Error>;
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
    fn analyze(&mut self) -> Result<Unit, Error> {
        let mut type_processor = TypeProcessor::new(&self.root_ast);
        type_processor.setup()?;

        let mut value_processor = ValueProcessor::new(&self.root_ast, &type_processor);
        value_processor.setup()?;

        let mut func_processor = FuncProcessor::new(&self.root_ast, &type_processor, &mut value_processor);
        let fn_declarations = func_processor.analyze()?;

        Ok(Unit {
            package_name: self.root_ast.package.value.as_ref().unwrap().clone(),
            type_declarations: Vec::new(),
            var_declarations: Vec::new(),
            fn_declarations,
        })
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
    type_void: Rc<Type>,
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
            type_void: Rc::new(VOID),
        }
    }

    fn setup(&mut self) -> Result<(), Error> {
        let plan = self.build_setup_plan()?;
        self.setup_types(&plan);
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

        Some(Rc::new(Type::Fn(FnType { arguments, return_type })))
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
                let value = self.analyze_expr(val, Some(&typ), true)?;
                let value_type = Rc::clone(&value.typ);
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

    fn analyze_expr(&self, expr: &ast::Expr, expected: Option<&Rc<Type>>, is_global_var: bool) -> Result<Expr, Error> {
        match &expr.kind {
            ast::ExprKind::Ident(token) => {
                let name = token.value.as_ref().unwrap();
                let symbol = self.find_symbol(name);

                if let Some(sym) = symbol {
                    Ok(Expr {
                        kind: ExprKind::Ident(name.clone()),
                        assignable: true,
                        typ: Rc::clone(&sym.typ),
                    })
                } else {
                    Err(Error::UndeclaredSymbol(token.clone()))
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                let name = struct_lit.name.value.as_ref().unwrap();
                let typ = self
                    .type_processor
                    .get_type_by_name(name)
                    .ok_or(Error::UndeclaredSymbol(struct_lit.name.clone()))?;

                let struct_fields = if let Type::Struct { fields } = typ.as_ref() {
                    fields
                } else {
                    return Err(Error::HasNoField(typ.as_ref().clone(), struct_lit.name.pos));
                };

                let mut fields = Vec::new();
                for field in struct_lit.fields.iter() {
                    let type_field = struct_fields.get(field.name.value.as_ref().unwrap());
                    if type_field.is_none() {
                        return Err(Error::UndefinedField {
                            typ: typ.as_ref().clone(),
                            field: field.name.clone(),
                        });
                    }
                    let type_field = type_field.unwrap();

                    let val = self.analyze_expr(&field.value, expected, is_global_var)?;
                    if val.typ != type_field.typ.0.borrow().upgrade().unwrap() {
                        return Err(Error::MismatchType {
                            expected: type_field.typ.0.borrow().upgrade().unwrap().as_ref().clone(),
                            got: val.typ.as_ref().clone(),
                            pos: field.value.pos,
                        });
                    }

                    fields.push(FieldValue {
                        index: type_field.index,
                        value: val,
                    });
                }

                Ok(Expr {
                    kind: ExprKind::Struct(StructLit { fields }),
                    assignable: false,
                    typ,
                })
            }
            ast::ExprKind::IntegerLit(val) => {
                let expected = expected.unwrap_or(&self.type_processor.type_i32);
                if let Type::Int { signed, size } = expected.deref() {
                    let kind = match (signed, size) {
                        (true, 8) => ExprKind::I8(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 16) => ExprKind::I16(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 32) => ExprKind::I32(val.value.as_ref().unwrap().parse().unwrap()),
                        (true, 64) => ExprKind::I64(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 8) => ExprKind::U8(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 16) => ExprKind::U16(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 32) => ExprKind::U32(val.value.as_ref().unwrap().parse().unwrap()),
                        (false, 64) => ExprKind::U64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: Rc::clone(expected),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::I32(0),
                        assignable: false,
                        typ: Rc::clone(&self.type_processor.type_i32),
                    })
                }
            }
            ast::ExprKind::FloatLit(val) => {
                let expected = expected.unwrap_or(&self.type_processor.type_f64);
                if let Type::Float { size } = expected.deref() {
                    let kind = match size {
                        32 => ExprKind::F32(val.value.as_ref().unwrap().parse().unwrap()),
                        64 => ExprKind::F64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: Rc::clone(expected),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::F64(0.0),
                        assignable: false,
                        typ: Rc::clone(&self.type_processor.type_f64),
                    })
                }
            }
            ast::ExprKind::StringLit(_) => todo!(),
            ast::ExprKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                typ: Rc::clone(&self.type_processor.type_bool),
            }),
            ast::ExprKind::Binary(binary) => {
                let a = self.analyze_expr(binary.a.borrow(), expected, is_global_var)?;
                let a_typ = Rc::clone(&a.typ);

                let b = self.analyze_expr(binary.b.borrow(), Some(&a_typ), is_global_var)?;
                let b_typ = Rc::clone(&b.typ);

                let matched = match binary.op.kind {
                    TokenKind::Eq | TokenKind::NotEq => a_typ == b_typ,
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq => a_typ == b_typ && a_typ.is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => {
                        a_typ == b_typ && a_typ.is_int()
                    }
                    TokenKind::Shl | TokenKind::Shr => a_typ.is_int() && b_typ.is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a_typ.is_bool() {
                            return Err(Error::MismatchType {
                                expected: Type::Bool,
                                got: a_typ.as_ref().clone(),
                                pos: binary.a.pos,
                            });
                        }
                        if !b_typ.is_bool() {
                            return Err(Error::MismatchType {
                                expected: Type::Bool,
                                got: b_typ.as_ref().clone(),
                                pos: binary.a.pos,
                            });
                        }
                        true
                    }
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType {
                        expected: a_typ.as_ref().clone(),
                        got: b_typ.as_ref().clone(),
                        pos: binary.b.pos,
                    });
                }

                let bin_op = match binary.op.kind {
                    TokenKind::Eq => BinOp::Eq,
                    TokenKind::NotEq => BinOp::NotEq,
                    TokenKind::GT => BinOp::GT,
                    TokenKind::LT => BinOp::LT,
                    TokenKind::GTEq => BinOp::GTEq,
                    TokenKind::LTEq => BinOp::LTEq,
                    TokenKind::And => BinOp::And,
                    TokenKind::Or => BinOp::Or,
                    TokenKind::Plus => BinOp::Plus,
                    TokenKind::Minus => BinOp::Minus,
                    TokenKind::Mul => BinOp::Mul,
                    TokenKind::Div => BinOp::Div,
                    TokenKind::Mod => BinOp::Mod,
                    TokenKind::BitAnd => BinOp::BitAnd,
                    TokenKind::BitOr => BinOp::BitOr,
                    TokenKind::BitXor => BinOp::BitXor,
                    TokenKind::Shl => BinOp::Shl,
                    TokenKind::Shr => BinOp::Shr,
                    _ => unreachable!(),
                };

                let kind = ExprKind::Binary(Binary {
                    op: bin_op,
                    a: Box::new(a),
                    b: Box::new(b),
                });

                let t = match binary.op.kind {
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
                    | TokenKind::Shr => a_typ,
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind,
                    assignable: false,
                    typ: Rc::clone(&t),
                })
            }
            ast::ExprKind::Unary(unary) => {
                let val = self.analyze_expr(unary.val.borrow(), expected, is_global_var)?;
                let val_type = Rc::clone(&val.typ);

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

                let op = match unary.op.kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::BitNot => UnaryOp::BitNot,
                    TokenKind::Plus => UnaryOp::Plus,
                    TokenKind::Minus => UnaryOp::Minus,
                    _ => unreachable!(),
                };

                let t = match unary.op.kind {
                    TokenKind::Not => Rc::clone(&self.type_processor.type_bool),
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => Rc::clone(&val_type),
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
                    assignable: false,
                    typ: t,
                })
            }
            ast::ExprKind::FunctionCall(func_call) => {
                if is_global_var {
                    return Err(Error::UnsupportedOperationInConstant(expr.pos));
                }

                let func = self.analyze_expr(func_call.func.as_ref(), expected, is_global_var)?;

                let fn_type = if let Type::Fn(fn_type) = func.typ.borrow() {
                    fn_type
                } else {
                    return Err(Error::IsNotAFunction);
                };

                if fn_type.arguments.len() != func_call.args.len() {
                    return Err(Error::FuncCallArgNumMismatch);
                }

                let mut args = Vec::new();
                for (i, arg) in func_call.args.iter().enumerate() {
                    let val = self.analyze_expr(arg, expected, is_global_var)?;
                    let val_type = Rc::clone(&val.typ);
                    let func_type = fn_type.arguments.get(i).unwrap().typ.borrow().upgrade().unwrap();
                    if val_type != func_type {
                        return Err(Error::MismatchType {
                            expected: func_type.as_ref().clone(),
                            got: val_type.as_ref().clone(),
                            pos: arg.pos,
                        });
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    Rc::clone(&t.0.borrow().upgrade().unwrap())
                } else {
                    Rc::clone(&self.type_processor.type_void)
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func),
                        args,
                    }),
                    assignable: false,
                    typ: return_type,
                })
            }
            ast::ExprKind::Cast(cast) => {
                let typ = self.type_processor.get_type(&cast.target);
                if let Some(typ) = typ {
                    self.analyze_expr(cast.val.borrow(), Some(typ.borrow()), is_global_var)
                } else {
                    Err(Error::UndeclaredType)
                }
            }
            ast::ExprKind::Selector(selector) => {
                let val = self.analyze_expr(selector.source.as_ref(), expected, is_global_var)?;
                let typ = Rc::clone(&val.typ);
                if let Type::Struct { fields } = typ.as_ref() {
                    if let Some(field) = fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Expr {
                            kind: ExprKind::Selector(Selector {
                                source: Box::new(val),
                                selection: selector.selection.value.as_ref().unwrap().clone(),
                                selection_index: field.index,
                            }),
                            assignable: true,
                            typ: Rc::clone(&field.typ.0.borrow().upgrade().unwrap()),
                        })
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

    fn pop_symbol_table_block(&mut self) {
        self.symbol_table.pop();
    }
}

struct FuncProcessor<'a, 'b> {
    root_ast: &'a ast::Root,
    type_processor: &'b TypeProcessor<'a>,
    value_processor: &'b mut ValueProcessor<'a, 'b>,
}

impl<'a, 'b> FuncProcessor<'a, 'b> {
    fn new(
        root_ast: &'a ast::Root,
        type_processor: &'b TypeProcessor<'a>,
        value_processor: &'b mut ValueProcessor<'a, 'b>,
    ) -> Self {
        Self {
            root_ast,
            type_processor,
            value_processor,
        }
    }

    fn analyze(&mut self) -> Result<Vec<FnDecl>, Error> {
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

        let mut fn_declarations: Vec<FnDecl> = Vec::new();
        for fn_decl in type_decls.iter() {
            let name = fn_decl.name.value.as_ref().unwrap().clone();
            let func_symbol = self.value_processor.find_symbol(&name).unwrap();
            let func_type = func_symbol.typ;

            self.value_processor.add_symbol_table_block();
            let ftype = if let Type::Fn(t) = func_type.as_ref() {
                for arg in t.arguments.iter() {
                    self.value_processor
                        .add_symbol(&arg.name, Rc::clone(&arg.typ.borrow().upgrade().unwrap()));
                }
                t
            } else {
                unreachable!();
            };

            let statement = self.analyze_block_stmt(&fn_decl.body, ftype)?;

            let native = fn_decl.header.native;

            fn_declarations.push(FnDecl {
                name,
                native,
                typ: func_type,
                body: statement,
            });

            self.value_processor.pop_symbol_table_block();
        }

        Ok(fn_declarations)
    }

    fn analyze_stmt(&mut self, stmt: &ast::Statement, ftype: &FnType) -> Result<Statement, Error> {
        match stmt {
            ast::Statement::Var(stmt) => {
                // TODO jauhararifin: this implementation is similar to the one in the value
                // processor. Maybe can consider make it dryer.

                let name = stmt.name.value.as_ref().unwrap();
                let typ = self
                    .type_processor
                    .get_type(&stmt.typ)
                    .ok_or(Error::UndeclaredSymbol(stmt.name.clone()))?; // TODO: fix the error reporting
                if self.value_processor.find_symbol(name).is_some() {
                    return Err(Error::RedeclaredSymbol(stmt.name.clone()));
                }

                let value = if let Some(val) = &stmt.value {
                    let value = self.value_processor.analyze_expr(val, Some(&typ), false)?;
                    let value_type = Rc::clone(&value.typ);
                    if &value.typ != &typ {
                        return Err(Error::MismatchType {
                            expected: typ.as_ref().clone(),
                            got: value_type.as_ref().clone(),
                            pos: val.pos,
                        });
                    }
                    Some(value)
                } else {
                    None
                };

                self.value_processor.add_symbol(name, Rc::clone(&typ));

                Ok(Statement::Var(Var {
                    name: name.clone(),
                    typ: Rc::clone(&typ),
                    value,
                }))
            }
            ast::Statement::Assign(stmt) => {
                let receiver = self.value_processor.analyze_expr(&stmt.receiver, None, false)?;
                if !receiver.assignable {
                    return Err(Error::CannotAssignTo(receiver.typ.as_ref().clone(), stmt.receiver.pos));
                }

                let value = self
                    .value_processor
                    .analyze_expr(&stmt.value, Some(&receiver.typ), false)?;

                // TODO: check based on the op instead of just using !=.
                if value.typ != receiver.typ {
                    return Err(Error::MismatchType {
                        expected: receiver.typ.as_ref().clone(),
                        got: value.typ.as_ref().clone(),
                        pos: stmt.value.pos,
                    });
                }

                let op = match &stmt.op.kind {
                    TokenKind::Assign => AssignOp::Assign,
                    TokenKind::PlusAssign => AssignOp::PlusAssign,
                    TokenKind::MinusAssign => AssignOp::MinusAssign,
                    TokenKind::MulAssign => AssignOp::MulAssign,
                    TokenKind::DivAssign => AssignOp::DivAssign,
                    TokenKind::ModAssign => AssignOp::ModAssign,
                    TokenKind::BitAndAssign => AssignOp::BitAndAssign,
                    TokenKind::BitOrAssign => AssignOp::BitOrAssign,
                    TokenKind::BitXorAssign => AssignOp::BitXorAssign,
                    TokenKind::ShlAssign => AssignOp::ShlAssign,
                    TokenKind::ShrAssign => AssignOp::ShrAssign,
                    _ => unreachable!(),
                };

                Ok(Statement::Assign(Assign { receiver, op, value }))
            }
            ast::Statement::Return(stmt) => {
                if let Some(expected_ret_type) = &ftype.return_type {
                    let t = expected_ret_type.0.borrow().upgrade().unwrap();
                    if let Some(ret_val) = &stmt.value {
                        let val = self.value_processor.analyze_expr(&ret_val, None, false)?;
                        if val.typ != t {
                            return Err(Error::MismatchType {
                                expected: t.as_ref().clone(),
                                got: val.typ.as_ref().clone(),
                                pos: ret_val.pos,
                            });
                        }

                        Ok(Statement::Return(Return { value: Some(val) }))
                    } else {
                        Err(Error::MissingReturnValue(stmt.ret.pos))
                    }
                } else {
                    if let Some(ret_val) = &stmt.value {
                        let val = self.value_processor.analyze_expr(&ret_val, None, false)?;
                        Err(Error::MismatchType {
                            expected: self.type_processor.type_void.as_ref().clone(),
                            got: val.typ.as_ref().clone(),
                            pos: ret_val.pos,
                        })
                    } else {
                        Ok(Statement::Return(Return { value: None }))
                    }
                }
            }
            ast::Statement::If(stmt) => {
                let cond =
                    self.value_processor
                        .analyze_expr(&stmt.cond, Some(&self.type_processor.type_bool), false)?;
                let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
                Ok(Statement::If(If { cond, body }))
            }
            ast::Statement::While(stmt) => {
                let cond =
                    self.value_processor
                        .analyze_expr(&stmt.cond, Some(&self.type_processor.type_bool), false)?;
                let body = Box::new(self.analyze_block_stmt(&stmt.body, ftype)?);
                Ok(Statement::While(While { cond, body }))
            }
            ast::Statement::Block(stmt) => self.analyze_block_stmt(stmt, ftype),
            ast::Statement::Expr(_stmt) => todo!(),
            ast::Statement::Continue => todo!(),
            ast::Statement::Break => todo!(),
        }
    }

    fn analyze_block_stmt(&mut self, stmt: &ast::BlockStatement, ftype: &FnType) -> Result<Statement, Error> {
        let mut body = Vec::new();
        for s in stmt.body.iter() {
            body.push(self.analyze_stmt(s, ftype)?);
        }
        Ok(Statement::Block(BlockStatement { body }))
    }
}
