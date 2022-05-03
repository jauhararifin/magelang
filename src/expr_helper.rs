use std::collections::HashMap;

use crate::{
    ast,
    semantic::{
        BinOp, Binary, ConcreteType, Expr, ExprKind, FieldValue, FloatType, FunctionCall, Header, IntType, Name,
        Selector, StructLit, Type, Unary, UnaryOp,
    },
    token::TokenKind,
    type_helper::ITypeHelper,
};

#[derive(Debug)]
pub enum Error {
    UndeclaredField,
    UndeclaredSymbol,
    MismatchType,
    NotAStruct,
    NotAFn,
    UnsupportedOperationInConstant,
    FnCallArgNumMismatch,
}

pub struct ExprHelper<'a, 'b> {
    package_name: &'a str,
    type_helper: &'b dyn ITypeHelper<'a>,

    symbol_table: Vec<HashMap<Name, Symbol>>,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: Name,
    pub typ: Type,
}

impl<'a, 'b> ExprHelper<'a, 'b> {
    pub fn empty(package_name: &'a str, type_helper: &'b dyn ITypeHelper<'a>) -> Self {
        let mut expr_helper = Self {
            package_name,
            type_helper,
            symbol_table: Vec::new(),
        };
        expr_helper.add_block();
        expr_helper
    }

    pub fn from_headers(package_name: &'a str, type_helper: &'b dyn ITypeHelper<'a>, headers: &'a [Header]) -> Self {
        let mut expr_helper = Self {
            package_name,
            type_helper,
            symbol_table: Vec::new(),
        };

        expr_helper.add_block();

        for header in headers.iter() {
            for var in header.vars.iter() {
                expr_helper.add_symbol(Symbol {
                    name: var.name.clone(),
                    typ: var.typ.clone(),
                });
            }
            for func in header.functions.iter() {
                expr_helper.add_symbol(Symbol {
                    name: func.name.clone(),
                    typ: func.typ.clone(),
                });
            }
        }

        expr_helper
    }

    // TODO: maybe can use iterator.
    pub fn get_all_symbols(&self) -> Vec<Symbol> {
        let mut symbols = HashMap::new();
        for table in self.symbol_table.iter() {
            for (name, sym) in table.iter() {
                symbols.insert(name, sym.clone());
            }
        }

        symbols.into_values().collect()
    }

    pub fn add_block(&mut self) {
        self.symbol_table.push(HashMap::new());
    }

    pub fn pop_block(&mut self) {
        self.symbol_table.pop();
    }

    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .insert(symbol.name.clone(), symbol);
    }

    pub fn find_symbol(&self, name: &Name) -> Option<&Symbol> {
        self.symbol_table.iter().rev().find_map(|table| table.get(name))
    }

    pub fn analyze_expr(&self, expr: &'a ast::Expr, expected: &Type, is_global_var: bool) -> Result<Expr, Error> {
        match &expr.kind {
            ast::ExprKind::Ident(token) => {
                let token_name = token.unwrap_str();
                let name = Name {
                    package: String::from(self.package_name),
                    name: String::from(token_name),
                };
                let symbol = self.find_symbol(&name);

                if let Some(sym) = symbol {
                    Ok(Expr {
                        kind: ExprKind::Ident(String::from(token_name)),
                        assignable: true,
                        typ: sym.typ.clone(),
                    })
                } else {
                    // TODO: currently, we can't reference symbol that is not declared previously.
                    // by right, this should be possible unless if the symbol has cyclic
                    // declaration. Improve this.
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                let typ = match &struct_lit.typ {
                    ast::Type::Primitive(_) => return Err(Error::NotAStruct),
                    ast::Type::Struct(_) => self.type_helper.get(&struct_lit.typ),
                    ast::Type::Ident(id) => self.type_helper.get_by_name(id.unwrap_str()),
                    ast::Type::Selector(s) => self.type_helper.get_selector(s),
                }
                .ok_or(Error::UndeclaredSymbol)?;

                let struct_fields = if let ConcreteType::Struct(strct) = &*typ.borrow() {
                    strct.fields.clone()
                } else {
                    return Err(Error::NotAStruct);
                };

                let mut fields = Vec::new();
                for field in struct_lit.fields.iter() {
                    let type_field = struct_fields.get(field.name.value.as_ref().unwrap());
                    if type_field.is_none() {
                        return Err(Error::UndeclaredField);
                    }
                    let type_field = type_field.unwrap();

                    let val = self.analyze_expr(&field.value, expected, is_global_var)?;
                    if val.typ != type_field.typ.upgrade().unwrap() {
                        return Err(Error::MismatchType);
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
                let concrete_type = expected.borrow();
                if let ConcreteType::Int(IntType { signed, size }) = &*concrete_type {
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
                        typ: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::I32(0),
                        assignable: false,
                        typ: self.type_helper.get_i32(),
                    })
                }
            }
            ast::ExprKind::FloatLit(val) => {
                let concrete_type = expected.borrow();
                if let ConcreteType::Float(FloatType { size }) = &*concrete_type {
                    let kind = match size {
                        32 => ExprKind::F32(val.value.as_ref().unwrap().parse().unwrap()),
                        64 => ExprKind::F64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        typ: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::F64(0.0),
                        assignable: false,
                        typ: self.type_helper.get_f64(),
                    })
                }
            }
            ast::ExprKind::StringLit(_) => todo!(),
            ast::ExprKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                typ: self.type_helper.get_bool(),
            }),
            ast::ExprKind::Binary(binary) => {
                let a = self.analyze_expr(binary.a.as_ref(), expected, is_global_var)?;
                let a_typ = a.typ.clone();

                let b = self.analyze_expr(binary.b.as_ref(), &a_typ, is_global_var)?;
                let b_typ = b.typ.clone();

                let matched = match binary.op.kind {
                    TokenKind::Eq | TokenKind::NotEq => a_typ == b_typ,
                    TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Mul
                    | TokenKind::Div
                    | TokenKind::GT
                    | TokenKind::LT
                    | TokenKind::GTEq
                    | TokenKind::LTEq => a_typ == b_typ && a_typ.borrow().is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => {
                        a_typ == b_typ && a_typ.borrow().is_int()
                    }
                    TokenKind::Shl | TokenKind::Shr => a_typ.borrow().is_int() && b_typ.borrow().is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a_typ.borrow().is_bool() || !b_typ.borrow().is_bool() {
                            return Err(Error::MismatchType);
                        }
                        true
                    }
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType);
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
                    | TokenKind::Or => self.type_helper.get_bool(),
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
                    typ: t.clone(),
                })
            }
            ast::ExprKind::Unary(unary) => {
                let val = self.analyze_expr(unary.val.as_ref(), expected, is_global_var)?;
                let val_type = val.typ.clone();

                let matched = match unary.op.kind {
                    TokenKind::Not => val_type.borrow().is_bool(),
                    TokenKind::BitNot => val_type.borrow().is_int(),
                    TokenKind::Plus | TokenKind::Minus => val_type.borrow().is_number(),
                    _ => unreachable!(),
                };

                if !matched {
                    return Err(Error::MismatchType);
                }

                let op = match unary.op.kind {
                    TokenKind::Not => UnaryOp::Not,
                    TokenKind::BitNot => UnaryOp::BitNot,
                    TokenKind::Plus => UnaryOp::Plus,
                    TokenKind::Minus => UnaryOp::Minus,
                    _ => unreachable!(),
                };

                let t = match unary.op.kind {
                    TokenKind::Not => self.type_helper.get_bool(),
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => val_type.clone(),
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
                    return Err(Error::UnsupportedOperationInConstant);
                }

                let func = self.analyze_expr(func_call.func.as_ref(), expected, is_global_var)?;

                let fn_type = func.typ.borrow();
                let fn_type = if let ConcreteType::Fn(fn_type) = &*fn_type {
                    fn_type
                } else {
                    return Err(Error::NotAFn);
                };

                if fn_type.arguments.len() != func_call.args.len() {
                    return Err(Error::FnCallArgNumMismatch);
                }

                let mut args = Vec::new();
                for (i, arg) in func_call.args.iter().enumerate() {
                    let val = self.analyze_expr(arg, expected, is_global_var)?;
                    let val_type = val.typ.clone();
                    let func_type = fn_type.arguments.get(i).unwrap().typ.upgrade().unwrap();
                    if val_type != func_type {
                        return Err(Error::MismatchType);
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    t.upgrade().unwrap()
                } else {
                    self.type_helper.get_void()
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func.clone()),
                        args,
                    }),
                    assignable: false,
                    typ: return_type,
                })
            }
            ast::ExprKind::Cast(cast) => {
                let typ = self.type_helper.get(&cast.target);
                if let Some(typ) = typ {
                    self.analyze_expr(cast.val.as_ref(), &typ, is_global_var)
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::Selector(selector) => {
                let val = self.analyze_expr(selector.source.as_ref(), expected, is_global_var)?;
                let typ = val.typ.clone();
                let typ = typ.borrow();
                if let ConcreteType::Struct(strct) = &*typ {
                    if let Some(field) = strct.fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Expr {
                            kind: ExprKind::Selector(Selector {
                                source: Box::new(val),
                                selection: selector.selection.value.as_ref().unwrap().clone(),
                                selection_index: field.index,
                            }),
                            assignable: true,
                            typ: field.typ.upgrade().unwrap(),
                        })
                    } else {
                        Err(Error::NotAStruct)
                    }
                } else {
                    Err(Error::NotAStruct)
                }
            }
        }
    }
}
