use std::collections::HashMap;

use crate::{
    ast::{ExprNode, ExprNodeKind},
    errors::Error,
    semantic::{BinOp, Binary, Expr, ExprKind, FloatType, FunctionCall, Header, IntType, Type, Unary, UnaryOp},
    token::TokenKind,
};

use super::types::TypeHelper;

pub struct ExprHelper<'a> {
    type_helper: &'a TypeHelper,
    symbol_table: Vec<HashMap<String, Symbol>>,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub typ: Type,
}

impl<'a> ExprHelper<'a> {
    pub fn empty(type_helper: &'a TypeHelper) -> Self {
        let mut expr_helper = Self {
            type_helper,
            symbol_table: Vec::new(),
        };
        expr_helper.add_block();
        expr_helper
    }

    pub fn from_headers(type_helper: &'a TypeHelper, headers: &'a [Header]) -> Self {
        let mut expr_helper = Self {
            type_helper,
            symbol_table: Vec::new(),
        };

        expr_helper.add_block();

        for header in headers.iter() {
            for func in header.functions.iter() {
                expr_helper.add_symbol(Symbol {
                    name: func.name.clone(),
                    typ: Type::Fn(func.typ.clone()),
                });
            }
        }

        expr_helper
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

    pub fn find_symbol(&self, name: &String) -> Option<Symbol> {
        self.symbol_table
            .iter()
            .rev()
            .find_map(|table| table.get(name))
            .map(|t| t.clone())
    }

    pub fn analyze(&self, expr: &'a ExprNode, expected: &Type) -> Result<Expr, Error> {
        match &expr.kind {
            ExprNodeKind::Ident(token) => {
                let token_name = token.unwrap_value();
                let symbol = self.find_symbol(token_name);

                if let Some(sym) = symbol {
                    Ok(Expr {
                        kind: ExprKind::Ident(String::from(token_name)),
                        assignable: true,
                        typ: sym.typ.clone(),
                    })
                } else {
                    Err(Error::UndeclaredSymbol)
                }
            }
            ExprNodeKind::IntegerLit(val) => {
                if let Type::Int(IntType { signed, size }) = expected {
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
                        typ: Type::Int(IntType::signed(32)),
                    })
                }
            }
            ExprNodeKind::FloatLit(val) => {
                if let Type::Float(FloatType { size }) = expected {
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
                        typ: Type::Float(FloatType { size: 64 }),
                    })
                }
            }
            ExprNodeKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                typ: Type::Bool,
            }),
            ExprNodeKind::Binary(binary) => {
                let a = self.analyze(binary.a.as_ref(), expected)?;
                let a_typ = a.typ.clone();

                let b = self.analyze(binary.b.as_ref(), &a_typ)?;
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
                    | TokenKind::LTEq => a_typ == b_typ && a_typ.is_number(),
                    TokenKind::Mod | TokenKind::BitAnd | TokenKind::BitOr | TokenKind::BitXor => {
                        a_typ == b_typ && a_typ.is_int()
                    }
                    TokenKind::Shl | TokenKind::Shr => a_typ.is_int() && b_typ.is_int(),
                    TokenKind::And | TokenKind::Or => {
                        if !a_typ.is_bool() || !b_typ.is_bool() {
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
                    | TokenKind::Or => Type::Bool,
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
            ExprNodeKind::Unary(unary) => {
                let val = self.analyze(unary.val.as_ref(), expected)?;
                let val_type = val.typ.clone();

                let matched = match unary.op.kind {
                    TokenKind::Not => val_type.is_bool(),
                    TokenKind::BitNot => val_type.is_int(),
                    TokenKind::Plus | TokenKind::Minus => val_type.is_number(),
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
                    TokenKind::Not => Type::Bool,
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => val_type.clone(),
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
                    assignable: false,
                    typ: t,
                })
            }
            ExprNodeKind::FunctionCall(func_call) => {
                let func = self.analyze(func_call.func.as_ref(), expected)?;

                let fn_type = if let Type::Fn(fn_type) = &func.typ {
                    fn_type
                } else {
                    return Err(Error::NotAFn);
                };

                if fn_type.arguments.len() != func_call.args.len() {
                    return Err(Error::FnCallArgNumMismatch);
                }

                let mut args = Vec::new();
                for (i, arg) in func_call.args.iter().enumerate() {
                    let val = self.analyze(arg, expected)?;
                    let val_type = val.typ.clone();
                    let func_type = fn_type.arguments.get(i).unwrap().typ.clone();
                    if val_type != func_type {
                        return Err(Error::MismatchType);
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    t.as_ref().clone()
                } else {
                    Type::Void
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func.clone()),
                        args,
                    }),
                    assignable: false,
                    typ: return_type.clone(),
                })
            }
            ExprNodeKind::Cast(cast) => {
                let typ = self.type_helper.get(&cast.target);
                self.analyze(cast.val.as_ref(), &typ)
            }
            ExprNodeKind::Empty => unreachable!(),
        }
    }
}
