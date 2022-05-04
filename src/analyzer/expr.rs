use std::collections::HashMap;

use crate::{
    ast,
    errors::Error,
    semantic::{
        BinOp, Binary, Expr, ExprKind, FieldValue, FloatType, FunctionCall, Header, IntType, Name, Selector, StructLit,
        TypeKind, Unary, UnaryOp,
    },
    token::TokenKind,
};

use super::types::{AstContext, TypeHelper};

pub struct ExprHelper<'a, 'b> {
    package_name: &'a str,
    type_helper: &'b TypeHelper,

    symbol_table: Vec<HashMap<Name, Symbol>>,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: Name,
    pub type_kind: TypeKind,
}

impl<'a, 'b> ExprHelper<'a, 'b> {
    pub fn empty(package_name: &'a str, type_helper: &'b TypeHelper) -> Self {
        let mut expr_helper = Self {
            package_name,
            type_helper,
            symbol_table: Vec::new(),
        };
        expr_helper.add_block();
        expr_helper
    }

    pub fn from_headers(package_name: &'a str, type_helper: &'b TypeHelper, headers: &'a [Header]) -> Self {
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
                    type_kind: var.type_kind.clone(),
                });
            }
            for func in header.functions.iter() {
                expr_helper.add_symbol(Symbol {
                    name: func.name.clone(),
                    type_kind: TypeKind::Fn(func.typ.clone()),
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

    pub fn find_symbol(&self, name: &Name) -> Option<&Symbol> {
        self.symbol_table.iter().rev().find_map(|table| table.get(name))
    }

    pub fn analyze(
        &self,
        ctx: &AstContext,
        expr: &'a ast::Expr,
        expected: &TypeKind,
        is_global_var: bool,
    ) -> Result<Expr, Error> {
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
                        type_kind: sym.type_kind.clone(),
                    })
                } else {
                    // TODO: currently, we can't reference symbol that is not declared previously.
                    // by right, this should be possible unless if the symbol has cyclic
                    // declaration. Improve this.
                    Err(Error::UndeclaredSymbol)
                }
            }
            ast::ExprKind::StructLit(struct_lit) => {
                let type_kind = match &struct_lit.typ {
                    ast::Type::Primitive(_) => return Err(Error::NotAStruct),
                    ast::Type::Struct(_) => self.type_helper.get(ctx, &struct_lit.typ),
                    ast::Type::Ident(id) => self.type_helper.get_by_name(&Name {
                        package: String::from(self.package_name),
                        name: String::from(id.unwrap_str()),
                    }),
                    ast::Type::Selector(s) => self.type_helper.get_by_selector(ctx, s),
                };

                if type_kind.is_invalid() {
                    return Err(Error::UndeclaredSymbol);
                }

                let struct_fields = if let TypeKind::Struct(strct) = &type_kind {
                    &strct.fields
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

                    let val = self.analyze(ctx, &field.value, expected, is_global_var)?;
                    if val.type_kind != type_field.type_kind {
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
                    type_kind,
                })
            }
            ast::ExprKind::IntegerLit(val) => {
                if let TypeKind::Int(IntType { signed, size }) = expected {
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
                        type_kind: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::I32(0),
                        assignable: false,
                        type_kind: TypeKind::Int(IntType::signed(32)),
                    })
                }
            }
            ast::ExprKind::FloatLit(val) => {
                if let TypeKind::Float(FloatType { size }) = expected {
                    let kind = match size {
                        32 => ExprKind::F32(val.value.as_ref().unwrap().parse().unwrap()),
                        64 => ExprKind::F64(val.value.as_ref().unwrap().parse().unwrap()),
                        _ => unreachable!(),
                    };
                    Ok(Expr {
                        kind,
                        assignable: false,
                        type_kind: expected.clone(),
                    })
                } else {
                    Ok(Expr {
                        kind: ExprKind::F64(0.0),
                        assignable: false,
                        type_kind: TypeKind::Float(FloatType { size: 64 }),
                    })
                }
            }
            ast::ExprKind::StringLit(_) => todo!(),
            ast::ExprKind::BoolLit(val) => Ok(Expr {
                kind: ExprKind::Bool(val.kind == TokenKind::True),
                assignable: false,
                type_kind: TypeKind::Bool,
            }),
            ast::ExprKind::Binary(binary) => {
                let a = self.analyze(ctx, binary.a.as_ref(), expected, is_global_var)?;
                let a_typ = a.type_kind.clone();

                let b = self.analyze(ctx, binary.b.as_ref(), &a_typ, is_global_var)?;
                let b_typ = b.type_kind.clone();

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
                    | TokenKind::Or => TypeKind::Bool,
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
                    type_kind: t.clone(),
                })
            }
            ast::ExprKind::Unary(unary) => {
                let val = self.analyze(ctx, unary.val.as_ref(), expected, is_global_var)?;
                let val_type = val.type_kind.clone();

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
                    TokenKind::Not => TypeKind::Bool,
                    TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => val_type.clone(),
                    _ => unreachable!(),
                };

                Ok(Expr {
                    kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
                    assignable: false,
                    type_kind: t,
                })
            }
            ast::ExprKind::FunctionCall(func_call) => {
                if is_global_var {
                    return Err(Error::UnsupportedOperationInConstant);
                }

                let func = self.analyze(ctx, func_call.func.as_ref(), expected, is_global_var)?;

                let fn_type = if let TypeKind::Fn(fn_type) = &func.type_kind {
                    fn_type
                } else {
                    return Err(Error::NotAFn);
                };

                if fn_type.arguments.len() != func_call.args.len() {
                    return Err(Error::FnCallArgNumMismatch);
                }

                let mut args = Vec::new();
                for (i, arg) in func_call.args.iter().enumerate() {
                    let val = self.analyze(ctx, arg, expected, is_global_var)?;
                    let val_type = val.type_kind.clone();
                    let func_type = fn_type.arguments.get(i).unwrap().type_kind.clone();
                    if val_type != func_type {
                        return Err(Error::MismatchType);
                    }

                    args.push(val);
                }

                let return_type = if let Some(t) = &fn_type.return_type {
                    t.as_ref().clone()
                } else {
                    TypeKind::Void
                };

                Ok(Expr {
                    kind: ExprKind::FunctionCall(FunctionCall {
                        func: Box::new(func.clone()),
                        args,
                    }),
                    assignable: false,
                    type_kind: return_type.clone(),
                })
            }
            ast::ExprKind::Cast(cast) => {
                let typ = self.type_helper.get(ctx, &cast.target);
                if typ.is_invalid() {
                    return Err(Error::UndeclaredSymbol);
                }
                self.analyze(ctx, cast.val.as_ref(), &typ, is_global_var)
            }
            ast::ExprKind::Selector(selector) => {
                let val = self.analyze(ctx, selector.source.as_ref(), expected, is_global_var)?;
                if let TypeKind::Struct(strct) = &val.type_kind {
                    if let Some(field) = strct.fields.get(selector.selection.value.as_ref().unwrap()) {
                        Ok(Expr {
                            kind: ExprKind::Selector(Selector {
                                source: Box::new(val.clone()),
                                selection: selector.selection.value.as_ref().unwrap().clone(),
                                selection_index: field.index,
                            }),
                            assignable: true,
                            type_kind: field.type_kind.clone(),
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
