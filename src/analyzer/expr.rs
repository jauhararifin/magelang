use std::{collections::HashMap, rc::Rc};

use crate::{
    ast::{ArrayNode, BinaryNode, CastNode, ExprNode, ExprNodeKind, FunctionCallNode, IndexNode, UnaryNode},
    errors::Error,
    semantic::{
        Array, ArrayType, BinOp, Binary, Cast, Expr, ExprKind, FloatType, FunctionCall, Header, Index, IntType, Type,
        Unary, UnaryOp,
    },
    token::{Token, TokenKind},
};

use super::types::TypeHelper;

pub struct ExprHelper<'a> {
    type_helper: &'a TypeHelper,
    symbol_table: Vec<HashMap<Rc<String>, Rc<Symbol>>>,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub name: Rc<String>,
    pub typ: Rc<Type>,
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
                expr_helper.add_symbol(Rc::new(Symbol {
                    name: func.name.clone(),
                    typ: func.typ.clone(),
                }));
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

    pub fn add_symbol(&mut self, symbol: Rc<Symbol>) {
        self.symbol_table
            .last_mut()
            .unwrap()
            .insert(symbol.name.clone(), symbol);
    }

    pub fn find_symbol(&self, name: &String) -> Option<Rc<Symbol>> {
        self.symbol_table
            .iter()
            .rev()
            .find_map(|table| table.get(name))
            .map(|t| t.clone())
    }

    pub fn analyze(&self, expr: &'a ExprNode, expected: Rc<Type>) -> Result<Expr, Error> {
        match &expr.kind {
            ExprNodeKind::Ident(token) => self.analyze_ident(token),
            ExprNodeKind::IntegerLit(token) => self.analyze_int_lit(token, &expected),
            ExprNodeKind::FloatLit(token) => self.analyze_float_lit(token, &expected),
            ExprNodeKind::BoolLit(token) => self.analyze_bool_lit(token),
            ExprNodeKind::Binary(binary) => self.analyze_binary_expr(binary, &expected),
            ExprNodeKind::Unary(unary) => self.analyze_unary_expr(unary, &expected),
            ExprNodeKind::FunctionCall(func_call) => self.analyze_func_call(func_call, &expected),
            ExprNodeKind::Index(index_node) => self.analyze_index(index_node),
            ExprNodeKind::Array(array_node) => self.analyze_array(array_node),
            ExprNodeKind::Cast(cast) => self.analyze_cast(cast, &expected),
            ExprNodeKind::Empty => unreachable!(),
        }
    }

    fn analyze_ident(&self, token: &Token) -> Result<Expr, Error> {
        let token_name = token.clone_value();
        self.find_symbol(token_name.as_ref())
            .map(|sym| Expr {
                kind: ExprKind::Ident(token_name),
                assignable: true,
                typ: sym.typ.clone(),
            })
            .ok_or(Error::UndeclaredSymbol)
    }

    fn analyze_int_lit(&self, token: &Token, expected: &Rc<Type>) -> Result<Expr, Error> {
        let token = token.clone();
        let assignable = false;

        if let Type::Int(IntType { signed, size }) = expected.as_ref() {
            let value = token.unwrap_value();
            let kind = match (signed, size) {
                (true, 8) => value.parse().map(|v| ExprKind::I8(v)),
                (true, 16) => value.parse().map(|v| ExprKind::I16(v)),
                (true, 32) => value.parse().map(|v| ExprKind::I32(v)),
                (true, 64) => value.parse().map(|v| ExprKind::I64(v)),
                (false, 8) => value.parse().map(|v| ExprKind::U8(v)),
                (false, 16) => value.parse().map(|v| ExprKind::U16(v)),
                (false, 32) => value.parse().map(|v| ExprKind::U32(v)),
                (false, 64) => value.parse().map(|v| ExprKind::U64(v)),
                _ => unreachable!(),
            }
            .map_err(|err| Error::InvalidIntLit { token, err })?;
            Ok(Expr {
                kind,
                assignable,
                typ: expected.clone(),
            })
        } else {
            Ok(Expr {
                kind: ExprKind::I32(0),
                assignable,
                typ: Rc::new(IntType::signed(32).into()),
            })
        }
    }

    fn analyze_float_lit(&self, token: &Token, expected: &Rc<Type>) -> Result<Expr, Error> {
        let token = token.clone();
        let assignable = false;

        if let Type::Float(FloatType { size }) = expected.as_ref() {
            let value = token.unwrap_value();
            let kind = match size {
                32 => value.parse().map(|v| ExprKind::F32(v)),
                64 => value.parse().map(|v| ExprKind::F64(v)),
                _ => unreachable!(),
            }
            .map_err(|err| Error::InvalidFloatLit { token, err })?;
            Ok(Expr {
                kind,
                assignable,
                typ: expected.clone(),
            })
        } else {
            Ok(Expr {
                kind: ExprKind::F64(0.0),
                assignable,
                typ: Rc::new(FloatType { size: 64 }.into()),
            })
        }
    }

    fn analyze_bool_lit(&self, token: &Token) -> Result<Expr, Error> {
        Ok(Expr {
            kind: ExprKind::Bool(token.kind == TokenKind::True),
            assignable: false,
            typ: Rc::new(Type::Bool),
        })
    }

    fn analyze_binary_expr(&self, binary: &BinaryNode, expected: &Rc<Type>) -> Result<Expr, Error> {
        let a = self.analyze(binary.a.as_ref(), expected.clone())?;
        let a_typ = a.typ.clone();

        let b = self.analyze(binary.b.as_ref(), a_typ.clone())?;
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
            TokenKind::And | TokenKind::Or => a_typ.is_bool() && b_typ.is_bool(),
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
            | TokenKind::Or => Rc::new(Type::Bool),
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

    fn analyze_unary_expr(&self, unary: &UnaryNode, expected: &Rc<Type>) -> Result<Expr, Error> {
        let val = self.analyze(unary.val.as_ref(), expected.clone())?;
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
            TokenKind::Not => Rc::new(Type::Bool),
            TokenKind::BitNot | TokenKind::Plus | TokenKind::Minus => val_type.clone(),
            _ => unreachable!(),
        };

        Ok(Expr {
            kind: ExprKind::Unary(Unary { op, val: Box::new(val) }),
            assignable: false,
            typ: t,
        })
    }

    fn analyze_func_call(&self, func_call: &FunctionCallNode, expected: &Rc<Type>) -> Result<Expr, Error> {
        let func = self.analyze(func_call.func.as_ref(), expected.clone())?;

        let fn_type = func.typ.try_unwrap_func().ok_or(Error::NotAFn)?;

        if fn_type.arguments.len() != func_call.args.len() {
            return Err(Error::FnCallArgNumMismatch);
        }

        let mut args = Vec::new();
        for (i, arg) in func_call.args.iter().enumerate() {
            let val = self.analyze(arg, expected.clone())?;

            let given_type = &val.typ;
            let required_type = &fn_type.arguments.get(i).unwrap().typ;
            if given_type != required_type {
                return Err(Error::MismatchType);
            }

            args.push(val);
        }

        let return_type = fn_type
            .return_type
            .as_ref()
            .map(Rc::clone)
            .unwrap_or(Rc::new(Type::Void));

        let func = Box::new(func.clone());

        Ok(Expr {
            kind: FunctionCall { func, args }.into(),
            assignable: false,
            typ: return_type,
        })
    }

    fn analyze_index(&self, index_node: &IndexNode) -> Result<Expr, Error> {
        let array = self.analyze(&index_node.array, Rc::new(Type::Void))?;

        let array_type = if let Some(typ) = array.typ.try_unwrap_array() {
            typ
        } else {
            return Err(Error::NotAnArray { expr: array });
        };

        let index = self.analyze(&index_node.index, Rc::new(IntType::signed(64).into()))?;
        if index.typ.as_ref() != &Type::Int(IntType::signed(64)) {
            return Err(Error::IndexIsNotAnInt { expr: index });
        }

        let typ = array_type.elem_type.clone();
        Ok(Expr {
            kind: ExprKind::Index(Index {
                array: Box::new(array),
                index: Box::new(index),
            }),
            assignable: true,
            typ,
        })
    }

    fn analyze_array(&self, array_node: &ArrayNode) -> Result<Expr, Error> {
        let elem_type = self.type_helper.get(&array_node.typ)?;

        let size = self.analyze(&array_node.size, Rc::new(IntType::signed(64).into()))?;
        if !size.typ.is_int() {
            return Err(Error::ArraySizeIsNotAnInt { expr: size });
        }

        Ok(Expr {
            kind: ExprKind::Array(Array {
                elem_type: Rc::clone(&elem_type),
                size: Box::new(size),
            }),
            assignable: true,
            typ: Rc::new(Type::Array(Rc::new(ArrayType { elem_type }))),
        })
    }

    fn analyze_cast(&self, cast: &CastNode, expected: &Rc<Type>) -> Result<Expr, Error> {
        let target = self.type_helper.get(&cast.target)?.clone();
        let val = self.analyze(cast.val.as_ref(), expected.clone())?;

        let target_ok = matches!(target.as_ref(), Type::Bool | Type::Int(_) | Type::Float(_));
        let expr_ok = matches!(val.typ.as_ref(), Type::Bool | Type::Int(_) | Type::Float(_));
        let can_cast = target_ok && expr_ok;
        if !can_cast {
            return Err(Error::CannotCast {
                pos: cast.val.pos.clone(),
                expr: val,
                target_type: target,
            });
        }

        Ok(Expr {
            kind: ExprKind::Cast(Cast {
                target: target.clone(),
                val: Box::new(val),
            }),
            assignable: false,
            typ: target,
        })
    }
}
