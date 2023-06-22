use crate::def::{DefDb, FuncId, GenFuncId, GlobalId};
use crate::error::Loc;
use crate::package::AstInfo;
use crate::scope::{get_object_from_expr, Object, Scope, ScopeDb};
use crate::symbol::{SymbolDb, SymbolId};
use crate::ty::{get_type_from_expr, is_assignable, BitSize, FloatSize, FloatType, IntType, Type, TypeArgsId, TypeId};
use crate::value::value_from_string_lit;
use magelang_syntax::{
    AstNode, BinaryExprNode, CallExprNode, CastExprNode, DerefExprNode, ExprNode, IndexExprNode, SelectionExprNode,
    StructLitNode, Token, TokenKind, UnaryExprNode,
};
use std::collections::HashMap;
use std::iter::zip;
use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
    pub type_id: TypeId,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Invalid,
    Nop(Box<Expr>),
    I8(u8),
    I16(u16),
    I32(u32),
    I64(u64),
    Isize(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    Zero(TypeId),
    StructLit(TypeId, Rc<[Expr]>),
    Bytes(Rc<[u8]>),

    Local(usize),
    Global(GlobalId),
    Func(FuncExprKind),

    GetElement(Box<Expr>, usize),
    GetIndex(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    ShiftLeft(Box<Expr>, Box<Expr>),
    ShiftRight(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NEq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    GEq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    LEq(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    BitNot(Box<Expr>),
    Not(Box<Expr>),
    Cast(Box<Expr>, TypeId),
}

#[derive(Debug)]
pub enum FuncExprKind {
    Concrete(FuncId),
    GenericInst(GenFuncId, TypeArgsId),
}

pub trait ExprDb: SymbolDb + DefDb + ScopeDb {
    fn get_global_expr(&self, global_id: GlobalId) -> Rc<Expr>;
}

fn get_global_expr(db: &impl ExprDb, global_id: GlobalId) -> Rc<Expr> {
    let node = db.get_ast_by_def_id(global_id.into()).expect("item is not a global");
    let node = node.as_global().expect("item is not a global");
    let ast_info = db.get_package_ast(global_id.package());
    let scope = db.get_package_scope(global_id.package());
    let type_id = db.get_global_type_id(global_id);
    if let Some(ref value) = node.value {
        Rc::new(get_expr_from_ast(db, &ast_info, &scope, value, Some(type_id)))
    } else {
        Rc::new(Expr {
            type_id,
            kind: ExprKind::Zero(type_id),
        })
    }
}

pub fn get_expr_from_ast(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    expr_node: &ExprNode,
    expected_type: Option<TypeId>,
) -> Expr {
    match expr_node {
        ExprNode::Ident(token) => get_expr_from_ident_node(db, ast_info, scope, token),
        ExprNode::IntegerLiteral(integer_lit) => get_expr_from_int_node(db, ast_info, integer_lit, expected_type),
        ExprNode::RealLiteral(token) => get_expr_from_float_node(db, ast_info, token, expected_type),
        ExprNode::BooleanLit(token) => get_expr_from_bool_node(db, token),
        ExprNode::StringLit(token) => get_expr_from_string_lit_node(db, token),
        ExprNode::Binary(node) => get_expr_from_binary_node(db, ast_info, scope, node, expected_type),
        ExprNode::Deref(node) => get_expr_from_deref_node(db, ast_info, scope, node),
        ExprNode::Unary(node) => get_expr_from_unary_node(db, ast_info, scope, node, expected_type),
        ExprNode::Call(node) => get_expr_from_call_node(db, ast_info, scope, node),
        ExprNode::Cast(node) => get_expr_from_cast_node(db, ast_info, scope, node),
        ExprNode::StructLit(node) => get_expr_from_struct_lit_node(db, ast_info, scope, node),
        ExprNode::Selection(node) => get_expr_from_selection_node(db, ast_info, scope, node),
        ExprNode::Index(node) => get_expr_from_index_node(db, ast_info, scope, node),
        ExprNode::Grouped(node) => get_expr_from_ast(db, ast_info, scope, &node.value, expected_type),
        ExprNode::ArrayPtr(node) => {
            db.not_a_value(Loc::new(ast_info.path, node.get_pos()));
            Expr {
                type_id: db.define_unknown_type(),
                kind: ExprKind::Invalid,
            }
        }
    }
}

fn get_expr_from_ident_node(db: &impl ExprDb, ast_info: &AstInfo, scope: &Scope, token: &Token) -> Expr {
    let name = db.define_symbol(token.value.clone());
    let Some(object) = scope.get(name) else {
        db.undeclared_symbol(Loc::new(ast_info.path, token.pos), &token.value);
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };

    match object {
        Object::Invalid
        | Object::Import { .. }
        | Object::GenericStruct { .. }
        | Object::GenericFunc { .. }
        | Object::Type(..) => {
            db.not_a_value(Loc::new(ast_info.path, token.pos));
            Expr {
                type_id: db.define_unknown_type(),
                kind: ExprKind::Invalid,
            }
        }
        Object::Local { ty, idx } => Expr {
            type_id: ty,
            kind: ExprKind::Local(idx),
        },
        Object::Global(global_id) => Expr {
            type_id: db.get_global_type_id(global_id),
            kind: ExprKind::Global(global_id),
        },
        Object::Func { func_id, is_native: _ } => Expr {
            type_id: db.get_func_type_id(func_id).into(),
            kind: ExprKind::Func(FuncExprKind::Concrete(func_id)),
        },
    }
}

fn get_expr_from_int_node(db: &impl ExprDb, ast_info: &AstInfo, tok: &Token, expected_type: Option<TypeId>) -> Expr {
    let expected_type = expected_type.map(|type_id| db.get_type(type_id));
    let mut expected_type = expected_type.unwrap_or(Rc::new(Type::Int(IntType::i64())));

    let kind = match expected_type.as_ref() {
        Type::Int(int_type) => match (int_type.sign, int_type.size) {
            (true, BitSize::ISize) => tok.value.parse::<i64>().map(|v| ExprKind::Isize(v as u64)),
            (true, BitSize::I32) => tok.value.parse::<i32>().map(|v| ExprKind::I32(v as u32)),
            (true, BitSize::I16) => tok.value.parse::<i16>().map(|v| ExprKind::I16(v as u16)),
            (true, BitSize::I8) => tok.value.parse::<i8>().map(|v| ExprKind::I8(v as u8)),
            (false, BitSize::ISize) => tok.value.parse::<i64>().map(|v| ExprKind::Isize(v as u64)),
            (false, BitSize::I64) => tok.value.parse::<i32>().map(|v| ExprKind::I64(v as u64)),
            (false, BitSize::I32) => tok.value.parse::<i32>().map(|v| ExprKind::I32(v as u32)),
            (false, BitSize::I16) => tok.value.parse::<i16>().map(|v| ExprKind::I16(v as u16)),
            (false, BitSize::I8) => tok.value.parse::<i8>().map(|v| ExprKind::I8(v as u8)),
            _ => {
                expected_type = Rc::new(Type::Int(IntType::i64()));
                tok.value.parse::<i64>().map(|v| ExprKind::I64(v as u64))
            }
        },
        Type::Pointer(..) => tok.value.parse::<u64>().map(ExprKind::Isize),
        _ => {
            expected_type = Rc::new(Type::Int(IntType::i64()));
            tok.value.parse::<i64>().map(|v| ExprKind::I64(v as u64))
        }
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            db.invalid_int_literal(Loc::new(ast_info.path, tok.pos), err);
            ExprKind::Invalid
        }
    };

    let type_id = db.define_type(expected_type);
    Expr { type_id, kind }
}

fn get_expr_from_float_node(db: &impl ExprDb, ast_info: &AstInfo, tok: &Token, expected_type: Option<TypeId>) -> Expr {
    let expected_type = expected_type.map(|type_id| db.get_type(type_id));
    let expected_type = expected_type.unwrap_or(Rc::new(Type::Float(FloatType::f64())));

    let kind = match expected_type.as_ref() {
        Type::Float(float_type) => match float_type.size {
            FloatSize::F32 => tok.value.parse::<f32>().map(ExprKind::F32),
            _ => tok.value.parse::<f64>().map(ExprKind::F64),
        },
        _ => tok.value.parse::<f64>().map(ExprKind::F64),
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            db.invalid_float_literal(Loc::new(ast_info.path, tok.pos), err);
            ExprKind::Invalid
        }
    };

    let type_id = db.define_type(expected_type);
    Expr { type_id, kind }
}

fn get_expr_from_bool_node(db: &impl ExprDb, tok: &Token) -> Expr {
    let kind = match tok.kind {
        TokenKind::True => ExprKind::Bool(true),
        TokenKind::False => ExprKind::Bool(false),
        _ => unreachable!("invalid state, not a boolean value"),
    };
    Expr {
        type_id: db.define_type(Rc::new(Type::Bool)),
        kind,
    }
}

fn get_expr_from_string_lit_node(db: &impl ExprDb, token: &Token) -> Expr {
    let u8_type_id = db.define_type(Rc::new(Type::Int(IntType::u8())));
    let type_id = db.define_array_ptr_type(u8_type_id);

    let Some(string_lit) = value_from_string_lit(&token.value) else {
        return Expr {
            type_id,
            kind: ExprKind::Invalid,
        };
    };

    Expr {
        type_id,
        kind: ExprKind::Bytes(string_lit),
    }
}

fn get_expr_from_binary_node(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &BinaryExprNode,
    expected_type: Option<TypeId>,
) -> Expr {
    let a = get_expr_from_ast(db, ast_info, scope, &node.a, expected_type);
    let b = get_expr_from_ast(db, ast_info, scope, &node.b, Some(a.type_id));

    let op_name = match node.op.kind {
        TokenKind::Add => "add",
        TokenKind::Sub => "sub",
        TokenKind::Mul => "mul",
        TokenKind::Div => "div",
        TokenKind::Mod => "mod",
        TokenKind::BitOr => "bitwise or",
        TokenKind::BitAnd => "bitwise and",
        TokenKind::BitXor => "bitwise xor",
        TokenKind::ShiftLeft => "shift left",
        TokenKind::ShiftRight => "shift right",
        TokenKind::And => "and",
        TokenKind::Or => "or",
        TokenKind::Eq => "eq",
        TokenKind::NEq => "neq",
        TokenKind::Gt => "gt",
        TokenKind::GEq => "geq",
        TokenKind::Lt => "lt",
        TokenKind::LEq => "leq",
        _ => unreachable!("found invalid token for binary operator"),
    };

    let estimated_type = match node.op.kind {
        TokenKind::Eq | TokenKind::NEq | TokenKind::Gt | TokenKind::GEq | TokenKind::Lt | TokenKind::LEq => {
            db.define_type(Rc::new(Type::Bool))
        }
        _ => a.type_id,
    };

    let a_ty = db.get_type(a.type_id);
    let b_ty = db.get_type(b.type_id);

    if a_ty != b_ty {
        db.binop_type_mismatch(
            Loc::new(ast_info.path, node.get_pos()),
            op_name,
            a_ty.display(db),
            b_ty.display(db),
        );
        return Expr {
            type_id: estimated_type,
            kind: ExprKind::Invalid,
        };
    }

    let result_ty = match node.op.kind {
        TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div => {
            if a_ty.is_arithmetic() {
                a_ty.as_ref().clone()
            } else {
                db.binop_type_unsupported(Loc::new(ast_info.path, node.get_pos()), op_name, a_ty.display(db));
                Type::Unknown
            }
        }
        TokenKind::Eq | TokenKind::NEq => Type::Bool,
        TokenKind::Gt | TokenKind::GEq | TokenKind::Lt | TokenKind::LEq => {
            if a_ty.is_arithmetic() {
                Type::Bool
            } else {
                db.binop_type_unsupported(Loc::new(ast_info.path, node.get_pos()), op_name, a_ty.display(db));
                Type::Unknown
            }
        }
        TokenKind::Mod
        | TokenKind::BitOr
        | TokenKind::BitAnd
        | TokenKind::BitXor
        | TokenKind::ShiftLeft
        | TokenKind::ShiftRight => {
            if a_ty.is_int() {
                a_ty.as_ref().clone()
            } else {
                db.binop_type_unsupported(Loc::new(ast_info.path, node.get_pos()), op_name, a_ty.display(db));
                Type::Unknown
            }
        }
        TokenKind::And | TokenKind::Not | TokenKind::Or => {
            if a_ty.is_bool() {
                Type::Bool
            } else {
                db.binop_type_unsupported(Loc::new(ast_info.path, node.get_pos()), op_name, a_ty.display(db));
                Type::Unknown
            }
        }
        op => unreachable!("token {op} is not a binary operator"),
    };

    let result_type_id = db.define_type(Rc::new(result_ty));
    let a = Box::new(a);
    let b = Box::new(b);
    let expr_kind = match node.op.kind {
        TokenKind::Add => ExprKind::Add(a, b),
        TokenKind::Sub => ExprKind::Sub(a, b),
        TokenKind::Mul => ExprKind::Mul(a, b),
        TokenKind::Div => ExprKind::Div(a, b),
        TokenKind::Mod => ExprKind::Mod(a, b),
        TokenKind::BitOr => ExprKind::BitOr(a, b),
        TokenKind::BitAnd => ExprKind::BitAnd(a, b),
        TokenKind::BitXor => ExprKind::BitXor(a, b),
        TokenKind::ShiftLeft => ExprKind::ShiftLeft(a, b),
        TokenKind::ShiftRight => ExprKind::ShiftRight(a, b),
        TokenKind::And => ExprKind::And(a, b),
        TokenKind::Or => ExprKind::Or(a, b),
        TokenKind::Eq => ExprKind::Eq(a, b),
        TokenKind::NEq => ExprKind::NEq(a, b),
        TokenKind::Gt => ExprKind::Gt(a, b),
        TokenKind::GEq => ExprKind::GEq(a, b),
        TokenKind::Lt => ExprKind::Lt(a, b),
        TokenKind::LEq => ExprKind::LEq(a, b),
        op => unreachable!("token {op} is not a binary operator"),
    };

    Expr {
        type_id: result_type_id,
        kind: expr_kind,
    }
}

fn get_expr_from_deref_node(db: &impl ExprDb, ast_info: &AstInfo, scope: &Rc<Scope>, node: &DerefExprNode) -> Expr {
    let value_ref = get_expr_from_ast(db, ast_info, scope, &node.value, None);
    let value_ref_ty = db.get_type(value_ref.type_id);
    let Type::Pointer(element_type_id) = value_ref_ty.as_ref() else {
        db.cannot_deref_non_pointer(Loc::new(ast_info.path, node.value.get_pos()));
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };
    Expr {
        type_id: *element_type_id,
        kind: ExprKind::Deref(Box::new(value_ref)),
    }
}

fn get_expr_from_unary_node(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &UnaryExprNode,
    expected_type: Option<TypeId>,
) -> Expr {
    let value = get_expr_from_ast(db, ast_info, scope, &node.value, expected_type);
    let ty = db.get_type(value.type_id);

    let op_name = match node.op.kind {
        TokenKind::BitNot => "bit not",
        TokenKind::Sub => "sub",
        TokenKind::Add => "add",
        TokenKind::Not => "not",
        op => unreachable!("token {op} is not a unary operator"),
    };

    let is_bool = ty.is_bool();
    let is_arithmetic = ty.is_arithmetic();
    let is_int = ty.is_int();

    let value = Box::new(value);
    let type_id = value.type_id;
    let (kind, is_valid) = match node.op.kind {
        TokenKind::BitNot => (ExprKind::BitNot(value), is_int),
        TokenKind::Sub => (ExprKind::Neg(value), is_arithmetic),
        TokenKind::Add => (ExprKind::Nop(value), is_arithmetic),
        TokenKind::Not => (ExprKind::Not(value), is_bool),
        op => unreachable!("token {op} is not a unary operator"),
    };

    if !is_valid {
        db.unop_type_unsupported(Loc::new(ast_info.path, node.get_pos()), op_name, ty.display(db));
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    }

    Expr { type_id, kind }
}

fn get_expr_from_call_node(db: &impl ExprDb, ast_info: &AstInfo, scope: &Rc<Scope>, node: &CallExprNode) -> Expr {
    let func_expr = get_expr_from_ast(db, ast_info, scope, &node.target, None);
    let func_type = db.get_type(func_expr.type_id);
    let Some(func_type) = func_type.as_func() else {
        db.not_callable(Loc::new(ast_info.path, node.target.get_pos()));
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };

    if node.arguments.len() != func_type.params.len() {
        db.wrong_number_of_arguments(
            Loc::new(ast_info.path, node.pos.clone()),
            func_type.params.len(),
            node.arguments.len(),
        );
        return Expr {
            type_id: func_type.return_type,
            kind: ExprKind::Invalid,
        };
    }

    let mut arguments = vec![];
    for (param, arg) in zip(func_type.params.iter(), node.arguments.iter()) {
        let param_ty = db.get_type(*param);

        let arg_expr = get_expr_from_ast(db, ast_info, scope, arg, Some(*param));
        if !is_assignable(db, *param, arg_expr.type_id) {
            db.type_mismatch(
                Loc::new(ast_info.path, arg.get_pos()),
                param_ty.display(db),
                db.get_type(arg_expr.type_id).display(db),
            );
            return Expr {
                type_id: *param,
                kind: ExprKind::Invalid,
            };
        }

        arguments.push(arg_expr);
    }

    Expr {
        type_id: func_type.return_type,
        kind: ExprKind::Call(Box::new(func_expr), arguments),
    }
}

fn get_expr_from_cast_node(db: &impl ExprDb, ast_info: &AstInfo, scope: &Rc<Scope>, node: &CastExprNode) -> Expr {
    let target_type_id = get_type_from_expr(db, ast_info, scope, &node.target);
    let target_type = db.get_type(target_type_id);

    let value = get_expr_from_ast(db, ast_info, scope, &node.value, None);
    let value_type = db.get_type(value.type_id);

    let kind = if value_type.is_arithmetic() && target_type.is_arithmetic() {
        ExprKind::Cast(Box::new(value), target_type_id)
    } else {
        db.unsupported_casting(
            Loc::new(ast_info.path, node.get_pos()),
            value_type.display(db),
            target_type.display(db),
        );
        ExprKind::Invalid
    };

    Expr {
        type_id: target_type_id,
        kind,
    }
}

fn get_expr_from_index_node(db: &impl ExprDb, ast_info: &AstInfo, scope: &Rc<Scope>, node: &IndexExprNode) -> Expr {
    if let Some(expr) = get_func_instance_from_expr(db, ast_info, scope, node) {
        return expr;
    }

    let value = get_expr_from_ast(db, ast_info, scope, &node.value, None);
    let ty = db.get_type(value.type_id);
    match ty.as_ref() {
        Type::ArrayPtr(element_type_id) => {
            if node.index.is_empty() {
                db.unexpected_index_num(Loc::new(ast_info.path, node.get_pos()), 1, 0);
                return Expr {
                    type_id: *element_type_id,
                    kind: ExprKind::Invalid,
                };
            }
            if node.index.len() != 1 {
                db.unexpected_index_num(Loc::new(ast_info.path, node.get_pos()), 1, node.index.len());
            }

            let index = get_expr_from_ast(db, ast_info, scope, &node.index[0], None);
            let index_type = db.get_type(index.type_id);

            if !index_type.is_int() {
                db.non_int_index(Loc::new(ast_info.path, node.index[0].get_pos()));
                return Expr {
                    type_id: *element_type_id,
                    kind: ExprKind::Invalid,
                };
            }

            Expr {
                type_id: *element_type_id,
                kind: ExprKind::GetIndex(Box::new(value), Box::new(index)),
            }
        }
        Type::Unknown => value,
        _ => {
            db.not_indexable(Loc::new(ast_info.path, node.get_pos()));
            Expr {
                type_id: db.define_unknown_type(),
                kind: ExprKind::Invalid,
            }
        }
    }
}

fn get_func_instance_from_expr(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &IndexExprNode,
) -> Option<Expr> {
    let object = get_object_from_expr(db, scope, &node.value);
    let Object::GenericFunc{typeparams, gen_func_id, is_native} = object else {
        return None;
    };

    if typeparams.len() != node.index.len() {
        db.wrong_number_of_type_arguments(
            Loc::new(ast_info.path, node.get_pos()),
            typeparams.len(),
            node.index.len(),
        );
    }

    let typeargs: Rc<[TypeId]> = node
        .index
        .iter()
        .map(|node| get_type_from_expr(db, ast_info, scope, node))
        .collect();
    let typeargs_id = db.define_typeargs(typeargs);

    Some(Expr {
        type_id: db.get_generic_func_inst_type_id(gen_func_id, typeargs_id).into(),
        kind: ExprKind::Func(FuncExprKind::GenericInst(gen_func_id, typeargs_id)),
    })
}

fn get_expr_from_struct_lit_node(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &StructLitNode,
) -> Expr {
    let type_id = get_type_from_expr(db, ast_info, scope, &node.target);
    let ty = db.get_type(type_id);

    // TODO: handle generic struct here.
    let Some(struct_type) = ty.as_struct() else {
        db.not_a_struct(Loc::new(ast_info.path, node.target.get_pos()), ty.display(db));
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };

    let struct_field = db.get_struct_field(type_id.into());

    let mut values = HashMap::<SymbolId, Expr>::default();
    for element in &node.elements {
        let field_name = db.define_symbol(element.key.value.clone());
        let type_id = struct_field.fields.get(&field_name).cloned().unwrap_or_else(|| {
            db.no_such_field(Loc::new(ast_info.path, element.key.pos), &element.key.value);
            db.define_unknown_type()
        });
        let value = get_expr_from_ast(db, ast_info, scope, &element.value, Some(type_id));

        let value = if !is_assignable(db, value.type_id, type_id) {
            db.type_mismatch(
                Loc::new(ast_info.path, element.value.get_pos()),
                db.get_type(type_id).display(db),
                db.get_type(value.type_id).display(db),
            );
            Expr {
                type_id,
                kind: ExprKind::Invalid,
            }
        } else {
            value
        };

        values.insert(field_name, value);
    }

    let mut full_values = Vec::default();
    for (field_name, type_id) in struct_field.fields.iter() {
        if let Some(value) = values.remove(field_name) {
            full_values.push(value);
        } else {
            full_values.push(Expr {
                type_id: *type_id,
                kind: ExprKind::Zero(*type_id),
            });
        }
    }

    Expr {
        type_id,
        kind: ExprKind::StructLit(type_id, full_values.into()),
    }
}

fn get_expr_from_selection_node(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &SelectionExprNode,
) -> Expr {
    if let Some(expr) = get_expr_from_package_selection(db, ast_info, scope, node) {
        return expr;
    }

    let mut value_expr = get_expr_from_ast(db, ast_info, scope, &node.value, None);
    while let Type::Pointer(element_type_id) = db.get_type(value_expr.type_id).as_ref() {
        value_expr = Expr {
            type_id: *element_type_id,
            kind: ExprKind::Deref(Box::new(value_expr)),
        }
    }

    let value_type = db.get_type(value_expr.type_id);
    // TODO: handle generic struct here.
    let Some(struct_type) = value_type.as_struct() else {
        db.not_a_struct(Loc::new(ast_info.path, node.get_pos()), value_type.display(db));
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };
    let struct_field = db.get_struct_field(value_expr.type_id.into());

    let selection_id = db.define_symbol(node.selection.value.clone());
    let Some((idx, _, type_id)) = struct_field.fields.get_full(&selection_id) else {
        db.no_such_field(Loc::new(ast_info.path, node.selection.pos), &node.selection.value);
        return Expr {
            type_id: db.define_unknown_type(),
            kind: ExprKind::Invalid,
        };
    };
    Expr {
        type_id: *type_id,
        kind: ExprKind::GetElement(Box::new(value_expr), idx),
    }
}

fn get_expr_from_package_selection(
    db: &impl ExprDb,
    ast_info: &AstInfo,
    scope: &Scope,
    node: &SelectionExprNode,
) -> Option<Expr> {
    let ExprNode::Ident(token) = node.value.as_ref() else { return None; };
    let ident_name = db.define_symbol(token.value.clone());
    let value_object = scope.get(ident_name)?;
    let package_name = value_object.as_import()?;
    let package_scope = db.get_package_scope(package_name);
    let selection_name = db.define_symbol(node.selection.value.clone());
    let Some(object) = package_scope.get(selection_name) else {
        db.undeclared_symbol(Loc::new(ast_info.path, node.selection.pos), &node.selection.value);
        return Some(Expr{type_id: db.define_unknown_type(), kind: ExprKind::Invalid});
    };

    let expr = match object {
        Object::Invalid
        | Object::Import { .. }
        | Object::GenericFunc { .. }
        | Object::GenericStruct { .. }
        | Object::Type(..) => {
            db.not_a_value(Loc::new(ast_info.path, node.selection.pos));
            Expr {
                type_id: db.define_unknown_type(),
                kind: ExprKind::Invalid,
            }
        }
        Object::Local { ty, idx } => Expr {
            type_id: ty,
            kind: ExprKind::Local(idx),
        },
        Object::Global(object_id) => Expr {
            type_id: db.get_global_type_id(object_id),
            kind: ExprKind::Global(object_id),
        },
        Object::Func { func_id, is_native: _ } => Expr {
            type_id: db.get_func_type_id(func_id).into(),
            kind: ExprKind::Func(FuncExprKind::Concrete(func_id)),
        },
    };

    Some(expr)
}
