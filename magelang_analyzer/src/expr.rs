use crate::analyze::TypeCheckContext;
use crate::errors::SemanticError;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::scope::Object;
use crate::symbols::SymbolId;
use crate::ty::{
    display_type, display_type_id, get_type_from_node, is_type_assignable, substitute_generic_args,
    BitSize, FloatType, Type, TypeArgsId, TypeId,
};
use crate::value::value_from_string_lit;
use magelang_syntax::{
    BinaryExprNode, CallExprNode, CastExprNode, DerefExprNode, ErrorReporter, ExprNode,
    IndexExprNode, InstanceExprNode, SelectionExprNode, StructExprNode, Token, TokenKind,
    UnaryExprNode,
};
use std::collections::HashMap;
use std::iter::zip;
use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
    pub ty: TypeId,
    pub kind: ExprKind,
    pub assignable: bool,
}

#[derive(Debug)]
pub enum ExprKind {
    Invalid,

    ConstI8(u8),
    ConstI16(u16),
    ConstI32(u32),
    ConstI64(u64),
    ConstIsize(u64),
    ConstF32(f32),
    ConstF64(f64),
    ConstBool(bool),
    Zero,
    StructLit(TypeId, Vec<Expr>),
    Bytes(Rc<[u8]>),

    Local(usize),
    Global(DefId),
    Func(DefId),
    FuncInst(DefId, TypeArgsId),

    GetElement(Box<Expr>, usize),
    GetElementAddr(Box<Expr>, usize),
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

pub fn get_expr_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &ExprNode,
) -> Expr {
    match node {
        ExprNode::Ident(token) => get_expr_from_ident(ctx, expected_type, token),
        ExprNode::Integer(token) => get_expr_from_int_lit(ctx, expected_type, token),
        ExprNode::Frac(token) => get_expr_from_float_lit(ctx, expected_type, token),
        ExprNode::Bool(token) => get_expr_from_boolean_lit(ctx, token),
        ExprNode::String(token) => get_expr_from_string_lit(ctx, token),
        ExprNode::Binary(node) => get_expr_from_binary_node(ctx, expected_type, node),
        ExprNode::Deref(node) => get_expr_from_deref_node(ctx, expected_type, node),
        ExprNode::Unary(node) => get_expr_from_unary_node(ctx, expected_type, node),
        ExprNode::Call(node) => get_expr_from_call_node(ctx, expected_type, node),
        ExprNode::Cast(node) => get_expr_from_cast_node(ctx, expected_type, node),
        ExprNode::Struct(struct_lit_node) => get_expr_from_struct_lit_node(ctx, struct_lit_node),
        ExprNode::Selection(selection_node) => {
            get_expr_from_selection_node(ctx, expected_type, selection_node)
        }
        ExprNode::Index(node) => get_expr_from_index_node(ctx, expected_type, node),
        ExprNode::Instance(node) => get_expr_from_instance_node(ctx, node),
        ExprNode::Grouped(node) => get_expr_from_node(ctx, expected_type, node),
    }
}

fn get_expr_from_ident<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    token: &Token,
) -> Expr {
    let name = ctx.symbols.define(&token.value);
    let Some(obj) = ctx.scope.lookup(name) else {
        ctx.errors.undeclared_symbol(token.pos, &token.value);
        return Expr {
            ty: expected_type.unwrap_or(ctx.types.define(Type::Unknown)),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    match obj {
        Object::Import(..)
        | Object::GenericStruct(..)
        | Object::GenericFunc(..)
        | Object::Struct(..)
        | Object::Type(..) => {
            ctx.errors.expr_not_a_value(token.pos);
            Expr {
                ty: expected_type.unwrap_or(ctx.types.define(Type::Unknown)),
                kind: ExprKind::Invalid,
                assignable: false,
            }
        }
        Object::Global(global_obj) => Expr {
            ty: *global_obj.ty.get().expect("missing global type"),
            kind: ExprKind::Global(global_obj.def_id),
            assignable: true,
        },
        Object::Local(local_obj) => Expr {
            ty: local_obj.ty,
            kind: ExprKind::Local(local_obj.id),
            assignable: true,
        },
        Object::Func(func_obj) => Expr {
            ty: *func_obj.ty.get().expect("missing function type"),
            kind: ExprKind::Func(func_obj.def_id),
            assignable: false,
        },
    }
}

fn get_expr_from_int_lit<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    token: &Token,
) -> Expr {
    let (sign, bit_size) = if let Some(type_id) = expected_type {
        let ty = ctx.types.get(type_id);
        match ty.as_ref() {
            Type::Int(sign, bit_size) => (*sign, *bit_size),
            Type::Ptr(..) | Type::ArrayPtr(..) => (false, BitSize::ISize),
            _ => (true, BitSize::ISize),
        }
    } else {
        (true, BitSize::ISize)
    };

    let kind = match (sign, bit_size) {
        (true, BitSize::ISize) => token
            .value
            .parse::<i64>()
            .map(|v| ExprKind::ConstIsize(v as u64)),
        (true, BitSize::I64) => token
            .value
            .parse::<i64>()
            .map(|v| ExprKind::ConstI64(v as u64)),
        (true, BitSize::I32) => token
            .value
            .parse::<i32>()
            .map(|v| ExprKind::ConstI32(v as u32)),
        (true, BitSize::I16) => token
            .value
            .parse::<i16>()
            .map(|v| ExprKind::ConstI16(v as u16)),
        (true, BitSize::I8) => token
            .value
            .parse::<i8>()
            .map(|v| ExprKind::ConstI8(v as u8)),
        (false, BitSize::ISize) => token
            .value
            .parse::<i64>()
            .map(|v| ExprKind::ConstIsize(v as u64)),
        (false, BitSize::I64) => token
            .value
            .parse::<i64>()
            .map(|v| ExprKind::ConstI64(v as u64)),
        (false, BitSize::I32) => token
            .value
            .parse::<i32>()
            .map(|v| ExprKind::ConstI32(v as u32)),
        (false, BitSize::I16) => token
            .value
            .parse::<i16>()
            .map(|v| ExprKind::ConstI16(v as u16)),
        (false, BitSize::I8) => token
            .value
            .parse::<i8>()
            .map(|v| ExprKind::ConstI8(v as u8)),
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_int_literal(token.pos, err);
            ExprKind::Invalid
        }
    };

    let type_id = ctx.types.define(Type::Int(sign, bit_size));
    Expr {
        ty: type_id,
        kind,
        assignable: false,
    }
}

fn get_expr_from_float_lit<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    token: &Token,
) -> Expr {
    let float_type = if let Some(type_id) = expected_type {
        let ty = ctx.types.get(type_id);
        match ty.as_ref() {
            Type::Float(float_ty) => *float_ty,
            _ => FloatType::F64,
        }
    } else {
        FloatType::F64
    };

    let kind = match float_type {
        FloatType::F32 => token.value.parse::<f32>().map(ExprKind::ConstF32),
        FloatType::F64 => token.value.parse::<f64>().map(ExprKind::ConstF64),
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_float_literal(token.pos, err);
            ExprKind::Invalid
        }
    };

    let type_id = ctx.types.define(Type::Float(float_type));
    Expr {
        ty: type_id,
        kind,
        assignable: false,
    }
}

fn get_expr_from_boolean_lit<E: ErrorReporter>(ctx: &TypeCheckContext<E>, token: &Token) -> Expr {
    let kind = match token.kind {
        TokenKind::True => ExprKind::ConstBool(true),
        TokenKind::False => ExprKind::ConstBool(false),
        _ => unreachable!("invalid ast: not a boolean literal"),
    };
    Expr {
        ty: ctx.types.define(Type::Bool),
        kind,
        assignable: false,
    }
}

fn get_expr_from_string_lit<E: ErrorReporter>(ctx: &TypeCheckContext<E>, token: &Token) -> Expr {
    let u8_type_id = ctx.types.define(Type::Int(false, BitSize::I8));
    let type_id = ctx.types.define(Type::ArrayPtr(u8_type_id));

    let Some(mut bytes) = value_from_string_lit(&token.value) else {
        return Expr {
            ty: type_id,
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };
    bytes.push(0);

    Expr {
        ty: type_id,
        kind: ExprKind::Bytes(bytes.into()),
        assignable: false,
    }
}

fn get_expr_from_binary_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &BinaryExprNode,
) -> Expr {
    let a = get_expr_from_node(ctx, expected_type, &node.a);
    let b = get_expr_from_node(ctx, Some(a.ty), &node.b);

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
        TokenKind::Eq
        | TokenKind::NEq
        | TokenKind::Gt
        | TokenKind::GEq
        | TokenKind::Lt
        | TokenKind::LEq => ctx.types.define(Type::Bool),
        _ => a.ty,
    };

    let a_ty = ctx.types.get(a.ty);
    let b_ty = ctx.types.get(b.ty);

    if a_ty != b_ty {
        ctx.errors.binop_type_mismatch(
            node.a.pos(),
            op_name,
            display_type(ctx, &a_ty),
            display_type(ctx, &b_ty),
        );
        return Expr {
            ty: estimated_type,
            kind: ExprKind::Invalid,
            assignable: false,
        };
    }

    let result_ty = match node.op.kind {
        TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div => {
            if a_ty.is_arithmetic() {
                a_ty.as_ref().clone()
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, display_type(ctx, &a_ty));
                Type::Unknown
            }
        }
        TokenKind::Eq | TokenKind::NEq => Type::Bool,
        TokenKind::Gt | TokenKind::GEq | TokenKind::Lt | TokenKind::LEq => {
            if a_ty.is_arithmetic() {
                Type::Bool
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, display_type(ctx, &a_ty));
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
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, display_type(ctx, &a_ty));
                Type::Unknown
            }
        }
        TokenKind::And | TokenKind::Not | TokenKind::Or => {
            if a_ty.is_bool() {
                Type::Bool
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, display_type(ctx, &a_ty));
                Type::Unknown
            }
        }
        op => unreachable!("token {op} is not a binary operator"),
    };

    let result_type_id = ctx.types.define(result_ty);
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
        ty: result_type_id,
        kind: expr_kind,
        assignable: false,
    }
}

fn get_expr_from_deref_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &DerefExprNode,
) -> Expr {
    let value = get_expr_from_node(ctx, expected_type, &node.value);
    let ty = ctx.types.get(value.ty);
    let Type::Ptr(element_ty) = ty.as_ref() else {
        ctx.errors.deref_non_pointer(node.pos);
        return Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };
    Expr {
        ty: *element_ty,
        kind: ExprKind::Deref(Box::new(value)),
        assignable: true,
    }
}

fn get_expr_from_unary_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &UnaryExprNode,
) -> Expr {
    let value = get_expr_from_node(ctx, expected_type, &node.value);
    let ty = ctx.types.get(value.ty);

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
    let type_id = value.ty;
    let (kind, is_valid) = match node.op.kind {
        TokenKind::BitNot => (ExprKind::BitNot(value), is_int),
        TokenKind::Sub => (ExprKind::Neg(value), is_arithmetic),
        TokenKind::Add => (value.kind, is_arithmetic),
        TokenKind::Not => (ExprKind::Not(value), is_bool),
        op => unreachable!("token {op} is not a unary operator"),
    };

    if !is_valid {
        ctx.errors
            .unop_type_unsupported(node.op.pos, op_name, display_type(ctx, &ty));
        return Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    }

    Expr {
        ty: type_id,
        kind,
        assignable: false,
    }
}

fn get_expr_from_call_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &CallExprNode,
) -> Expr {
    let func_expr = get_expr_from_node(ctx, expected_type, &node.callee);
    let func_type = ctx.types.get(func_expr.ty);

    let Some(func_type) = func_type.as_func() else {
        if !func_type.is_unknown() {
            ctx.errors.not_callable(node.callee.pos());
        }
        return Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    if node.arguments.len() != func_type.params.len() {
        ctx.errors.wrong_number_of_arguments(
            node.pos,
            func_type.params.len(),
            node.arguments.len(),
        );
    }

    let mut arguments = Vec::default();
    for (i, arg) in node.arguments.iter().enumerate() {
        let arg_expr = get_expr_from_node(ctx, func_type.params.get(i).cloned(), arg);
        arguments.push(arg_expr);
    }

    for (i, (arg, param)) in zip(&arguments, &func_type.params).enumerate() {
        if !is_type_assignable(ctx, *param, arg.ty) {
            let param_ty = ctx.types.get(*param);
            ctx.errors.type_mismatch(
                node.arguments[i].pos(),
                display_type(ctx, &param_ty),
                display_type_id(ctx, arg.ty),
            );
        }
    }

    Expr {
        ty: func_type.return_type,
        kind: ExprKind::Call(Box::new(func_expr), arguments),
        assignable: false,
    }
}

fn get_expr_from_cast_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &CastExprNode,
) -> Expr {
    let target_type_id = get_type_from_node(ctx, &node.target);
    let target_type = ctx.types.get(target_type_id);

    let value = get_expr_from_node(ctx, expected_type, &node.value);
    let value_type = ctx.types.get(value.ty);

    let valid_casting = (value_type.is_integral() && target_type.is_integral())
        || (value_type.is_float() && target_type.is_float())
        || value_type.is_unknown()
        || target_type.is_unknown();

    let kind = if valid_casting {
        ExprKind::Cast(Box::new(value), target_type_id)
    } else {
        ctx.errors.casting_unsupported(
            node.value.pos(),
            display_type(ctx, &value_type),
            display_type(ctx, &target_type),
        );
        ExprKind::Invalid
    };

    Expr {
        ty: target_type_id,
        kind,
        assignable: false,
    }
}

fn get_expr_from_struct_lit_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    struct_lit_node: &StructExprNode,
) -> Expr {
    let type_id = get_type_from_node(ctx, &struct_lit_node.target);
    let ty = ctx.types.get(type_id);

    let struct_type = match ty.as_ref() {
        Type::NamedStruct(named_struct_type) => {
            named_struct_type.body.get().expect("missing struct body")
        }
        Type::NamedStructInst(named_struct_inst) => &named_struct_inst.body,
        _ => {
            ctx.errors.non_struct_type(struct_lit_node.target.pos());
            return Expr {
                ty: ctx.types.define(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            };
        }
    };

    let mut values = HashMap::<SymbolId, Expr>::default();
    for element in &struct_lit_node.elements {
        let field_name = ctx.symbols.define(&element.key.value);
        let type_id = struct_type
            .fields
            .get(&field_name)
            .cloned()
            .unwrap_or_else(|| {
                ctx.errors
                    .undeclared_field(element.key.pos, &element.key.value);
                ctx.types.define(Type::Unknown)
            });
        let value = get_expr_from_node(ctx, Some(type_id), &element.value);

        let value = if !is_type_assignable(ctx, type_id, value.ty) {
            ctx.errors.type_mismatch(
                element.value.pos(),
                display_type_id(ctx, type_id),
                display_type_id(ctx, value.ty),
            );
            Expr {
                ty: ctx.types.define(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            }
        } else {
            value
        };

        values.insert(field_name, value);
    }

    let mut full_values = Vec::default();
    for (field_name, type_id) in &struct_type.fields {
        if let Some(value) = values.remove(field_name) {
            full_values.push(value)
        } else {
            full_values.push(Expr {
                ty: *type_id,
                kind: ExprKind::Zero,
                assignable: false,
            })
        }
    }

    Expr {
        ty: type_id,
        kind: ExprKind::StructLit(type_id, full_values),
        assignable: false,
    }
}

fn get_expr_from_selection_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    selection_node: &SelectionExprNode,
) -> Expr {
    if let Some(obj) = get_object_from_selection(ctx, selection_node) {
        match obj {
            Object::Global(global_obj) => Expr {
                ty: *global_obj.ty.get().expect("missing global type"),
                kind: ExprKind::Global(global_obj.def_id),
                assignable: true,
            },
            Object::Local(local_obj) => Expr {
                ty: local_obj.ty,
                kind: ExprKind::Local(local_obj.id),
                assignable: true,
            },
            Object::Func(func_obj) => Expr {
                ty: *func_obj.ty.get().expect("missing function type"),
                kind: ExprKind::Func(func_obj.def_id),
                assignable: false,
            },
            Object::Import(..)
            | Object::Struct(..)
            | Object::Type(..)
            | Object::GenericFunc(..)
            | Object::GenericStruct(..) => {
                ctx.errors.expr_not_a_value(selection_node.value.pos());
                Expr {
                    ty: ctx.types.define(Type::Unknown),
                    kind: ExprKind::Invalid,
                    assignable: false,
                }
            }
        }
    } else {
        let value = get_expr_from_node(ctx, expected_type, &selection_node.value);

        let mut ty = ctx.types.get(value.ty);
        let mut is_ptr = false;

        if let Type::Ptr(element_type_id) = ty.as_ref() {
            let element_ty = ctx.types.get(*element_type_id);
            is_ptr = true;
            ty = element_ty;
        }

        let struct_type = match ty.as_ref() {
            Type::NamedStruct(named_struct_type) => {
                named_struct_type.body.get().expect("missing struct body")
            }
            Type::NamedStructInst(named_struct_inst) => &named_struct_inst.body,
            _ => {
                ctx.errors.non_field_type(
                    selection_node.selection.pos,
                    &selection_node.selection.value,
                );
                return Expr {
                    ty: ctx.types.define(Type::Unknown),
                    kind: ExprKind::Invalid,
                    assignable: false,
                };
            }
        };

        let selection_name = ctx.symbols.define(&selection_node.selection.value);
        let Some((idx, _, field_type_id)) = struct_type.fields.get_full(&selection_name) else {
            ctx.errors.undeclared_field(
                selection_node.selection.pos,
                &selection_node.selection.value,
            );
            return Expr {
                ty: ctx.types.define(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            };
        };

        let assignable = value.assignable;
        if is_ptr {
            Expr {
                ty: ctx.types.define(Type::Ptr(*field_type_id)),
                kind: ExprKind::GetElementAddr(Box::new(value), idx),
                assignable,
            }
        } else {
            Expr {
                ty: *field_type_id,
                kind: ExprKind::GetElement(Box::new(value), idx),
                assignable,
            }
        }
    }
}

fn get_expr_from_index_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    expected_type: Option<TypeId>,
    node: &IndexExprNode,
) -> Expr {
    let value = get_expr_from_node(ctx, expected_type, &node.value);
    let ty = ctx.types.get(value.ty);

    match ty.as_ref() {
        Type::ArrayPtr(element_type_id) => {
            let index = get_expr_from_node(ctx, expected_type, &node.index);
            let index_type = ctx.types.get(index.ty);

            if !index_type.is_int() {
                ctx.errors.non_int_index(node.index.pos());
                return Expr {
                    ty: *element_type_id,
                    kind: ExprKind::Invalid,
                    assignable: false,
                };
            }

            Expr {
                ty: ctx.types.define(Type::Ptr(*element_type_id)),
                kind: ExprKind::GetIndex(Box::new(value), Box::new(index)),
                assignable: false,
            }
        }
        Type::Unknown => Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        },
        _ => {
            ctx.errors.not_indexable(node.value.pos());
            Expr {
                ty: ctx.types.define(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            }
        }
    }
}

fn get_expr_from_instance_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    node: &InstanceExprNode,
) -> Expr {
    let Some(obj) = get_object_from_expr(ctx, &node.value) else {
        ctx.errors.non_generic_value(node.value.pos());
        return Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    let Some(generic_func) = obj.as_generic_func() else {
        ctx.errors.non_generic_value(node.value.pos());
        return Expr {
            ty: ctx.types.define(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    let expected_type_param = generic_func.signature.type_params.len();
    let provided_type_param = node.args.len();
    if expected_type_param != provided_type_param {
        ctx.errors.type_arguments_count_mismatch(
            node.value.pos(),
            expected_type_param,
            provided_type_param,
        );
    }

    let type_id = *generic_func
        .ty
        .get()
        .expect("missing generic function type");
    let type_args: Vec<TypeId> = node
        .args
        .iter()
        .map(|type_expr| get_type_from_node(ctx, type_expr))
        .collect();
    let instance_type_id = substitute_generic_args(ctx, &type_args, type_id);

    let typeargs_id = ctx.typeargs.define(&type_args);
    Expr {
        ty: instance_type_id,
        kind: ExprKind::FuncInst(generic_func.def_id, typeargs_id),
        assignable: false,
    }
}

fn get_object_from_expr<'ctx, E>(
    ctx: &'ctx TypeCheckContext<'ctx, E>,
    node: &ExprNode,
) -> Option<&'ctx Object> {
    match node {
        ExprNode::Ident(token) => ctx.scope.lookup(ctx.symbols.define(&token.value)),
        ExprNode::Selection(node) => get_object_from_selection(ctx, node),
        _ => None,
    }
}

fn get_object_from_selection<'ctx, E>(
    ctx: &'ctx TypeCheckContext<'ctx, E>,
    node: &SelectionExprNode,
) -> Option<&'ctx Object> {
    let ExprNode::Ident(token) = node.value.as_ref() else {
        return None;
    };
    let ident_name = ctx.symbols.define(&token.value);
    let object = ctx.scope.lookup(ident_name)?;
    let import_object = object.as_import()?;
    let package_scope = ctx.package_scopes.get(&import_object.package)?;
    let selection = ctx.symbols.define(&node.selection.value);
    package_scope.lookup(selection)
}

pub fn monomorphize_expr<E>(ctx: &TypeCheckContext<E>, arg_table: &[TypeId], expr: &Expr) -> Expr {
    let type_id = substitute_generic_args(ctx, arg_table, expr.ty);

    let kind = match &expr.kind {
        ExprKind::Invalid => ExprKind::Invalid,
        ExprKind::ConstI8(val) => ExprKind::ConstI8(*val),
        ExprKind::ConstI16(val) => ExprKind::ConstI16(*val),
        ExprKind::ConstI32(val) => ExprKind::ConstI32(*val),
        ExprKind::ConstI64(val) => ExprKind::ConstI64(*val),
        ExprKind::ConstIsize(val) => ExprKind::ConstIsize(*val),
        ExprKind::ConstF32(val) => ExprKind::ConstF32(*val),
        ExprKind::ConstF64(val) => ExprKind::ConstF64(*val),
        ExprKind::ConstBool(val) => ExprKind::ConstBool(*val),
        ExprKind::Zero => ExprKind::Zero,
        ExprKind::StructLit(type_id, values) => {
            let type_id = substitute_generic_args(ctx, arg_table, *type_id);
            let values = values
                .iter()
                .map(|val| monomorphize_expr(ctx, arg_table, val))
                .collect();
            ExprKind::StructLit(type_id, values)
        }
        ExprKind::Bytes(val) => ExprKind::Bytes(val.clone()),
        ExprKind::Local(idx) => ExprKind::Local(*idx),
        ExprKind::Global(def_id) => ExprKind::Global(*def_id),
        ExprKind::Func(def_id) => ExprKind::Func(*def_id),
        ExprKind::FuncInst(def_id, typeargs_id) => ExprKind::FuncInst(*def_id, *typeargs_id),
        ExprKind::GetElement(expr, idx) => ExprKind::GetElement(
            Box::new(monomorphize_expr(ctx, arg_table, expr.as_ref())),
            *idx,
        ),
        ExprKind::GetElementAddr(expr, idx) => ExprKind::GetElementAddr(
            Box::new(monomorphize_expr(ctx, arg_table, expr.as_ref())),
            *idx,
        ),
        ExprKind::GetIndex(target, idx_value) => ExprKind::GetIndex(
            Box::new(monomorphize_expr(ctx, arg_table, target.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, idx_value.as_ref())),
        ),
        ExprKind::Deref(target) => {
            ExprKind::Deref(Box::new(monomorphize_expr(ctx, arg_table, target.as_ref())))
        }
        ExprKind::Call(target, arguments) => ExprKind::Call(
            Box::new(monomorphize_expr(ctx, arg_table, target)),
            arguments
                .iter()
                .map(|arg| monomorphize_expr(ctx, arg_table, arg))
                .collect(),
        ),
        ExprKind::Add(a, b) => ExprKind::Add(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Sub(a, b) => ExprKind::Sub(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Mul(a, b) => ExprKind::Mul(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Div(a, b) => ExprKind::Div(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Mod(a, b) => ExprKind::Mod(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::BitOr(a, b) => ExprKind::BitOr(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::BitXor(a, b) => ExprKind::BitXor(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::ShiftLeft(a, b) => ExprKind::ShiftLeft(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::ShiftRight(a, b) => ExprKind::ShiftRight(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::And(a, b) => ExprKind::And(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Or(a, b) => ExprKind::Or(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Eq(a, b) => ExprKind::Eq(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::NEq(a, b) => ExprKind::NEq(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Gt(a, b) => ExprKind::Gt(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::GEq(a, b) => ExprKind::GEq(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Lt(a, b) => ExprKind::Lt(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::LEq(a, b) => ExprKind::LEq(
            Box::new(monomorphize_expr(ctx, arg_table, a.as_ref())),
            Box::new(monomorphize_expr(ctx, arg_table, b.as_ref())),
        ),
        ExprKind::Neg(value) => {
            ExprKind::Neg(Box::new(monomorphize_expr(ctx, arg_table, value.as_ref())))
        }
        ExprKind::BitNot(value) => {
            ExprKind::BitNot(Box::new(monomorphize_expr(ctx, arg_table, value.as_ref())))
        }
        ExprKind::Not(value) => {
            ExprKind::Not(Box::new(monomorphize_expr(ctx, arg_table, value.as_ref())))
        }
        ExprKind::Cast(value, type_id) => ExprKind::Cast(
            Box::new(monomorphize_expr(ctx, arg_table, value.as_ref())),
            substitute_generic_args(ctx, arg_table, *type_id),
        ),
    };

    Expr {
        ty: type_id,
        kind,
        assignable: expr.assignable,
    }
}
