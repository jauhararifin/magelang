use crate::analyze::{Context, Scopes, ValueObject};
use crate::errors::SemanticError;
use crate::interner::{Interned, Interner};
use crate::ty::{get_type_from_node, BitSize, FloatType, InternType, InternTypeArgs, Type};
use crate::value::value_from_string_lit;
use crate::{DefId, Symbol};
use magelang_syntax::{
    BinaryExprNode, CallExprNode, CastExprNode, DerefExprNode, ErrorReporter, ExprNode,
    IndexExprNode, PathNode, SelectionExprNode, StructExprNode, Token, TokenKind, UnaryExprNode,
};
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::zip;
use std::rc::Rc;

pub(crate) type ExprInterner<'a> = Interner<'a, Expr<'a>>;
pub type InternExpr<'a> = Interned<'a, Expr<'a>>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Expr<'a> {
    pub ty: InternType<'a>,
    pub kind: ExprKind<'a>,
    pub(crate) assignable: bool,
}

impl<'a> Expr<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> Expr<'a> {
        let ty = self.ty.monomorphize(ctx, self.ty, type_args);

        let kind = match &self.kind {
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
            ExprKind::StructLit(ty, values) => {
                let ty = ty.monomorphize(ctx, *ty, type_args);
                let values = values
                    .iter()
                    .map(|val| val.monomorphize(ctx, type_args))
                    .collect();
                ExprKind::StructLit(ty, values)
            }
            ExprKind::Bytes(val) => ExprKind::Bytes(val.clone()),
            ExprKind::Local(idx) => ExprKind::Local(*idx),
            ExprKind::Global(def_id) => ExprKind::Global(*def_id),
            ExprKind::Func(def_id) => ExprKind::Func(*def_id),
            ExprKind::FuncInst(def_id, inner_typeargs) => {
                let typeargs = inner_typeargs
                    .iter()
                    .map(|ty| ty.monomorphize(ctx, *ty, type_args))
                    .collect::<Vec<_>>();
                let typeargs = ctx.define_typeargs(&typeargs);
                ExprKind::FuncInst(*def_id, typeargs)
            }
            ExprKind::GetElement(expr, idx) => {
                ExprKind::GetElement(Box::new(expr.monomorphize(ctx, type_args)), *idx)
            }
            ExprKind::GetElementAddr(expr, idx) => {
                ExprKind::GetElementAddr(Box::new(expr.monomorphize(ctx, type_args)), *idx)
            }
            ExprKind::GetIndex(target, idx_value) => ExprKind::GetIndex(
                Box::new(target.monomorphize(ctx, type_args)),
                Box::new(idx_value.monomorphize(ctx, type_args)),
            ),
            ExprKind::Deref(target) => {
                ExprKind::Deref(Box::new(target.monomorphize(ctx, type_args)))
            }
            ExprKind::Call(target, arguments) => ExprKind::Call(
                Box::new(target.monomorphize(ctx, type_args)),
                arguments
                    .iter()
                    .map(|arg| arg.monomorphize(ctx, type_args))
                    .collect(),
            ),
            ExprKind::Add(a, b) => ExprKind::Add(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Sub(a, b) => ExprKind::Sub(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Mul(a, b) => ExprKind::Mul(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Div(a, b) => ExprKind::Div(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Mod(a, b) => ExprKind::Mod(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitOr(a, b) => ExprKind::BitOr(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitXor(a, b) => ExprKind::BitXor(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::ShiftLeft(a, b) => ExprKind::ShiftLeft(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::ShiftRight(a, b) => ExprKind::ShiftRight(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::And(a, b) => ExprKind::And(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Or(a, b) => ExprKind::Or(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Eq(a, b) => ExprKind::Eq(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::NEq(a, b) => ExprKind::NEq(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Gt(a, b) => ExprKind::Gt(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::GEq(a, b) => ExprKind::GEq(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Lt(a, b) => ExprKind::Lt(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::LEq(a, b) => ExprKind::LEq(
                Box::new(a.monomorphize(ctx, type_args)),
                Box::new(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Neg(value) => ExprKind::Neg(Box::new(value.monomorphize(ctx, type_args))),
            ExprKind::BitNot(value) => {
                ExprKind::BitNot(Box::new(value.monomorphize(ctx, type_args)))
            }
            ExprKind::Not(value) => ExprKind::Not(Box::new(value.monomorphize(ctx, type_args))),
            ExprKind::Cast(value, into_type) => ExprKind::Cast(
                Box::new(value.monomorphize(ctx, type_args)),
                into_type.monomorphize(ctx, *into_type, type_args),
            ),
        };

        Expr {
            ty,
            kind,
            assignable: self.assignable,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Float<'a, T> {
    source: Symbol<'a>,
    value: T,
}

impl<'a, T> Float<'a, T> {
    fn new(source: Symbol<'a>, value: T) -> Self {
        Self { source, value }
    }
}

impl<'a, T> Hash for Float<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.source.hash(state)
    }
}

impl<'a, T> PartialEq for Float<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.source.eq(&other.source)
    }
}

impl<'a, T> Eq for Float<'a, T> {}

impl<'a, T> std::ops::Deref for Float<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'a> {
    Invalid,

    ConstI8(u8),
    ConstI16(u16),
    ConstI32(u32),
    ConstI64(u64),
    ConstIsize(u64),
    ConstF32(Float<'a, f32>),
    ConstF64(Float<'a, f64>),
    ConstBool(bool),
    Zero,
    StructLit(InternType<'a>, Vec<Expr<'a>>),
    Bytes(Rc<[u8]>),

    Local(usize),
    Global(DefId<'a>),
    Func(DefId<'a>),
    FuncInst(DefId<'a>, InternTypeArgs<'a>),

    GetElement(Box<Expr<'a>>, usize),
    GetElementAddr(Box<Expr<'a>>, usize),
    GetIndex(Box<Expr<'a>>, Box<Expr<'a>>),
    Deref(Box<Expr<'a>>),

    Call(Box<Expr<'a>>, Vec<Expr<'a>>),

    Add(Box<Expr<'a>>, Box<Expr<'a>>),
    Sub(Box<Expr<'a>>, Box<Expr<'a>>),
    Mul(Box<Expr<'a>>, Box<Expr<'a>>),
    Div(Box<Expr<'a>>, Box<Expr<'a>>),
    Mod(Box<Expr<'a>>, Box<Expr<'a>>),
    BitOr(Box<Expr<'a>>, Box<Expr<'a>>),
    BitAnd(Box<Expr<'a>>, Box<Expr<'a>>),
    BitXor(Box<Expr<'a>>, Box<Expr<'a>>),
    ShiftLeft(Box<Expr<'a>>, Box<Expr<'a>>),
    ShiftRight(Box<Expr<'a>>, Box<Expr<'a>>),
    And(Box<Expr<'a>>, Box<Expr<'a>>),
    Or(Box<Expr<'a>>, Box<Expr<'a>>),
    Eq(Box<Expr<'a>>, Box<Expr<'a>>),
    NEq(Box<Expr<'a>>, Box<Expr<'a>>),
    Gt(Box<Expr<'a>>, Box<Expr<'a>>),
    GEq(Box<Expr<'a>>, Box<Expr<'a>>),
    Lt(Box<Expr<'a>>, Box<Expr<'a>>),
    LEq(Box<Expr<'a>>, Box<Expr<'a>>),
    Neg(Box<Expr<'a>>),
    BitNot(Box<Expr<'a>>),
    Not(Box<Expr<'a>>),
    Cast(Box<Expr<'a>>, InternType<'a>),
}

pub(crate) fn get_expr_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &ExprNode,
) -> Expr<'a> {
    match node {
        ExprNode::Path(node) => get_expr_from_path(ctx, scope, expected_type, node),
        ExprNode::Integer(token) => get_expr_from_int_lit(ctx, expected_type, token),
        ExprNode::Frac(token) => get_expr_from_float_lit(ctx, expected_type, token),
        ExprNode::Null(..) => Expr {
            ty: ctx.define_type(Type::Opaque),
            kind: ExprKind::Zero,
            assignable: false,
        },
        ExprNode::Bool(token) => get_expr_from_bool_lit(ctx, token),
        ExprNode::String(token) => get_expr_from_string_lit(ctx, token),
        ExprNode::Binary(node) => get_expr_from_binary_node(ctx, scope, expected_type, node),
        ExprNode::Deref(node) => get_expr_from_deref_node(ctx, scope, expected_type, node),
        ExprNode::Unary(node) => get_expr_from_unary_node(ctx, scope, expected_type, node),
        ExprNode::Call(node) => get_expr_from_call_node(ctx, scope, expected_type, node),
        ExprNode::Cast(node) => get_expr_from_cast_node(ctx, scope, expected_type, node),
        ExprNode::Struct(struct_lit_node) => {
            get_expr_from_struct_lit_node(ctx, scope, struct_lit_node)
        }
        ExprNode::Selection(selection_node) => {
            get_expr_from_selection_node(ctx, scope, selection_node)
        }
        ExprNode::Index(node) => get_expr_from_index_node(ctx, scope, expected_type, node),
        ExprNode::Grouped(node) => get_expr_from_node(ctx, scope, expected_type, node),
    }
}

fn get_expr_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &PathNode,
) -> Expr<'a> {
    let Some(object) = get_value_object_from_path(ctx, scope, &node.names) else {
        return Expr {
            ty: expected_type.unwrap_or(ctx.define_type(Type::Unknown)),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    let is_generic = !node.args.is_empty();

    match object {
        ValueObject::Func(func_obj) => {
            if func_obj.type_params.is_empty() {
                if is_generic {
                    ctx.errors.non_generic_value(node.pos());
                }
                Expr {
                    ty: func_obj.ty,
                    kind: ExprKind::Func(func_obj.def_id),
                    assignable: false,
                }
            } else {
                let expected_type_param = func_obj.type_params.len();
                let provided_type_param = node.args.len();
                if expected_type_param != provided_type_param {
                    ctx.errors.type_arguments_count_mismatch(
                        node.pos(),
                        expected_type_param,
                        provided_type_param,
                    );
                }

                let mut type_args = Vec::<InternType>::default();
                for type_expr in &node.args {
                    let ty = get_type_from_node(ctx, scope, type_expr);
                    type_args.push(ty);
                }
                while type_args.len() < expected_type_param {
                    let unknown_type = ctx.define_type(Type::Unknown);
                    type_args.push(unknown_type);
                }
                let type_args = ctx.define_typeargs(&type_args);

                let instance_ty = func_obj.ty.monomorphize(ctx, func_obj.ty, type_args);

                Expr {
                    ty: instance_ty,
                    kind: ExprKind::FuncInst(func_obj.def_id, type_args),
                    assignable: false,
                }
            }
        }
        ValueObject::Global(global_obj) => {
            if is_generic {
                ctx.errors.non_generic_value(node.pos());
            }
            Expr {
                ty: global_obj.ty,
                kind: ExprKind::Global(global_obj.def_id),
                assignable: true,
            }
        }
        ValueObject::Local(local_obj) => {
            if is_generic {
                ctx.errors.non_generic_value(node.pos());
            }
            Expr {
                ty: local_obj.ty,
                kind: ExprKind::Local(local_obj.id),
                assignable: true,
            }
        }
    }
}

fn get_value_object_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    names: &[Token],
) -> Option<&'b ValueObject<'a>> {
    let name = names.first().expect("path contains empty names");
    let name = ctx.define_symbol(name.value.as_str());

    if names.len() == 1 {
        let Some(object) = scope.value_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[0].pos, &names[0].value);
            return None;
        };
        Some(object)
    } else {
        let Some(import_object) = scope.import_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[0].pos, &names[0].value);
            return None;
        };

        let Some(scope) = ctx.scopes.get(&import_object.package) else {
            ctx.errors.undeclared_symbol(names[1].pos, &names[1].value);
            return None;
        };

        let name = ctx.define_symbol(names[1].value.as_ref());
        let Some(object) = scope.value_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[1].pos, &names[1].value);
            return None;
        };

        Some(object)
    }
}

fn get_expr_from_int_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    expected_type: Option<InternType<'a>>,
    token: &Token,
) -> Expr<'a> {
    let (sign, bit_size) = if let Some(ty) = expected_type {
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
        (false, BitSize::ISize) => token.value.parse::<u64>().map(ExprKind::ConstIsize),
        (false, BitSize::I64) => token.value.parse::<u64>().map(ExprKind::ConstI64),
        (false, BitSize::I32) => token.value.parse::<u32>().map(ExprKind::ConstI32),
        (false, BitSize::I16) => token.value.parse::<u16>().map(ExprKind::ConstI16),
        (false, BitSize::I8) => token.value.parse::<u8>().map(ExprKind::ConstI8),
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_int_literal(token.pos, err);
            ExprKind::Invalid
        }
    };

    let ty = ctx.define_type(Type::Int(sign, bit_size));
    Expr {
        ty,
        kind,
        assignable: false,
    }
}

fn get_expr_from_float_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    expected_type: Option<InternType<'a>>,
    token: &Token,
) -> Expr<'a> {
    let float_type = if let Some(ty) = expected_type {
        match ty.as_ref() {
            Type::Float(float_ty) => *float_ty,
            _ => FloatType::F64,
        }
    } else {
        FloatType::F64
    };

    let value = ctx.define_symbol(&token.value);
    let kind = match float_type {
        FloatType::F32 => value
            .parse::<f32>()
            .map(|val| ExprKind::ConstF32(Float::new(value, val))),
        FloatType::F64 => token
            .value
            .parse::<f64>()
            .map(|val| ExprKind::ConstF64(Float::new(value, val))),
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_float_literal(token.pos, err);
            ExprKind::Invalid
        }
    };

    let ty = ctx.define_type(Type::Float(float_type));
    Expr {
        ty,
        kind,
        assignable: false,
    }
}

fn get_expr_from_bool_lit<'a, E: ErrorReporter>(ctx: &Context<'a, E>, token: &Token) -> Expr<'a> {
    let kind = match token.kind {
        TokenKind::True => ExprKind::ConstBool(true),
        TokenKind::False => ExprKind::ConstBool(false),
        _ => unreachable!("invalid ast: not a boolean literal"),
    };
    Expr {
        ty: ctx.define_type(Type::Bool),
        kind,
        assignable: false,
    }
}

fn get_expr_from_string_lit<'a, E: ErrorReporter>(ctx: &Context<'a, E>, token: &Token) -> Expr<'a> {
    let u8_ty = ctx.define_type(Type::Int(false, BitSize::I8));
    let ty = ctx.define_type(Type::ArrayPtr(u8_ty));

    let Some(mut bytes) = value_from_string_lit(&token.value) else {
        return Expr {
            ty,
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };
    bytes.push(0);

    Expr {
        ty,
        kind: ExprKind::Bytes(bytes.into()),
        assignable: false,
    }
}

fn get_expr_from_binary_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &BinaryExprNode,
) -> Expr<'a> {
    let a = get_expr_from_node(ctx, scope, expected_type, &node.a);
    let b = get_expr_from_node(ctx, scope, Some(a.ty), &node.b);

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
        | TokenKind::LEq => ctx.define_type(Type::Bool),
        _ => a.ty,
    };

    if a.ty != b.ty {
        ctx.errors
            .binop_type_mismatch(node.a.pos(), op_name, a.ty, b.ty);
        return Expr {
            ty: estimated_type,
            kind: ExprKind::Invalid,
            assignable: false,
        };
    }

    let result_ty = match node.op.kind {
        TokenKind::Add | TokenKind::Sub | TokenKind::Mul | TokenKind::Div => {
            if a.ty.is_arithmetic() {
                a.ty
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type::Unknown)
            }
        }
        TokenKind::Eq | TokenKind::NEq => {
            if a.ty.is_opaque() {
                let a_is_null = matches!(a.kind, ExprKind::Zero);
                let b_is_null = matches!(b.kind, ExprKind::Zero);
                if !a_is_null && !b_is_null {
                    ctx.errors.compare_opaque(node.a.pos());
                }
            }

            ctx.define_type(Type::Bool)
        }
        TokenKind::Gt | TokenKind::GEq | TokenKind::Lt | TokenKind::LEq => {
            if a.ty.is_arithmetic() {
                ctx.define_type(Type::Bool)
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type::Unknown)
            }
        }
        TokenKind::Mod
        | TokenKind::BitOr
        | TokenKind::BitAnd
        | TokenKind::BitXor
        | TokenKind::ShiftLeft
        | TokenKind::ShiftRight => {
            if a.ty.is_int() {
                a.ty
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type::Unknown)
            }
        }
        TokenKind::And | TokenKind::Not | TokenKind::Or => {
            if a.ty.is_bool() {
                ctx.define_type(Type::Bool)
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type::Unknown)
            }
        }
        op => unreachable!("token {op} is not a binary operator"),
    };

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
        ty: result_ty,
        kind: expr_kind,
        assignable: false,
    }
}

fn get_expr_from_deref_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &DerefExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
    let ty = value.ty;
    let Type::Ptr(element_ty) = ty.as_ref() else {
        ctx.errors.deref_non_pointer(node.pos);
        return Expr {
            ty: ctx.define_type(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    if !element_ty.is_sized() {
        ctx.errors.deref_unsized(node.pos);
    }

    Expr {
        ty: *element_ty,
        kind: ExprKind::Deref(Box::new(value)),
        assignable: true,
    }
}

fn get_expr_from_unary_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &UnaryExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
    let ty = value.ty;

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
        ctx.errors.unop_type_unsupported(node.op.pos, op_name, ty);
        return Expr {
            ty: ctx.define_type(Type::Unknown),
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

fn get_expr_from_call_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &CallExprNode,
) -> Expr<'a> {
    let func_expr = get_expr_from_node(ctx, scope, expected_type, &node.callee);
    let func_type = func_expr.ty;

    let Type::Func(func_type) = func_type.as_ref() else {
        if !func_type.is_unknown() {
            ctx.errors.not_callable(node.callee.pos());
        }
        return Expr {
            ty: ctx.define_type(Type::Unknown),
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
        let arg_expr = get_expr_from_node(ctx, scope, func_type.params.get(i).cloned(), arg);
        arguments.push(arg_expr);
    }

    for (i, (arg, param)) in zip(&arguments, &func_type.params).enumerate() {
        if !param.is_assignable_with(&arg.ty) {
            ctx.errors
                .type_mismatch(node.arguments[i].pos(), param, arg.ty);
        }
    }

    Expr {
        ty: func_type.return_type,
        kind: ExprKind::Call(Box::new(func_expr), arguments),
        assignable: false,
    }
}

fn get_expr_from_cast_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &CastExprNode,
) -> Expr<'a> {
    let target_type = get_type_from_node(ctx, scope, &node.target);

    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
    let value_type = value.ty;

    let valid_casting = (value_type.is_integral() && target_type.is_integral())
        || (value_type.is_float() && target_type.is_float())
        || (value_type.is_integral() && target_type.is_float())
        || (value_type.is_float() && target_type.is_integral())
        || value_type.is_unknown()
        || target_type.is_unknown();

    let kind = if valid_casting {
        ExprKind::Cast(Box::new(value), target_type)
    } else {
        ctx.errors
            .casting_unsupported(node.value.pos(), value_type, target_type);
        ExprKind::Invalid
    };

    Expr {
        ty: target_type,
        kind,
        assignable: false,
    }
}

fn get_expr_from_struct_lit_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &StructExprNode,
) -> Expr<'a> {
    let ty = get_type_from_node(ctx, scope, &node.target);

    let struct_body = match ty.as_ref() {
        Type::Struct(struct_type) => struct_type.body.get().expect("missing struct body"),
        Type::Inst(inst_type) => inst_type.body.get().expect("missing struct body"),
        _ => {
            ctx.errors.non_struct_type(node.target.pos());
            return Expr {
                ty: ctx.define_type(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            };
        }
    };

    let mut values = HashMap::<Symbol, Expr>::default();
    for element in &node.elements {
        let field_name = ctx.define_symbol(&element.key.value);
        let ty = struct_body
            .fields
            .get(&field_name)
            .cloned()
            .unwrap_or_else(|| {
                ctx.errors
                    .undeclared_field(element.key.pos, &element.key.value);
                ctx.define_type(Type::Unknown)
            });
        let value = get_expr_from_node(ctx, scope, Some(ty), &element.value);

        let value = if !ty.is_assignable_with(value.ty.as_ref()) {
            ctx.errors.type_mismatch(element.value.pos(), ty, value.ty);
            Expr {
                ty: ctx.define_type(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            }
        } else {
            value
        };

        values.insert(field_name, value);
    }

    let mut full_values = Vec::default();
    for (field_name, type_id) in &struct_body.fields {
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
        ty,
        kind: ExprKind::StructLit(ty, full_values),
        assignable: false,
    }
}

fn get_expr_from_selection_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &SelectionExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node(ctx, scope, None, &node.value);

    let mut ty = value.ty;
    let mut is_ptr = false;

    if let Type::Ptr(element_ty) = ty.as_ref() {
        is_ptr = true;
        ty = *element_ty;
    }

    let struct_body = match ty.as_ref() {
        Type::Struct(struct_type) => struct_type.body.get().expect("missing struct body"),
        Type::Inst(inst_type) => inst_type.body.get().expect("missing struct body"),
        _ => {
            ctx.errors
                .non_field_type(node.selection.pos, &node.selection.value);
            return Expr {
                ty: ctx.define_type(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            };
        }
    };

    let selection_name = ctx.define_symbol(&node.selection.value);
    let Some((idx, _, field_type_id)) = struct_body.fields.get_full(&selection_name) else {
        ctx.errors
            .undeclared_field(node.selection.pos, &node.selection.value);
        return Expr {
            ty: ctx.define_type(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        };
    };

    let assignable = value.assignable;
    if is_ptr {
        Expr {
            ty: ctx.define_type(Type::Ptr(*field_type_id)),
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

fn get_expr_from_index_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    expected_type: Option<InternType<'a>>,
    node: &IndexExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
    let ty = value.ty;

    match ty.as_ref() {
        Type::ArrayPtr(element) => {
            let index = get_expr_from_node(ctx, scope, expected_type, &node.index);
            let index_type = index.ty;

            if !index_type.is_int() {
                ctx.errors.non_int_index(node.index.pos());
                return Expr {
                    ty: *element,
                    kind: ExprKind::Invalid,
                    assignable: false,
                };
            }

            Expr {
                ty: ctx.define_type(Type::Ptr(*element)),
                kind: ExprKind::GetIndex(Box::new(value), Box::new(index)),
                assignable: false,
            }
        }
        Type::Unknown => Expr {
            ty: ctx.define_type(Type::Unknown),
            kind: ExprKind::Invalid,
            assignable: false,
        },
        _ => {
            ctx.errors.not_indexable(node.value.pos());
            Expr {
                ty: ctx.define_type(Type::Unknown),
                kind: ExprKind::Invalid,
                assignable: false,
            }
        }
    }
}
