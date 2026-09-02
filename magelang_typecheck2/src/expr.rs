//! Expression checking. Every function here takes the scope it works in; generic
//! definitions are checked by binding their type parameters in that scope (`func.rs`).

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::instance::func_instance;
use crate::resolve::{lookup_value, resolve_type, resolve_type_args};
use crate::scope::{Scopes, ValueEntry};
use crate::ty::{BitSize, FloatType, Type, TypeArgs, TypeRepr};
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use magelang_syntax::{
    BinaryExprNode, BinaryOp, BoolLiteral, CallExprNode, CastExprNode, CharLit, DerefExprNode,
    ErrorReporter, ExprNode, IndexExprNode, NumberLit, PathNode, Pos, SelectionExprNode,
    StringLit, StructExprNode, TryFromNumberError, UnaryExprNode, UnaryOp,
};
use num::{BigInt, Signed, Zero};
use std::collections::HashMap;
use std::iter::zip;

#[derive(Debug)]
pub struct Expr<'a> {
    pub ty: &'a Type<'a>,
    pub kind: ExprKind<'a>,
    pub pos: Pos,
    pub(crate) assignable: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Float<T> {
    value: T,
}

impl<T> Float<T> {
    fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> std::ops::Deref for Float<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug)]
pub enum ExprKind<'a> {
    Invalid,

    /// An integer constant that hasn't been given a type yet.
    ConstInt(BigInt),
    ConstI8(u8),
    ConstI16(u16),
    ConstI32(u32),
    ConstI64(u64),
    ConstIsize(u64),
    /// A float constant that hasn't been given a type yet.
    ConstFloat(Float<f64>),
    ConstF32(Float<f32>),
    ConstF64(Float<f64>),
    ConstBool(bool),
    Zero,
    StructLit(&'a Type<'a>, &'a [Expr<'a>]),
    Bytes(&'a [u8]),

    Local(usize),
    Global(DefId<'a>),
    Func(DefId<'a>),
    FuncInst(DefId<'a>, &'a TypeArgs<'a>),

    GetElement(&'a Expr<'a>, usize),
    GetElementAddr(&'a Expr<'a>, usize),
    GetIndex(&'a Expr<'a>, &'a Expr<'a>),
    Deref(&'a Expr<'a>),

    Call(&'a Expr<'a>, &'a [Expr<'a>]),

    Add(&'a Expr<'a>, &'a Expr<'a>),
    Sub(&'a Expr<'a>, &'a Expr<'a>),
    Mul(&'a Expr<'a>, &'a Expr<'a>),
    Div(&'a Expr<'a>, &'a Expr<'a>),
    Mod(&'a Expr<'a>, &'a Expr<'a>),
    BitOr(&'a Expr<'a>, &'a Expr<'a>),
    BitAnd(&'a Expr<'a>, &'a Expr<'a>),
    BitXor(&'a Expr<'a>, &'a Expr<'a>),
    ShiftLeft(&'a Expr<'a>, &'a Expr<'a>),
    ShiftRight(&'a Expr<'a>, &'a Expr<'a>),
    And(&'a Expr<'a>, &'a Expr<'a>),
    Or(&'a Expr<'a>, &'a Expr<'a>),
    Eq(&'a Expr<'a>, &'a Expr<'a>),
    NEq(&'a Expr<'a>, &'a Expr<'a>),
    Gt(&'a Expr<'a>, &'a Expr<'a>),
    GEq(&'a Expr<'a>, &'a Expr<'a>),
    Lt(&'a Expr<'a>, &'a Expr<'a>),
    LEq(&'a Expr<'a>, &'a Expr<'a>),
    Neg(&'a Expr<'a>),
    BitNot(&'a Expr<'a>),
    Not(&'a Expr<'a>),
    Cast(&'a Expr<'a>, &'a Type<'a>),
}

fn invalid_expr<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>, pos: Pos) -> Expr<'a> {
    Expr {
        ty: ctx.unknown_type(),
        kind: ExprKind::Invalid,
        pos,
        assignable: false,
    }
}

/// Checks an expression. When an expected type is known, an untyped constant is
/// converted to it; otherwise untyped constants default to `isize` / `f64`.
pub(crate) fn get_expr_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &ExprNode,
) -> Expr<'a> {
    let expr = check_expr(ctx, scope, node);
    match expected_type {
        Some(target) => coerce_untyped(expr, target),
        None => default_untyped(ctx, expr),
    }
}

fn check_expr<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &ExprNode,
) -> Expr<'a> {
    match node {
        ExprNode::Path(node) => get_expr_from_path(ctx, scope, node),
        ExprNode::Number(lit) => get_expr_from_number_lit(ctx, lit),
        ExprNode::Null(pos) => Expr {
            ty: ctx.opaque_type(),
            kind: ExprKind::Zero,
            pos: *pos,
            assignable: false,
        },
        ExprNode::Bool(lit) => get_expr_from_bool_lit(ctx, lit),
        ExprNode::Char(lit) => get_expr_from_char_lit(ctx, lit),
        ExprNode::String(lit) => get_expr_from_string_lit(ctx, lit),
        ExprNode::Binary(node) => get_expr_from_binary_node(ctx, scope, node),
        ExprNode::Deref(node) => get_expr_from_deref_node(ctx, scope, node),
        ExprNode::Unary(node) => get_expr_from_unary_node(ctx, scope, node),
        ExprNode::Call(node) => get_expr_from_call_node(ctx, scope, node),
        ExprNode::Cast(node) => get_expr_from_cast_node(ctx, scope, node),
        ExprNode::Struct(node) => get_expr_from_struct_lit_node(ctx, scope, node),
        ExprNode::Selection(node) => get_expr_from_selection_node(ctx, scope, node),
        ExprNode::Index(node) => get_expr_from_index_node(ctx, scope, node),
        ExprNode::Grouped(node) => check_expr(ctx, scope, node),
    }
}

// ---------------------------------------------------------------------------------------
// Untyped constants
// ---------------------------------------------------------------------------------------

/// Gives an untyped constant its default type.
fn default_untyped<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>, expr: Expr<'a>) -> Expr<'a> {
    match &expr.kind {
        ExprKind::ConstInt(v) => Expr {
            ty: ctx.isize_type(),
            kind: ExprKind::ConstIsize(v.to_u64()),
            pos: expr.pos,
            assignable: false,
        },
        ExprKind::ConstFloat(v) => Expr {
            ty: ctx.f64_type(),
            kind: ExprKind::ConstF64(*v),
            pos: expr.pos,
            assignable: false,
        },
        _ => expr,
    }
}

/// Converts an untyped constant to `target` when `target` is a number type; any other
/// expression (or target) is returned unchanged.
fn coerce_untyped<'a>(expr: Expr<'a>, target: &'a Type<'a>) -> Expr<'a> {
    coerce_untyped_kind(&expr.kind, expr.pos, target).unwrap_or(expr)
}

fn coerce_untyped_kind<'a>(
    kind: &ExprKind<'a>,
    pos: Pos,
    target: &'a Type<'a>,
) -> Option<Expr<'a>> {
    let kind = match (&target.repr, kind) {
        (TypeRepr::Int(_, size), ExprKind::ConstInt(v)) => int_const(*size, v),
        (TypeRepr::Float(FloatType::F32), ExprKind::ConstInt(v)) => {
            ExprKind::ConstF32(Float::new(v.to_f32()))
        }
        (TypeRepr::Float(FloatType::F64), ExprKind::ConstInt(v)) => {
            ExprKind::ConstF64(Float::new(v.to_f64()))
        }
        (TypeRepr::Int(sign, size), ExprKind::ConstFloat(v)) => {
            float_to_int_const(*sign, *size, v.value)
        }
        (TypeRepr::Float(FloatType::F32), ExprKind::ConstFloat(v)) => {
            ExprKind::ConstF32(Float::new(v.value as f32))
        }
        (TypeRepr::Float(FloatType::F64), ExprKind::ConstFloat(v)) => {
            ExprKind::ConstF64(Float::new(v.value))
        }
        _ => return None,
    };
    Some(Expr {
        ty: target,
        kind,
        pos,
        assignable: false,
    })
}

/// The two's-complement bit pattern of `v` at the given width.
fn int_const<'a>(size: BitSize, v: &BigInt) -> ExprKind<'a> {
    match size {
        BitSize::I8 => ExprKind::ConstI8(v.to_u8()),
        BitSize::I16 => ExprKind::ConstI16(v.to_u16()),
        BitSize::I32 => ExprKind::ConstI32(v.to_u32()),
        BitSize::I64 => ExprKind::ConstI64(v.to_u64()),
        BitSize::ISize => ExprKind::ConstIsize(v.to_u64()),
    }
}

fn float_to_int_const<'a>(sign: bool, size: BitSize, v: f64) -> ExprKind<'a> {
    match (sign, size) {
        (false, BitSize::I8) => ExprKind::ConstI8(v as u8),
        (false, BitSize::I16) => ExprKind::ConstI16(v as u16),
        (false, BitSize::I32) => ExprKind::ConstI32(v as u32),
        (false, BitSize::I64) => ExprKind::ConstI64(v as u64),
        (false, BitSize::ISize) => ExprKind::ConstIsize(v as u64),
        (true, BitSize::I8) => ExprKind::ConstI8((v as i8) as u8),
        (true, BitSize::I16) => ExprKind::ConstI16((v as i16) as u16),
        (true, BitSize::I32) => ExprKind::ConstI32((v as i32) as u32),
        (true, BitSize::I64) => ExprKind::ConstI64((v as i64) as u64),
        (true, BitSize::ISize) => ExprKind::ConstIsize((v as i64) as u64),
    }
}

trait BigIntExt {
    fn to_u8(&self) -> u8;
    fn to_u16(&self) -> u16;
    fn to_u32(&self) -> u32;
    fn to_u64(&self) -> u64;
    fn to_f32(&self) -> f32;
    fn to_f64(&self) -> f64;
}

macro_rules! impl_bigint_truncation {
    ($name:ident, $target:ident, $size:expr) => {
        fn $name(&self) -> $target {
            let mut bytes = self.to_signed_bytes_le();
            let negative = bytes.last().is_some_and(|b| b & 0x80 != 0);
            bytes.resize($size.max(bytes.len()), if negative { 0xff } else { 0x00 });
            $target::from_le_bytes(bytes[..$size].try_into().unwrap())
        }
    };
}

impl BigIntExt for BigInt {
    impl_bigint_truncation!(to_u8, u8, 1);
    impl_bigint_truncation!(to_u16, u16, 2);
    impl_bigint_truncation!(to_u32, u32, 4);
    impl_bigint_truncation!(to_u64, u64, 8);

    fn to_f32(&self) -> f32 {
        num::ToPrimitive::to_f32(self).expect("bigint is convertible to f32")
    }

    fn to_f64(&self) -> f64 {
        num::ToPrimitive::to_f64(self).expect("bigint is convertible to f64")
    }
}

// ---------------------------------------------------------------------------------------
// Literals and paths
// ---------------------------------------------------------------------------------------

fn get_expr_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &PathNode,
) -> Expr<'a> {
    let Some(entry) = lookup_value(ctx, scope, &node.path) else {
        // Assignable, so that a misspelled assignment target reports only one error.
        return Expr {
            ty: ctx.unknown_type(),
            kind: ExprKind::Invalid,
            pos: node.pos(),
            assignable: true,
        };
    };

    let has_type_args = !node.args.is_empty();
    match entry {
        ValueEntry::Func(def_id) => {
            let def = ctx.defs.func_def(def_id);
            if def.type_params.is_empty() {
                if has_type_args {
                    ctx.errors.non_generic_value(node.pos());
                }
                return Expr {
                    ty: def.sig.get().expect("signatures are resolved in phase 3"),
                    kind: ExprKind::Func(def_id),
                    pos: node.pos(),
                    assignable: false,
                };
            }

            let type_args =
                resolve_type_args(ctx, scope, def.type_params.len(), &node.args, node.pos());
            match func_instance(ctx, def, type_args, node.pos()) {
                Some(inst) => Expr {
                    ty: inst.ty,
                    kind: ExprKind::FuncInst(def_id, type_args),
                    pos: node.pos(),
                    assignable: false,
                },
                None => invalid_expr(ctx, node.pos()),
            }
        }
        ValueEntry::Global(def_id) => {
            if has_type_args {
                ctx.errors.non_generic_value(node.pos());
            }
            let def = ctx.defs.global_def(def_id);
            Expr {
                ty: def.ty.get().expect("global types are resolved in phase 3"),
                kind: ExprKind::Global(def_id),
                pos: node.pos(),
                assignable: true,
            }
        }
        ValueEntry::Local { id, ty } => {
            if has_type_args {
                ctx.errors.non_generic_value(node.pos());
            }
            Expr {
                ty,
                kind: ExprKind::Local(id),
                pos: node.pos(),
                assignable: true,
            }
        }
    }
}

fn get_expr_from_number_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &NumberLit,
) -> Expr<'a> {
    if lit.value.is_int() {
        get_expr_from_int_lit(ctx, lit)
    } else {
        get_expr_from_float_lit(ctx, lit)
    }
}

fn get_expr_from_int_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &NumberLit,
) -> Expr<'a> {
    match lit.value.to_int() {
        Ok(v) => Expr {
            ty: ctx.anon(TypeRepr::UntypedInt),
            kind: ExprKind::ConstInt(v),
            pos: lit.pos,
            assignable: false,
        },
        Err(TryFromNumberError::OutOfRange) => {
            ctx.errors.overflowed_int_literal(lit.pos);
            invalid_expr(ctx, lit.pos)
        }
        Err(..) => {
            ctx.errors.invalid_int_literal(lit.pos);
            invalid_expr(ctx, lit.pos)
        }
    }
}

fn get_expr_from_float_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &NumberLit,
) -> Expr<'a> {
    match f64::try_from(&lit.value) {
        Ok(v) => Expr {
            ty: ctx.anon(TypeRepr::UntypedFloat),
            kind: ExprKind::ConstFloat(Float::new(v)),
            pos: lit.pos,
            assignable: false,
        },
        Err(err) => {
            ctx.errors.invalid_float_literal(lit.pos, err);
            invalid_expr(ctx, lit.pos)
        }
    }
}

fn get_expr_from_bool_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &BoolLiteral,
) -> Expr<'a> {
    Expr {
        ty: ctx.bool_type(),
        kind: ExprKind::ConstBool(lit.value),
        pos: lit.pos,
        assignable: false,
    }
}

fn get_expr_from_char_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &CharLit,
) -> Expr<'a> {
    Expr {
        ty: ctx.anon(TypeRepr::UntypedInt),
        kind: ExprKind::ConstInt(BigInt::from(lit.value as u32)),
        pos: lit.pos,
        assignable: false,
    }
}

fn get_expr_from_string_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    lit: &StringLit,
) -> Expr<'a> {
    let ty = ctx.define_type(Type::anonymous(TypeRepr::ArrayPtr(ctx.u8_type())));

    let mut bytes = lit.value.clone();
    bytes.push(0);
    let bytes = ctx.arena.alloc_slice_copy(&bytes);

    Expr {
        ty,
        kind: ExprKind::Bytes(bytes),
        pos: lit.pos,
        assignable: false,
    }
}

// ---------------------------------------------------------------------------------------
// Binary operators
// ---------------------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq)]
enum OpClass {
    /// `+ - * /` on integers and floats; the result has the operand type.
    Arith,
    /// `% | & ^` on integers; the result has the operand type.
    Integer,
    /// `<< >>` on integers of possibly different types; the result has the left type.
    Shift,
    /// `&& ||` on booleans.
    Bool,
    /// `== !=` on any pair of equal types (opaque only against `null`).
    Equality,
    /// `< <= > >=` on integers and floats.
    Comparison,
}

fn op_class(op: &BinaryOp) -> OpClass {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => OpClass::Arith,
        BinaryOp::Mod | BinaryOp::BitOr | BinaryOp::BitAnd | BinaryOp::BitXor => OpClass::Integer,
        BinaryOp::ShiftLeft | BinaryOp::ShiftRight => OpClass::Shift,
        BinaryOp::And | BinaryOp::Or => OpClass::Bool,
        BinaryOp::Eq | BinaryOp::NEq => OpClass::Equality,
        BinaryOp::Gt | BinaryOp::GEq | BinaryOp::Lt | BinaryOp::LEq => OpClass::Comparison,
    }
}

fn op_name(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "div",
        BinaryOp::Mod => "mod",
        BinaryOp::BitOr => "bitwise or",
        BinaryOp::BitAnd => "bitwise and",
        BinaryOp::BitXor => "bitwise xor",
        BinaryOp::ShiftLeft => "shift left",
        BinaryOp::ShiftRight => "shift right",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::Eq => "eq",
        BinaryOp::NEq => "neq",
        BinaryOp::Gt => "gt",
        BinaryOp::GEq => "geq",
        BinaryOp::Lt => "lt",
        BinaryOp::LEq => "leq",
    }
}

fn build_binary<'a>(op: &BinaryOp, a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
    match op {
        BinaryOp::Add => ExprKind::Add(a, b),
        BinaryOp::Sub => ExprKind::Sub(a, b),
        BinaryOp::Mul => ExprKind::Mul(a, b),
        BinaryOp::Div => ExprKind::Div(a, b),
        BinaryOp::Mod => ExprKind::Mod(a, b),
        BinaryOp::BitOr => ExprKind::BitOr(a, b),
        BinaryOp::BitAnd => ExprKind::BitAnd(a, b),
        BinaryOp::BitXor => ExprKind::BitXor(a, b),
        BinaryOp::ShiftLeft => ExprKind::ShiftLeft(a, b),
        BinaryOp::ShiftRight => ExprKind::ShiftRight(a, b),
        BinaryOp::And => ExprKind::And(a, b),
        BinaryOp::Or => ExprKind::Or(a, b),
        BinaryOp::Eq => ExprKind::Eq(a, b),
        BinaryOp::NEq => ExprKind::NEq(a, b),
        BinaryOp::Gt => ExprKind::Gt(a, b),
        BinaryOp::GEq => ExprKind::GEq(a, b),
        BinaryOp::Lt => ExprKind::Lt(a, b),
        BinaryOp::LEq => ExprKind::LEq(a, b),
    }
}

fn get_expr_from_binary_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &BinaryExprNode,
) -> Expr<'a> {
    let pos = node.a.pos();
    let op = &node.op;
    let a: &'a Expr<'a> = ctx.arena.alloc(check_expr(ctx, scope, &node.a));
    let b: &'a Expr<'a> = ctx.arena.alloc(check_expr(ctx, scope, &node.b));
    let class = op_class(op);

    if class == OpClass::Bool {
        return check_bool_binary(ctx, op, pos, a, b);
    }

    if let Some(expr) = fold_untyped_consts(ctx, op, pos, a, b) {
        return expr;
    }

    if matches!(class, OpClass::Integer | OpClass::Shift) {
        for operand in [a, b] {
            if matches!(operand.ty.repr, TypeRepr::UntypedFloat) {
                ctx.errors.binop_on_untyped_float(operand.pos, op_name(op));
                return invalid_expr(ctx, operand.pos);
            }
        }
    }

    let (a, b) = coerce_untyped_operands(ctx, a, b);
    let bool_ty = ctx.bool_type();

    match class {
        OpClass::Equality | OpClass::Comparison => {
            let invalid = Expr {
                ty: bool_ty,
                kind: ExprKind::Invalid,
                pos,
                assignable: false,
            };
            if a.ty.is_unknown() || b.ty.is_unknown() {
                return invalid;
            }
            if a.ty != b.ty {
                ctx.errors.binop_type_mismatch(pos, op_name(op), a.ty, b.ty);
                return invalid;
            }
            if class == OpClass::Comparison && !a.ty.is_arithmetic() {
                ctx.errors.binop_type_unsupported(pos, op_name(op), a.ty);
                return invalid;
            }
            if class == OpClass::Equality && a.ty.is_strictly_opaque() {
                // An opaque value can only be compared with null.
                let a_is_null = matches!(a.kind, ExprKind::Zero);
                let b_is_null = matches!(b.kind, ExprKind::Zero);
                if !a_is_null && !b_is_null {
                    ctx.errors.compare_opaque(pos);
                }
            }
            Expr {
                ty: bool_ty,
                kind: build_binary(op, a, b),
                pos,
                assignable: false,
            }
        }
        OpClass::Arith | OpClass::Integer => {
            let result_ty = if a.ty.is_unknown() { b.ty } else { a.ty };
            let invalid = Expr {
                ty: result_ty,
                kind: ExprKind::Invalid,
                pos,
                assignable: false,
            };
            if a.ty.is_unknown() || b.ty.is_unknown() {
                return invalid;
            }
            if a.ty != b.ty {
                ctx.errors.binop_type_mismatch(pos, op_name(op), a.ty, b.ty);
                return invalid;
            }
            let supported = match class {
                OpClass::Arith => a.ty.is_arithmetic(),
                _ => a.ty.is_int(),
            };
            if !supported {
                ctx.errors.binop_type_unsupported(pos, op_name(op), a.ty);
                return invalid;
            }
            Expr {
                ty: result_ty,
                kind: build_binary(op, a, b),
                pos,
                assignable: false,
            }
        }
        OpClass::Shift => {
            let invalid = Expr {
                ty: a.ty,
                kind: ExprKind::Invalid,
                pos,
                assignable: false,
            };
            if a.ty.is_unknown() || b.ty.is_unknown() {
                return invalid;
            }
            if !a.ty.is_int() {
                ctx.errors.binop_type_unsupported(pos, op_name(op), a.ty);
                return invalid;
            }
            if !b.ty.is_int() {
                ctx.errors.binop_type_unsupported(pos, op_name(op), b.ty);
                return invalid;
            }
            Expr {
                ty: a.ty,
                kind: build_binary(op, a, b),
                pos,
                assignable: false,
            }
        }
        OpClass::Bool => unreachable!("boolean operators are handled above"),
    }
}

fn check_bool_binary<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    op: &BinaryOp,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    let bool_ty = ctx.bool_type();
    let invalid = Expr {
        ty: bool_ty,
        kind: ExprKind::Invalid,
        pos,
        assignable: false,
    };
    if a.ty.is_unknown() || b.ty.is_unknown() {
        return invalid;
    }
    if !a.ty.is_bool() || !b.ty.is_bool() {
        ctx.errors.binop_type_mismatch(pos, op_name(op), a.ty, b.ty);
        return invalid;
    }
    Expr {
        ty: bool_ty,
        kind: build_binary(op, a, b),
        pos,
        assignable: false,
    }
}

enum Folded {
    /// The operator doesn't apply to these constants.
    Invalid,
    /// The operator applies, but this particular evaluation is an error.
    Illegal(&'static str),
    Bool(bool),
    Int(BigInt),
    Float(f64),
}

/// Evaluates an operator whose operands are both untyped constants.
fn fold_untyped_consts<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    op: &BinaryOp,
    pos: Pos,
    a: &Expr<'a>,
    b: &Expr<'a>,
) -> Option<Expr<'a>> {
    let folded = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(x), ExprKind::ConstInt(y)) => fold_ints(op, x, y),
        (ExprKind::ConstInt(x), ExprKind::ConstFloat(y)) => fold_floats(op, x.to_f64(), y.value),
        (ExprKind::ConstFloat(x), ExprKind::ConstInt(y)) => fold_floats(op, x.value, y.to_f64()),
        (ExprKind::ConstFloat(x), ExprKind::ConstFloat(y)) => fold_floats(op, x.value, y.value),
        _ => return None,
    };

    let expr = match folded {
        Folded::Invalid => {
            ctx.errors.binop_type_mismatch(pos, op_name(op), a.ty, b.ty);
            invalid_expr(ctx, pos)
        }
        Folded::Illegal(message) => {
            ctx.errors.report(pos, message.to_string());
            invalid_expr(ctx, pos)
        }
        Folded::Bool(v) => Expr {
            ty: ctx.bool_type(),
            kind: ExprKind::ConstBool(v),
            pos,
            assignable: false,
        },
        Folded::Int(v) => Expr {
            ty: ctx.anon(TypeRepr::UntypedInt),
            kind: ExprKind::ConstInt(v),
            pos,
            assignable: false,
        },
        Folded::Float(v) => Expr {
            ty: ctx.anon(TypeRepr::UntypedFloat),
            kind: ExprKind::ConstFloat(Float::new(v)),
            pos,
            assignable: false,
        },
    };
    Some(expr)
}

fn fold_ints(op: &BinaryOp, a: &BigInt, b: &BigInt) -> Folded {
    match op {
        BinaryOp::Add => Folded::Int(a + b),
        BinaryOp::Sub => Folded::Int(a - b),
        BinaryOp::Mul => Folded::Int(a * b),
        BinaryOp::Div => {
            if b.is_zero() {
                Folded::Illegal("illegal operation: division by zero")
            } else {
                Folded::Int(a / b)
            }
        }
        BinaryOp::Mod => {
            if b.is_zero() {
                Folded::Illegal("illegal operation: mod by zero")
            } else {
                Folded::Int(a % b)
            }
        }
        BinaryOp::BitOr => Folded::Int(a | b),
        BinaryOp::BitAnd => Folded::Int(a & b),
        BinaryOp::BitXor => Folded::Int(a ^ b),
        BinaryOp::ShiftLeft | BinaryOp::ShiftRight => {
            if b.is_negative() {
                return Folded::Illegal("illegal operation: shift by negative");
            }
            if b > &BigInt::from(512) {
                return Folded::Illegal("illegal operation: integer overflow for shift operation");
            }
            let amount = num::ToPrimitive::to_i32(b).expect("shift amount fits in i32");
            if matches!(op, BinaryOp::ShiftLeft) {
                Folded::Int(a << amount)
            } else {
                Folded::Int(a >> amount)
            }
        }
        BinaryOp::Eq => Folded::Bool(a == b),
        BinaryOp::NEq => Folded::Bool(a != b),
        BinaryOp::Gt => Folded::Bool(a > b),
        BinaryOp::GEq => Folded::Bool(a >= b),
        BinaryOp::Lt => Folded::Bool(a < b),
        BinaryOp::LEq => Folded::Bool(a <= b),
        BinaryOp::And | BinaryOp::Or => unreachable!("boolean operators are never folded"),
    }
}

fn fold_floats(op: &BinaryOp, a: f64, b: f64) -> Folded {
    match op {
        BinaryOp::Add => Folded::Float(a + b),
        BinaryOp::Sub => Folded::Float(a - b),
        BinaryOp::Mul => Folded::Float(a * b),
        BinaryOp::Div => Folded::Float(a / b),
        BinaryOp::Eq => Folded::Bool(a == b),
        BinaryOp::NEq => Folded::Bool(a != b),
        BinaryOp::Gt => Folded::Bool(a > b),
        BinaryOp::GEq => Folded::Bool(a >= b),
        BinaryOp::Lt => Folded::Bool(a < b),
        BinaryOp::LEq => Folded::Bool(a <= b),
        BinaryOp::Mod
        | BinaryOp::BitOr
        | BinaryOp::BitAnd
        | BinaryOp::BitXor
        | BinaryOp::ShiftLeft
        | BinaryOp::ShiftRight => Folded::Invalid,
        BinaryOp::And | BinaryOp::Or => unreachable!("boolean operators are never folded"),
    }
}

/// When exactly one operand is an untyped constant and the other has a number type, the
/// constant takes the other operand's type.
fn coerce_untyped_operands<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> (&'a Expr<'a>, &'a Expr<'a>) {
    let is_untyped = |e: &Expr| matches!(e.ty.repr, TypeRepr::UntypedInt | TypeRepr::UntypedFloat);
    let is_number = |e: &Expr| matches!(e.ty.repr, TypeRepr::Int(..) | TypeRepr::Float(..));

    if is_untyped(a)
        && is_number(b)
        && let Some(coerced) = coerce_untyped_kind(&a.kind, a.pos, b.ty)
    {
        let coerced: &'a Expr<'a> = ctx.arena.alloc(coerced);
        return (coerced, b);
    }
    if is_untyped(b)
        && is_number(a)
        && let Some(coerced) = coerce_untyped_kind(&b.kind, b.pos, a.ty)
    {
        let coerced: &'a Expr<'a> = ctx.arena.alloc(coerced);
        return (a, coerced);
    }
    (a, b)
}

// ---------------------------------------------------------------------------------------
// Unary operators, casts
// ---------------------------------------------------------------------------------------

fn get_expr_from_deref_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &DerefExprNode,
) -> Expr<'a> {
    let value = check_expr(ctx, scope, &node.value);
    let ty = value.ty;
    let TypeRepr::Ptr(element_ty) = ty.repr else {
        if !ty.is_unknown() {
            ctx.errors.deref_non_pointer(node.pos);
        }
        return Expr {
            ty: ctx.unknown_type(),
            kind: ExprKind::Deref(ctx.arena.alloc(value)),
            pos: node.pos,
            assignable: true,
        };
    };

    Expr {
        ty: element_ty,
        kind: ExprKind::Deref(ctx.arena.alloc(value)),
        pos: node.pos,
        assignable: true,
    }
}

fn get_expr_from_unary_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &UnaryExprNode,
) -> Expr<'a> {
    let value = check_expr(ctx, scope, &node.value);
    let ty = value.ty;

    // Untyped constants are folded.
    if let ExprKind::ConstInt(val) = &value.kind {
        let folded = match node.op {
            UnaryOp::BitNot => Some(ExprKind::ConstInt(!val)),
            UnaryOp::Sub => Some(ExprKind::ConstInt(-val)),
            UnaryOp::Add | UnaryOp::Not => None,
        };
        if let Some(kind) = folded {
            return Expr {
                ty,
                kind,
                pos: node.pos,
                assignable: false,
            };
        }
    }
    if let ExprKind::ConstFloat(val) = value.kind
        && let UnaryOp::Sub = node.op
    {
        return Expr {
            ty,
            kind: ExprKind::ConstFloat(Float::new(-val.value)),
            pos: node.pos,
            assignable: false,
        };
    }
    if matches!(value.kind, ExprKind::ConstInt(..) | ExprKind::ConstFloat(..))
        && matches!(node.op, UnaryOp::Add)
    {
        return value;
    }

    let (op_name, is_valid) = match node.op {
        UnaryOp::BitNot => ("bit not", ty.is_int()),
        UnaryOp::Sub => ("sub", ty.is_arithmetic()),
        UnaryOp::Add => ("add", ty.is_arithmetic()),
        UnaryOp::Not => ("not", ty.is_bool()),
    };

    let kind = match node.op {
        UnaryOp::BitNot => ExprKind::BitNot(ctx.arena.alloc(value)),
        UnaryOp::Sub => ExprKind::Neg(ctx.arena.alloc(value)),
        UnaryOp::Add => value.kind,
        UnaryOp::Not => ExprKind::Not(ctx.arena.alloc(value)),
    };

    if !is_valid {
        ctx.errors.unop_type_unsupported(node.pos, op_name, ty);
        return Expr {
            ty: ctx.unknown_type(),
            kind,
            pos: node.pos,
            assignable: false,
        };
    }

    Expr {
        ty,
        kind,
        pos: node.pos,
        assignable: false,
    }
}

fn get_expr_from_cast_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &CastExprNode,
) -> Expr<'a> {
    let target_type = resolve_type(ctx, scope, &node.target);
    let value = check_expr(ctx, scope, &node.value);
    let value_pos = node.value.pos();

    // Casting an untyped constant is a conversion of the constant itself.
    match (&value.kind, &target_type.repr) {
        (ExprKind::ConstInt(..) | ExprKind::ConstFloat(..), TypeRepr::Int(..) | TypeRepr::Float(..)) => {
            return coerce_untyped_kind(&value.kind, value_pos, target_type)
                .expect("number constants convert to number types");
        }
        (ExprKind::ConstInt(v), TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..)) => {
            return Expr {
                ty: target_type,
                kind: ExprKind::ConstIsize(v.to_u64()),
                pos: value_pos,
                assignable: false,
            };
        }
        _ => (),
    }

    let value_type = value.ty;
    let valid_casting = value_type.is_unknown()
        || target_type.is_unknown()
        || matches!(
            (&value_type.repr, &target_type.repr),
            (
                TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..),
                TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..),
            ) | (TypeRepr::Int(..), TypeRepr::Float(..))
                | (TypeRepr::Float(..), TypeRepr::Int(..))
                | (TypeRepr::Float(..), TypeRepr::Float(..))
        );

    let kind = if valid_casting {
        ExprKind::Cast(ctx.arena.alloc(value), target_type)
    } else {
        ctx.errors
            .casting_unsupported(value_pos, value_type, target_type);
        ExprKind::Invalid
    };

    Expr {
        ty: target_type,
        kind,
        pos: value_pos,
        assignable: false,
    }
}

// ---------------------------------------------------------------------------------------
// Calls, structs, selection, indexing
// ---------------------------------------------------------------------------------------

fn get_expr_from_call_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &CallExprNode,
) -> Expr<'a> {
    let callee = check_expr(ctx, scope, &node.callee);
    let callee_ty = callee.ty;

    let TypeRepr::Func(func_type) = &callee_ty.repr else {
        if !callee_ty.is_unknown() {
            ctx.errors.not_callable(node.callee.pos());
        }
        return invalid_expr(ctx, node.pos);
    };

    if node.arguments.len() != func_type.params.len() {
        ctx.errors.wrong_number_of_arguments(
            node.pos,
            func_type.params.len(),
            node.arguments.len(),
        );
    }

    let mut arguments = BumpVec::with_capacity_in(node.arguments.len(), ctx.arena);
    for (i, arg) in node.arguments.iter().enumerate() {
        let expected = func_type.params.get(i).copied();
        arguments.push(get_expr_from_node(ctx, scope, expected, arg));
    }

    for (i, (arg, param)) in zip(&arguments, func_type.params).enumerate() {
        if !param.is_assignable_with(arg.ty) {
            ctx.errors
                .type_mismatch(node.arguments[i].pos(), param, arg.ty);
        }
    }

    Expr {
        ty: func_type.return_type,
        kind: ExprKind::Call(ctx.arena.alloc(callee), arguments.into_bump_slice()),
        pos: node.pos,
        assignable: false,
    }
}

fn get_expr_from_struct_lit_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &StructExprNode,
) -> Expr<'a> {
    let ty = resolve_type(ctx, scope, &node.target);

    let Some(struct_type) = ty.as_struct() else {
        if !ty.is_unknown() {
            ctx.errors.non_struct_type(node.target.pos());
        }
        return invalid_expr(ctx, node.pos);
    };
    let body = struct_type
        .body
        .get()
        .expect("struct bodies are complete before expressions are checked");

    let mut values = HashMap::<Symbol<'a>, Expr<'a>>::default();
    for element in &node.elements {
        let field_name = ctx.define_symbol(&element.key.value);
        let field_ty = body.fields.get(&field_name).copied().unwrap_or_else(|| {
            ctx.errors
                .undeclared_field(element.key.pos, &element.key.value);
            ctx.unknown_type()
        });

        let value = get_expr_from_node(ctx, scope, Some(field_ty), &element.value);
        let value = if field_ty.is_assignable_with(value.ty) {
            value
        } else {
            ctx.errors
                .type_mismatch(element.value.pos(), field_ty, value.ty);
            invalid_expr(ctx, element.pos)
        };
        values.insert(field_name, value);
    }

    // Fields are laid out in declaration order; unmentioned fields are zero.
    let mut full_values = BumpVec::with_capacity_in(body.fields.len(), ctx.arena);
    for (field_name, field_ty) in &body.fields {
        let value = values.remove(field_name).unwrap_or(Expr {
            ty: field_ty,
            kind: ExprKind::Zero,
            pos: node.pos,
            assignable: false,
        });
        full_values.push(value);
    }

    Expr {
        ty,
        kind: ExprKind::StructLit(ty, full_values.into_bump_slice()),
        pos: node.pos,
        assignable: false,
    }
}

fn get_expr_from_selection_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &SelectionExprNode,
) -> Expr<'a> {
    let value = check_expr(ctx, scope, &node.value);
    let pos = node.value.pos();

    let mut ty = value.ty;
    let mut is_ptr = false;
    if let TypeRepr::Ptr(element_ty) = ty.repr {
        is_ptr = true;
        ty = element_ty;
    }

    let Some(struct_type) = ty.as_struct() else {
        if !ty.is_unknown() {
            ctx.errors
                .non_field_type(node.selection.pos, &node.selection.value);
        }
        return Expr {
            ty: ctx.unknown_type(),
            kind: ExprKind::Invalid,
            pos,
            assignable: true,
        };
    };
    let body = struct_type
        .body
        .get()
        .expect("struct bodies are complete before expressions are checked");

    let selection = ctx.define_symbol(&node.selection.value);
    let Some((idx, _, field_ty)) = body.fields.get_full(&selection) else {
        ctx.errors
            .undeclared_field(node.selection.pos, &node.selection.value);
        return Expr {
            ty: ctx.unknown_type(),
            kind: ExprKind::Invalid,
            pos,
            assignable: true,
        };
    };
    let field_ty = *field_ty;

    if is_ptr {
        // Selecting a field of a pointer yields the address of the field, not the field
        // itself. An address is not assignable.
        return Expr {
            ty: ctx.define_type(Type::anonymous(TypeRepr::Ptr(field_ty))),
            kind: ExprKind::GetElementAddr(ctx.arena.alloc(value), idx),
            pos,
            assignable: false,
        };
    }

    // A field of a dereferenced struct is assignable: `p.*.b = 5` is the same as
    // `p.b.* = 5`, so it is lowered to exactly that.
    if let ExprKind::Deref(ptr) = &value.kind {
        let ptr = *ptr;
        let field_addr = ctx.arena.alloc(Expr {
            ty: ctx.define_type(Type::anonymous(TypeRepr::Ptr(field_ty))),
            kind: ExprKind::GetElementAddr(ptr, idx),
            pos,
            assignable: false,
        });
        return Expr {
            ty: field_ty,
            kind: ExprKind::Deref(field_addr),
            pos,
            assignable: true,
        };
    }

    let assignable = value.assignable;
    Expr {
        ty: field_ty,
        kind: ExprKind::GetElement(ctx.arena.alloc(value), idx),
        pos,
        assignable,
    }
}

fn get_expr_from_index_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &IndexExprNode,
) -> Expr<'a> {
    let value = check_expr(ctx, scope, &node.value);
    let pos = node.value.pos();

    match value.ty.repr {
        TypeRepr::ArrayPtr(element) => {
            let index = get_expr_from_node(ctx, scope, Some(ctx.isize_type()), &node.index);
            if !index.ty.is_int() {
                ctx.errors.non_int_index(node.index.pos());
                return Expr {
                    ty: element,
                    kind: ExprKind::Invalid,
                    pos,
                    assignable: false,
                };
            }

            Expr {
                ty: ctx.define_type(Type::anonymous(TypeRepr::Ptr(element))),
                kind: ExprKind::GetIndex(ctx.arena.alloc(value), ctx.arena.alloc(index)),
                pos,
                assignable: false,
            }
        }
        TypeRepr::Unknown => invalid_expr(ctx, pos),
        _ => {
            ctx.errors.not_indexable(pos);
            invalid_expr(ctx, pos)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bigint_truncation() {
        assert_eq!(0x80, BigInt::from(-128).to_u8());
        assert_eq!(0xff, BigInt::from(-1).to_u8());
        assert_eq!(1260u64, BigInt::from(1260).to_u64());
        assert_eq!(44, BigInt::from(300).to_u8());
    }
}
