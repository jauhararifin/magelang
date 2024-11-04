use crate::analyze::{Context, Scopes, ValueObject};
use crate::errors::SemanticError;
use crate::ty::{get_type_from_node, BitSize, FloatType, Type, TypeArgs, TypeKind, TypeRepr};
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use magelang_syntax::{
    BinaryExprNode, BinaryOp, BoolLiteral, CallExprNode, CastExprNode, CharLit, DerefExprNode,
    ErrorReporter, ExprNode, IndexExprNode, NumberLit, PathName, PathNode, Pos, SelectionExprNode,
    StringLit, StructExprNode, TryFromNumberError, UnaryExprNode, UnaryOp,
};
use num::{BigInt, Signed, Zero};
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::zip;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Expr<'a> {
    pub ty: &'a Type<'a>,
    pub kind: ExprKind<'a>,
    pub pos: Pos,
    pub(crate) assignable: bool,
}

impl<'a> Expr<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> Expr<'a> {
        let ty = self.ty.substitute(ctx, type_args);

        let kind = match &self.kind {
            ExprKind::Invalid => ExprKind::Invalid,
            ExprKind::ConstInt(val) => ExprKind::ConstInt(val.clone()),
            ExprKind::ConstI8(val) => ExprKind::ConstI8(*val),
            ExprKind::ConstI16(val) => ExprKind::ConstI16(*val),
            ExprKind::ConstI32(val) => ExprKind::ConstI32(*val),
            ExprKind::ConstI64(val) => ExprKind::ConstI64(*val),
            ExprKind::ConstIsize(val) => ExprKind::ConstIsize(*val),
            ExprKind::ConstFloat(val) => ExprKind::ConstFloat(*val),
            ExprKind::ConstF32(val) => ExprKind::ConstF32(*val),
            ExprKind::ConstF64(val) => ExprKind::ConstF64(*val),
            ExprKind::ConstBool(val) => ExprKind::ConstBool(*val),
            ExprKind::Zero => ExprKind::Zero,
            ExprKind::StructLit(ty, values) => {
                let ty = ty.substitute(ctx, type_args);
                let mut fields = BumpVec::with_capacity_in(values.len(), ctx.arena);
                for val in values.iter() {
                    fields.push(val.monomorphize(ctx, type_args));
                }
                ExprKind::StructLit(ty, fields.into_bump_slice())
            }
            ExprKind::Bytes(val) => ExprKind::Bytes(val),
            ExprKind::Local(idx) => ExprKind::Local(*idx),
            ExprKind::Global(def_id) => ExprKind::Global(*def_id),
            ExprKind::Func(def_id) => ExprKind::Func(*def_id),
            ExprKind::FuncInst(def_id, inner_typeargs) => {
                let typeargs = inner_typeargs
                    .iter()
                    .map(|ty| ty.substitute(ctx, type_args))
                    .collect::<Vec<_>>();
                let typeargs = ctx.define_typeargs(&typeargs);
                ExprKind::FuncInst(*def_id, typeargs)
            }
            ExprKind::GetElement(expr, idx) => {
                ExprKind::GetElement(ctx.arena.alloc(expr.monomorphize(ctx, type_args)), *idx)
            }
            ExprKind::GetElementAddr(expr, idx) => {
                ExprKind::GetElementAddr(ctx.arena.alloc(expr.monomorphize(ctx, type_args)), *idx)
            }
            ExprKind::GetIndex(target, idx_value) => ExprKind::GetIndex(
                ctx.arena.alloc(target.monomorphize(ctx, type_args)),
                ctx.arena.alloc(idx_value.monomorphize(ctx, type_args)),
            ),
            ExprKind::Deref(target) => {
                ExprKind::Deref(ctx.arena.alloc(target.monomorphize(ctx, type_args)))
            }
            ExprKind::Call(target, arguments) => {
                let mut args = BumpVec::with_capacity_in(arguments.len(), ctx.arena);
                for arg in arguments.iter() {
                    args.push(arg.monomorphize(ctx, type_args));
                }
                ExprKind::Call(
                    ctx.arena.alloc(target.monomorphize(ctx, type_args)),
                    args.into_bump_slice(),
                )
            }
            ExprKind::Add(a, b) => ExprKind::Add(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Sub(a, b) => ExprKind::Sub(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Mul(a, b) => ExprKind::Mul(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Div(a, b) => ExprKind::Div(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Mod(a, b) => ExprKind::Mod(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitOr(a, b) => ExprKind::BitOr(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::BitXor(a, b) => ExprKind::BitXor(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::ShiftLeft(a, b) => ExprKind::ShiftLeft(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::ShiftRight(a, b) => ExprKind::ShiftRight(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::And(a, b) => ExprKind::And(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Or(a, b) => ExprKind::Or(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Eq(a, b) => ExprKind::Eq(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::NEq(a, b) => ExprKind::NEq(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Gt(a, b) => ExprKind::Gt(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::GEq(a, b) => ExprKind::GEq(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Lt(a, b) => ExprKind::Lt(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::LEq(a, b) => ExprKind::LEq(
                ctx.arena.alloc(a.monomorphize(ctx, type_args)),
                ctx.arena.alloc(b.monomorphize(ctx, type_args)),
            ),
            ExprKind::Neg(value) => {
                ExprKind::Neg(ctx.arena.alloc(value.monomorphize(ctx, type_args)))
            }
            ExprKind::BitNot(value) => {
                ExprKind::BitNot(ctx.arena.alloc(value.monomorphize(ctx, type_args)))
            }
            ExprKind::Not(value) => {
                ExprKind::Not(ctx.arena.alloc(value.monomorphize(ctx, type_args)))
            }
            ExprKind::Cast(value, into_type) => ExprKind::Cast(
                ctx.arena.alloc(value.monomorphize(ctx, type_args)),
                into_type.substitute(ctx, type_args),
            ),
        };

        Expr {
            ty,
            kind,
            pos: self.pos,
            assignable: self.assignable,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FloatConst(f64);

impl Hash for FloatConst {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as i64).hash(state)
    }
}

impl PartialEq for FloatConst {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for FloatConst {}

#[derive(Debug, Clone, Copy)]
pub struct Float<T> {
    value: T,
}

impl<T> Float<T> {
    fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> Hash for Float<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&[0]) //TODO: solve this
    }
}

impl<T> PartialEq for Float<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<T> Eq for Float<T> where T: PartialEq {}

impl<T> std::ops::Deref for Float<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExprKind<'a> {
    Invalid,

    ConstInt(BigInt),
    ConstI8(u8),
    ConstI16(u16),
    ConstI32(u32),
    ConstI64(u64),
    ConstIsize(u64),
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

pub(crate) fn get_expr_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &ExprNode,
) -> Expr<'a> {
    let result = get_expr_from_node_internal(ctx, scope, node);

    let Some(expected_type) = expected_type else {
        if let ExprKind::ConstInt(val) = result.kind {
            return Expr {
                ty: ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Int(true, BitSize::ISize),
                }),
                kind: ExprKind::ConstIsize(val.to_u64()),
                pos: result.pos,
                assignable: false,
            };
        }

        if let ExprKind::ConstFloat(val) = result.kind {
            return Expr {
                ty: ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Float(FloatType::F64),
                }),
                kind: ExprKind::ConstF64(val),
                pos: result.pos,
                assignable: false,
            };
        }

        return result;
    };

    match (&expected_type.repr, &result.kind) {
        (TypeRepr::Int(.., BitSize::I8), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI8(v.to_u8()),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I16), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI16(v.to_u16()),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I32), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI32(v.to_u32()),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I64), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI64(v.to_u64()),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::ISize), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstIsize(v.to_u64()),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Float(FloatType::F32), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstF32(Float::new(v.to_f32())),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Float(FloatType::F64), ExprKind::ConstInt(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstF64(Float::new(v.to_f64())),
            pos: result.pos,
            assignable: result.assignable,
        },

        (TypeRepr::Int(.., BitSize::I8), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI8(v.value as u8),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I16), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI16(v.value as u16),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I32), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI32(v.value as u32),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::I64), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstI64(v.value as u64),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Int(.., BitSize::ISize), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstIsize(v.value as u64),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Float(FloatType::F32), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstF32(Float::new(v.value as f32)),
            pos: result.pos,
            assignable: result.assignable,
        },
        (TypeRepr::Float(FloatType::F64), ExprKind::ConstFloat(v)) => Expr {
            ty: expected_type,
            kind: ExprKind::ConstF64(Float::new(v.value)),
            pos: result.pos,
            assignable: result.assignable,
        },
        _ => result,
    }
}

fn get_expr_from_node_internal<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &ExprNode,
) -> Expr<'a> {
    match node {
        ExprNode::Path(node) => get_expr_from_path(ctx, scope, node),
        ExprNode::Number(num_lit) => get_expr_from_number_lit(ctx, num_lit),
        ExprNode::Null(..) => Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Opaque,
            }),
            kind: ExprKind::Zero,
            pos: node.pos(),
            assignable: false,
        },
        ExprNode::Bool(token) => get_expr_from_bool_lit(ctx, token),
        ExprNode::Char(char_lit) => get_expr_from_char_lit(ctx, char_lit),
        ExprNode::String(string_lit) => get_expr_from_string_lit(ctx, string_lit),
        ExprNode::Binary(node) => get_expr_from_binary_node(ctx, scope, node),
        ExprNode::Deref(node) => get_expr_from_deref_node(ctx, scope, node),
        ExprNode::Unary(node) => get_expr_from_unary_node(ctx, scope, node),
        ExprNode::Call(node) => get_expr_from_call_node(ctx, scope, node),
        ExprNode::Cast(node) => get_expr_from_cast_node(ctx, scope, node),
        ExprNode::Struct(struct_lit_node) => {
            get_expr_from_struct_lit_node(ctx, scope, struct_lit_node)
        }
        ExprNode::Selection(selection_node) => {
            get_expr_from_selection_node(ctx, scope, selection_node)
        }
        ExprNode::Index(node) => get_expr_from_index_node(ctx, scope, node),
        ExprNode::Grouped(node) => get_expr_from_node_internal(ctx, scope, node),
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

macro_rules! impl_bigint_ext_conversion {
    ($name:ident, $target:ident, $size: expr) => {
        fn $name(&self) -> $target {
            let mut v = self.to_signed_bytes_le();
            let size = $size;
            let one = if v.len() > 0 {
                v[v.len() - 1] & 0x80 != 0
            } else {
                false
            };
            while v.len() < size {
                if one {
                    v.push(0xff);
                } else {
                    v.push(0x00);
                }
            }
            $target::from_le_bytes(v[..size].try_into().unwrap())
        }
    };
}

impl BigIntExt for BigInt {
    impl_bigint_ext_conversion!(to_u8, u8, 1);
    impl_bigint_ext_conversion!(to_u16, u16, 2);
    impl_bigint_ext_conversion!(to_u32, u32, 4);
    impl_bigint_ext_conversion!(to_u64, u64, 8);

    fn to_f32(&self) -> f32 {
        num::ToPrimitive::to_f32(self).expect("bigint is convertible to f32")
    }

    fn to_f64(&self) -> f64 {
        num::ToPrimitive::to_f64(self).expect("bigint is convertible to f64")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_bigint_conversion() {
        let val = BigInt::from(-128);
        assert_eq!(0x80, val.to_u8());

        let val = BigInt::from(1260);
        assert_eq!(1260u64, val.to_u64());
    }
}

fn get_expr_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &PathNode,
) -> Expr<'a> {
    let Some(object) = get_value_object_from_path(ctx, scope, &node.path) else {
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.pos(),
            assignable: true,
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
                    pos: node.pos(),
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

                let mut type_args = Vec::<&Type>::default();
                for type_expr in &node.args {
                    let ty = get_type_from_node(ctx, scope, type_expr);
                    type_args.push(ty);
                }
                while type_args.len() < expected_type_param {
                    let unknown_type = ctx.define_type(Type {
                        kind: TypeKind::Anonymous,
                        repr: TypeRepr::Unknown,
                    });
                    type_args.push(unknown_type);
                }
                let type_args = ctx.define_typeargs(&type_args);

                let instance_ty = func_obj.ty.specialize(ctx, type_args);

                Expr {
                    ty: instance_ty,
                    kind: ExprKind::FuncInst(func_obj.def_id, type_args),
                    pos: node.pos(),
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
                pos: node.pos(),
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
                pos: node.pos(),
                assignable: true,
            }
        }
    }
}

fn get_value_object_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, '_, E>,
    scope: &'b Scopes<'a>,
    path: &PathName,
) -> Option<&'b ValueObject<'a>> {
    match path {
        PathName::Local(name) => {
            let name_symbol = ctx.define_symbol(name.value.as_str());
            let Some(object) = scope.value_scopes.lookup(name_symbol) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };
            Some(object)
        }
        PathName::Package { package, name } => {
            let package_symbol = ctx.define_symbol(&package.value);
            let Some(import_object) = scope.import_scopes.lookup(package_symbol) else {
                ctx.errors.undeclared_symbol(package.pos, &package.value);
                return None;
            };

            let Some(scope) = ctx.scopes.get(&import_object.package) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };

            let name_symbol = ctx.define_symbol(name.value.as_ref());
            let Some(object) = scope.value_scopes.lookup(name_symbol) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };

            Some(object)
        }
        PathName::Invalid(..) => None,
    }
}

fn get_expr_from_number_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    if number_lit.value.is_int() {
        get_expr_from_int_lit(ctx, number_lit)
    } else {
        get_expr_from_float_lit(ctx, number_lit)
    }
}

fn get_expr_from_int_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    let kind = match number_lit.value.to_int() {
        Ok(v) => ExprKind::ConstInt(v),
        Err(TryFromNumberError::OutOfRange) => {
            ctx.errors.overflowed_int_literal(number_lit.pos);
            ExprKind::Invalid
        }
        Err(..) => {
            ctx.errors.invalid_int_literal(number_lit.pos);
            ExprKind::Invalid
        }
    };

    Expr {
        ty: ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::UntypedInt,
        }),
        kind,
        pos: number_lit.pos,
        assignable: false,
    }
}

fn get_expr_from_float_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    let kind = match f64::try_from(&number_lit.value) {
        Ok(v) => ExprKind::ConstFloat(Float::new(v)),
        Err(err) => {
            ctx.errors.invalid_float_literal(number_lit.pos, err);
            ExprKind::Invalid
        }
    };
    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::UntypedFloat,
    });
    Expr {
        ty,
        kind,
        pos: number_lit.pos,
        assignable: false,
    }
}

fn get_expr_from_bool_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    token: &BoolLiteral,
) -> Expr<'a> {
    Expr {
        ty: ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Bool,
        }),
        kind: ExprKind::ConstBool(token.value),
        pos: token.pos,
        assignable: false,
    }
}

fn get_expr_from_char_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    char_lit: &CharLit,
) -> Expr<'a> {
    let ch = char_lit.value;
    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::UntypedInt,
    });
    Expr {
        ty,
        kind: ExprKind::ConstInt(BigInt::from(ch as u32)),
        pos: char_lit.pos,
        assignable: false,
    }
}

fn get_expr_from_string_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    token: &StringLit,
) -> Expr<'a> {
    let u8_ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I8),
    });
    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::ArrayPtr(u8_ty),
    });

    let mut bytes = token.value.clone();
    bytes.push(0);

    let bytes = ctx.arena.alloc_slice_copy(&bytes);
    Expr {
        ty,
        kind: ExprKind::Bytes(bytes),
        pos: token.pos,
        assignable: false,
    }
}

fn get_expr_from_binary_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &BinaryExprNode,
) -> Expr<'a> {
    let a = get_expr_from_node_internal(ctx, scope, &node.a);
    let b = get_expr_from_node_internal(ctx, scope, &node.b);

    let a = ctx.arena.alloc(a);
    let b = ctx.arena.alloc(b);

    match node.op {
        BinaryOp::Add => get_binary_arith_exprs::<BinopAdd, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Sub => get_binary_arith_exprs::<BinopSub, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Mul => get_binary_arith_exprs::<BinopMul, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Div => get_binary_div_exprs(ctx, node.a.pos(), a, b),
        BinaryOp::Mod => get_binary_integer_exprs::<BinopMod, E>(ctx, node.a.pos(), a, b),
        BinaryOp::BitOr => get_binary_integer_exprs::<BinopBitOr, E>(ctx, node.a.pos(), a, b),
        BinaryOp::BitAnd => get_binary_integer_exprs::<BinopBitAnd, E>(ctx, node.a.pos(), a, b),
        BinaryOp::BitXor => get_binary_integer_exprs::<BinopBitXor, E>(ctx, node.a.pos(), a, b),
        BinaryOp::ShiftLeft => get_binary_shifts_exprs::<BinopShl, E>(ctx, node.a.pos(), a, b),
        BinaryOp::ShiftRight => get_binary_shifts_exprs::<BinopShr, E>(ctx, node.a.pos(), a, b),
        BinaryOp::And => get_binary_bool_exprs::<BinopAnd, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Or => get_binary_bool_exprs::<BinopOr, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Eq => get_binary_equality_exprs::<BinopEq, E>(ctx, node.a.pos(), a, b),
        BinaryOp::NEq => get_binary_equality_exprs::<BinopNEq, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Gt => get_binary_comparison_exprs::<BinopGt, E>(ctx, node.a.pos(), a, b),
        BinaryOp::GEq => get_binary_comparison_exprs::<BinopGEq, E>(ctx, node.a.pos(), a, b),
        BinaryOp::Lt => get_binary_comparison_exprs::<BinopLt, E>(ctx, node.a.pos(), a, b),
        BinaryOp::LEq => get_binary_comparison_exprs::<BinopLEq, E>(ctx, node.a.pos(), a, b),
    }
}

trait BinopEvaluator {
    fn name() -> &'static str;
    fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation;
    fn eval_if(a: &BigInt, b: f64) -> BinopEvaluation;
    fn eval_fi(a: f64, b: &BigInt) -> BinopEvaluation;
    fn eval_ff(a: f64, b: f64) -> BinopEvaluation;

    fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a>;
}

enum BinopEvaluation {
    Invalid,
    Illegal(&'static str),
    Bool(bool),
    Int(BigInt),
    Float(f64),
}

macro_rules! impl_binop_evaluator_for_equality {
    ($op:ident, $name:expr, $cmp:expr, $expr:ident) => {
        struct $op;

        impl BinopEvaluator for $op {
            fn name() -> &'static str {
                $name
            }
            fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Bool($cmp(a, b))
            }
            fn eval_if(a: &BigInt, b: f64) -> BinopEvaluation {
                BinopEvaluation::Bool($cmp(&a.to_f64(), &b))
            }
            fn eval_fi(a: f64, b: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Bool($cmp(&a, &b.to_f64()))
            }
            fn eval_ff(a: f64, b: f64) -> BinopEvaluation {
                BinopEvaluation::Bool($cmp(&a, &b))
            }
            fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$expr(a, b)
            }
        }
    };
}

impl_binop_evaluator_for_equality!(BinopEq, "eq", std::cmp::PartialEq::eq, Eq);
impl_binop_evaluator_for_equality!(BinopNEq, "neq", std::cmp::PartialEq::ne, NEq);

impl_binop_evaluator_for_equality!(BinopGt, "gt", std::cmp::PartialOrd::gt, Gt);
impl_binop_evaluator_for_equality!(BinopGEq, "geq", std::cmp::PartialOrd::ge, GEq);
impl_binop_evaluator_for_equality!(BinopLt, "lt", std::cmp::PartialOrd::lt, Lt);
impl_binop_evaluator_for_equality!(BinopLEq, "leq", std::cmp::PartialOrd::le, LEq);

macro_rules! impl_binop_evaluator_for_arith {
    ($op:ident, $name:expr, $cmp:expr, $expr:ident) => {
        struct $op;

        impl BinopEvaluator for $op {
            fn name() -> &'static str {
                $name
            }
            fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Int($cmp(a, b))
            }
            fn eval_if(a: &BigInt, b: f64) -> BinopEvaluation {
                BinopEvaluation::Float($cmp(&a.to_f64(), &b))
            }
            fn eval_fi(a: f64, b: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Float($cmp(&a, &b.to_f64()))
            }
            fn eval_ff(a: f64, b: f64) -> BinopEvaluation {
                BinopEvaluation::Float($cmp(&a, &b))
            }
            fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$expr(a, b)
            }
        }
    };
}

impl_binop_evaluator_for_arith!(BinopAdd, "add", std::ops::Add::add, Add);
impl_binop_evaluator_for_arith!(BinopSub, "sub", std::ops::Sub::sub, Sub);
impl_binop_evaluator_for_arith!(BinopMul, "mul", std::ops::Mul::mul, Mul);

struct BinopDiv;

impl BinopEvaluator for BinopDiv {
    fn name() -> &'static str {
        "div"
    }
    fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
        if b.is_zero() {
            BinopEvaluation::Illegal("illegal operation: division by zero")
        } else {
            BinopEvaluation::Int(a / b)
        }
    }
    fn eval_if(a: &BigInt, b: f64) -> BinopEvaluation {
        BinopEvaluation::Float(a.to_f64() / b)
    }
    fn eval_fi(a: f64, b: &BigInt) -> BinopEvaluation {
        BinopEvaluation::Float(a / b.to_f64())
    }
    fn eval_ff(a: f64, b: f64) -> BinopEvaluation {
        BinopEvaluation::Float(a / b)
    }
    fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
        ExprKind::Div(a, b)
    }
}

struct BinopMod;

impl BinopEvaluator for BinopMod {
    fn name() -> &'static str {
        "mod"
    }
    fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
        if b.is_zero() {
            BinopEvaluation::Illegal("illegal operation: mod by zero")
        } else {
            BinopEvaluation::Int(a % b)
        }
    }
    fn eval_if(_: &BigInt, _: f64) -> BinopEvaluation {
        BinopEvaluation::Invalid
    }
    fn eval_fi(_: f64, _: &BigInt) -> BinopEvaluation {
        BinopEvaluation::Invalid
    }
    fn eval_ff(_: f64, _: f64) -> BinopEvaluation {
        BinopEvaluation::Invalid
    }
    fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
        ExprKind::Mod(a, b)
    }
}

macro_rules! impl_binop_evaluator_for_bit_ops {
    ($op:ident, $name:expr, $cmp:expr, $expr:ident) => {
        struct $op;

        impl BinopEvaluator for $op {
            fn name() -> &'static str {
                $name
            }
            fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Int($cmp(a, b))
            }
            fn eval_if(_: &BigInt, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_fi(_: f64, _: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_ff(_: f64, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$expr(a, b)
            }
        }
    };
}

impl_binop_evaluator_for_bit_ops!(BinopBitAnd, "bitwise and", std::ops::BitAnd::bitand, BitAnd);
impl_binop_evaluator_for_bit_ops!(BinopBitOr, "bitwise and", std::ops::BitOr::bitor, BitOr);
impl_binop_evaluator_for_bit_ops!(BinopBitXor, "bitwise and", std::ops::BitXor::bitxor, BitXor);

macro_rules! impl_binop_evaluator_for_bool_ops {
    ($op:ident, $name:expr, $expr:ident) => {
        struct $op;

        impl BinopEvaluator for $op {
            fn name() -> &'static str {
                $name
            }
            fn eval_ii(_: &BigInt, _: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_if(_: &BigInt, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_fi(_: f64, _: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_ff(_: f64, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$expr(a, b)
            }
        }
    };
}

impl_binop_evaluator_for_bool_ops!(BinopAnd, "and", And);
impl_binop_evaluator_for_bool_ops!(BinopOr, "or", Or);

macro_rules! impl_binop_evaluator_for_shift_ops {
    ($op:ident, $name:expr, $shift:expr, $expr:ident) => {
        struct $op;
        impl BinopEvaluator for $op {
            fn name() -> &'static str {
                $name
            }
            fn eval_ii(a: &BigInt, b: &BigInt) -> BinopEvaluation {
                if b.is_negative() {
                    return BinopEvaluation::Illegal("illegal operation: shift by negative");
                }
                let max = BigInt::from(512);
                if b > &max {
                    BinopEvaluation::Illegal(
                        "illegal operation: integer overflow for shift operation",
                    )
                } else {
                    let b = num::ToPrimitive::to_i32(b).expect("b fits in i32");
                    BinopEvaluation::Int($shift(a, b))
                }
            }
            fn eval_if(_: &BigInt, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_fi(_: f64, _: &BigInt) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn eval_ff(_: f64, _: f64) -> BinopEvaluation {
                BinopEvaluation::Invalid
            }
            fn build<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$expr(a, b)
            }
        }
    };
}

impl_binop_evaluator_for_shift_ops!(BinopShl, "shift left", std::ops::Shl::<i32>::shl, ShiftLeft);
impl_binop_evaluator_for_shift_ops!(
    BinopShr,
    "shift right",
    std::ops::Shr::<i32>::shr,
    ShiftRight
);

fn get_binary_equality_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<T, E>(ctx, pos, a, b) {
        return expr;
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let bool_ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    if a.ty.is_strictly_opaque() {
        assert!(
            b.ty.is_strictly_opaque(),
            "if a is opaque, then b must be opaque as well"
        );
        let a_is_null = matches!(a.kind, ExprKind::Zero);
        let b_is_null = matches!(b.kind, ExprKind::Zero);
        // we can only compare opaque type with null. we can't comare opaque type
        // with another opaque type
        if !a_is_null && !b_is_null {
            ctx.errors.compare_opaque(pos);
        }
    }

    Expr {
        ty: bool_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_binary_comparison_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<T, E>(ctx, pos, a, b) {
        return expr;
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let bool_ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    if !a.ty.is_arithmetic() {
        assert!(
            !b.ty.is_arithmetic(),
            "if a is not arithmetic, then b must be not arithmetic as well"
        );
        ctx.errors.binop_type_unsupported(pos, T::name(), a.ty);
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    Expr {
        ty: bool_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_binary_arith_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<T, E>(ctx, pos, a, b) {
        return expr;
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let expected_ty = if a.ty.is_unknown() { b.ty } else { a.ty };

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    if !a.ty.is_arithmetic() {
        assert!(
            !b.ty.is_arithmetic(),
            "if a is not arithmetic, then b must be not arithmetic as well"
        );
        ctx.errors.binop_type_unsupported(pos, T::name(), a.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    Expr {
        ty: expected_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_binary_div_exprs<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<BinopDiv, E>(ctx, pos, a, b) {
        return expr;
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let expected_ty = if a.ty.is_unknown() { b.ty } else { a.ty };

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if a.ty != b.ty {
        ctx.errors
            .binop_type_mismatch(pos, BinopDiv::name(), a.ty, b.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    if !a.ty.is_arithmetic() {
        assert!(
            !b.ty.is_arithmetic(),
            "if a is not arithmetic, then b must be not arithmetic as well"
        );
        ctx.errors
            .binop_type_unsupported(pos, BinopDiv::name(), a.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    Expr {
        ty: expected_ty,
        kind: BinopDiv::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_binary_integer_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<T, E>(ctx, pos, a, b) {
        return expr;
    }

    let a_is_untyped_float = matches!(a.ty.repr, TypeRepr::UntypedFloat);
    let b_is_untyped_float = matches!(a.ty.repr, TypeRepr::UntypedFloat);
    if a_is_untyped_float || b_is_untyped_float {
        let pos = if a_is_untyped_float { a.pos } else { b.pos };
        ctx.errors.report(
            pos,
            format!(
                "Cannot perform {} binary operation on floating number",
                T::name()
            ),
        );
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let expected_ty = if a.ty.is_unknown() { b.ty } else { a.ty };

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    if !a.ty.is_int() {
        assert!(
            !b.ty.is_int(),
            "if a is not int, then b must be not int as well"
        );
        ctx.errors.binop_type_unsupported(pos, T::name(), a.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    Expr {
        ty: expected_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_binary_shifts_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    if let Some(expr) = evaluate_untyped_const::<T, E>(ctx, pos, a, b) {
        return expr;
    }

    let a_is_untyped_float = matches!(a.ty.repr, TypeRepr::UntypedFloat);
    let b_is_untyped_float = matches!(a.ty.repr, TypeRepr::UntypedFloat);
    if a_is_untyped_float || b_is_untyped_float {
        let pos = if a_is_untyped_float { a.pos } else { b.pos };
        ctx.errors.report(
            pos,
            format!(
                "Cannot perform {} binary operation on floating number",
                T::name()
            ),
        );
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    let (a, b) = cast_untyped_const(ctx, a, b);

    let expected_ty = a.ty;

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if !a.ty.is_int() {
        assert!(
            !b.ty.is_int(),
            "if a is not int, then b must be not int as well"
        );
        ctx.errors.binop_type_unsupported(pos, T::name(), a.ty);
        return Expr {
            ty: expected_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(a.ty.is_int() && b.ty.is_int());
    Expr {
        ty: expected_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn evaluate_untyped_const<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Option<Expr<'a>> {
    let eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => T::eval_ii(va, vb),
        (ExprKind::ConstInt(va), ExprKind::ConstFloat(vb)) => T::eval_if(va, vb.value),
        (ExprKind::ConstFloat(va), ExprKind::ConstInt(vb)) => T::eval_fi(va.value, vb),
        (ExprKind::ConstFloat(va), ExprKind::ConstFloat(vb)) => T::eval_ff(va.value, vb.value),
        _ => return None,
    };

    if let BinopEvaluation::Invalid = eval {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
    }
    if let BinopEvaluation::Illegal(msg) = eval {
        ctx.errors.report(pos, msg.to_string())
    }

    let type_repr = match eval {
        BinopEvaluation::Invalid | BinopEvaluation::Illegal(..) => TypeRepr::Unknown,
        BinopEvaluation::Bool(..) => TypeRepr::Bool,
        BinopEvaluation::Int(..) => TypeRepr::UntypedInt,
        BinopEvaluation::Float(..) => TypeRepr::UntypedFloat,
    };

    let type_kind = match eval {
        BinopEvaluation::Invalid | BinopEvaluation::Illegal(..) => ExprKind::Invalid,
        BinopEvaluation::Bool(v) => ExprKind::ConstBool(v),
        BinopEvaluation::Int(v) => ExprKind::ConstInt(v),
        BinopEvaluation::Float(v) => ExprKind::ConstFloat(Float::new(v)),
    };

    Some(Expr {
        ty: ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: type_repr,
        }),
        kind: type_kind,
        pos,
        assignable: false,
    })
}

fn cast_untyped_const<'a, E>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> (&'a Expr<'a>, &'a Expr<'a>) {
    match (&a.ty.repr, &b.ty.repr) {
        (TypeRepr::UntypedInt, TypeRepr::Int(..) | TypeRepr::Float(..)) => {
            let ExprKind::ConstInt(ref val_a) = a.kind else {
                unreachable!();
            };
            let a = ctx.arena.alloc(cast_untyped_int(val_a, a.pos, b.ty));
            (a, b)
        }
        (TypeRepr::Int(..) | TypeRepr::Float(..), TypeRepr::UntypedInt) => {
            let ExprKind::ConstInt(ref val_b) = b.kind else {
                unreachable!();
            };
            let b = ctx.arena.alloc(cast_untyped_int(val_b, b.pos, a.ty));
            (a, b)
        }

        (TypeRepr::UntypedFloat, TypeRepr::Int(..) | TypeRepr::Float(..)) => {
            let ExprKind::ConstFloat(val_a) = a.kind else {
                unreachable!();
            };
            let a = ctx
                .arena
                .alloc(cast_untyped_float(val_a.value, a.pos, b.ty));
            (a, b)
        }
        (TypeRepr::Int(..) | TypeRepr::Float(..), TypeRepr::UntypedFloat) => {
            let ExprKind::ConstFloat(val_b) = b.kind else {
                unreachable!();
            };
            let b = ctx
                .arena
                .alloc(cast_untyped_float(val_b.value, b.pos, a.ty));
            (a, b)
        }

        _ => (a, b),
    }
}

fn cast_untyped_int<'a>(a: &BigInt, pos: Pos, target: &'a Type<'a>) -> Expr<'a> {
    match target.repr {
        TypeRepr::Int(.., bitsize) => Expr {
            ty: target,
            kind: match bitsize {
                BitSize::I8 => ExprKind::ConstI8(a.to_u8()),
                BitSize::I16 => ExprKind::ConstI16(a.to_u16()),
                BitSize::I32 => ExprKind::ConstI32(a.to_u32()),
                BitSize::I64 => ExprKind::ConstI64(a.to_u64()),
                BitSize::ISize => ExprKind::ConstIsize(a.to_u64()),
            },
            pos,
            assignable: false,
        },
        TypeRepr::Float(FloatType::F32) => Expr {
            ty: target,
            kind: ExprKind::ConstF32(Float::new(a.to_f32())),
            pos,
            assignable: false,
        },
        TypeRepr::Float(FloatType::F64) => Expr {
            ty: target,
            kind: ExprKind::ConstF64(Float::new(a.to_f64())),
            pos,
            assignable: false,
        },
        _ => unreachable!(),
    }
}

fn cast_untyped_float<'a>(a: f64, pos: Pos, target: &'a Type<'a>) -> Expr<'a> {
    match target.repr {
        TypeRepr::Int(sign, bitsize) => Expr {
            ty: target,
            kind: match (sign, bitsize) {
                (false, BitSize::I8) => ExprKind::ConstI8(a as u8),
                (false, BitSize::I16) => ExprKind::ConstI16(a as u16),
                (false, BitSize::I32) => ExprKind::ConstI32(a as u32),
                (false, BitSize::I64) => ExprKind::ConstI64(a as u64),
                (false, BitSize::ISize) => ExprKind::ConstIsize(a as u64),
                (true, BitSize::I8) => ExprKind::ConstI8((a as i8) as u8),
                (true, BitSize::I16) => ExprKind::ConstI16((a as i16) as u16),
                (true, BitSize::I32) => ExprKind::ConstI32((a as i32) as u32),
                (true, BitSize::I64) => ExprKind::ConstI64((a as i64) as u64),
                (true, BitSize::ISize) => ExprKind::ConstIsize((a as i64) as u64),
            },
            pos,
            assignable: false,
        },
        TypeRepr::Float(FloatType::F32) => Expr {
            ty: target,
            kind: ExprKind::ConstF32(Float::new(a as f32)),
            pos,
            assignable: false,
        },
        TypeRepr::Float(FloatType::F64) => Expr {
            ty: target,
            kind: ExprKind::ConstF64(Float::new(a)),
            pos,
            assignable: false,
        },
        _ => unreachable!(),
    }
}

fn get_binary_bool_exprs<'a, T: BinopEvaluator, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    pos: Pos,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    let bool_ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });
    if a.ty.is_unknown() || b.ty.is_unknown() {
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "neither a nor b should have unknown type"
    );

    if !a.ty.is_bool() || !b.ty.is_bool() {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
        return Expr {
            ty: bool_ty,
            kind: ExprKind::Invalid,
            pos,
            assignable: false,
        };
    }

    Expr {
        ty: bool_ty,
        kind: T::build(a, b),
        pos,
        assignable: false,
    }
}

fn get_expr_from_deref_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &DerefExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node_internal(ctx, scope, &node.value);
    let ty = value.ty;
    let TypeRepr::Ptr(element_ty) = ty.repr else {
        if !ty.is_unknown() {
            ctx.errors.deref_non_pointer(node.pos);
        }
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
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
    let value = get_expr_from_node_internal(ctx, scope, &node.value);
    let ty = value.ty;

    if let ExprKind::ConstInt(ref val) = value.kind {
        if matches!(node.op, UnaryOp::BitNot) {
            return Expr {
                ty,
                kind: ExprKind::ConstInt(!val),
                pos: node.pos,
                assignable: false,
            };
        }
        if matches!(node.op, UnaryOp::Add) {
            return value;
        }
        if matches!(node.op, UnaryOp::Sub) {
            return Expr {
                ty,
                kind: ExprKind::ConstInt(-val),
                pos: node.pos,
                assignable: false,
            };
        }
    }

    if let ExprKind::ConstFloat(val) = value.kind {
        if matches!(node.op, UnaryOp::Add) {
            return value;
        }
        if matches!(node.op, UnaryOp::Sub) {
            return Expr {
                ty,
                kind: ExprKind::ConstFloat(Float::new(-val.value)),
                pos: node.pos,
                assignable: false,
            };
        }
    }

    let op_name = match node.op {
        UnaryOp::BitNot => "bit not",
        UnaryOp::Sub => "sub",
        UnaryOp::Add => "add",
        UnaryOp::Not => "not",
    };

    let is_bool = ty.is_bool();
    let is_arithmetic = ty.is_arithmetic();
    let is_int = ty.is_int();

    let type_id = value.ty;
    let (kind, is_valid) = match node.op {
        UnaryOp::BitNot => (ExprKind::BitNot(ctx.arena.alloc(value)), is_int),
        UnaryOp::Sub => (ExprKind::Neg(ctx.arena.alloc(value)), is_arithmetic),
        UnaryOp::Add => (value.kind, is_arithmetic),
        UnaryOp::Not => (ExprKind::Not(ctx.arena.alloc(value)), is_bool),
    };

    if !is_valid {
        ctx.errors.unop_type_unsupported(node.pos, op_name, ty);
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind,
            pos: node.pos,
            assignable: false,
        };
    }

    Expr {
        ty: type_id,
        kind,
        pos: node.pos,
        assignable: false,
    }
}

fn get_expr_from_call_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &CallExprNode,
) -> Expr<'a> {
    let func_expr = get_expr_from_node_internal(ctx, scope, &node.callee);
    let func_type = func_expr.ty;

    let TypeRepr::Func(func_type) = &func_type.repr else {
        if !func_type.is_unknown() {
            ctx.errors.not_callable(node.callee.pos());
        }
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.pos,
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

    let mut arguments = BumpVec::with_capacity_in(node.arguments.len(), ctx.arena);
    for (i, arg) in node.arguments.iter().enumerate() {
        let arg_expr = get_expr_from_node(ctx, scope, func_type.params.get(i).cloned(), arg);
        arguments.push(arg_expr);
    }

    for (i, (arg, param)) in zip(&arguments, func_type.params).enumerate() {
        if !param.is_assignable_with(arg.ty) {
            ctx.errors
                .type_mismatch(node.arguments[i].pos(), param, arg.ty);
        }
    }

    Expr {
        ty: func_type.return_type,
        kind: ExprKind::Call(ctx.arena.alloc(func_expr), arguments.into_bump_slice()),
        pos: node.pos,
        assignable: false,
    }
}

fn get_expr_from_cast_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &CastExprNode,
) -> Expr<'a> {
    let target_type = get_type_from_node(ctx, scope, &node.target);

    assert!(!matches!(
        target_type.repr,
        TypeRepr::UntypedFloat | TypeRepr::UntypedInt
    ));

    let value = get_expr_from_node_internal(ctx, scope, &node.value);
    let value_type = value.ty;

    match (&value.kind, &target_type.repr) {
        (ExprKind::ConstInt(val), TypeRepr::Int(..) | TypeRepr::Float(..)) => {
            return cast_untyped_int(&val, node.value.pos(), target_type);
        }
        (ExprKind::ConstInt(val), TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..)) => {
            return Expr {
                ty: target_type,
                kind: ExprKind::ConstIsize(val.to_u64()),
                pos: node.value.pos(),
                assignable: false,
            }
        }
        (ExprKind::ConstFloat(val), TypeRepr::Int(..) | TypeRepr::Float(..)) => {
            return cast_untyped_float(val.value, node.value.pos(), target_type);
        }
        _ => (),
    };

    let valid_casting = (value_type.is_integral() && target_type.is_integral())
        || (value_type.is_float() && target_type.is_float())
        || (value_type.is_integral() && target_type.is_float())
        || (value_type.is_float() && target_type.is_integral())
        || value_type.is_unknown()
        || target_type.is_unknown();

    let kind = if valid_casting {
        ExprKind::Cast(ctx.arena.alloc(value), target_type)
    } else {
        ctx.errors
            .casting_unsupported(node.value.pos(), value_type, target_type);
        ExprKind::Invalid
    };

    Expr {
        ty: target_type,
        kind,
        pos: node.value.pos(),
        assignable: false,
    }
}

fn get_expr_from_struct_lit_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &StructExprNode,
) -> Expr<'a> {
    let ty = get_type_from_node(ctx, scope, &node.target);

    let Some(struct_type) = ty.as_struct() else {
        if !ty.is_unknown() {
            ctx.errors.non_struct_type(node.target.pos());
        }
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.pos,
            assignable: false,
        };
    };
    let struct_body = struct_type.body.get().expect("missing struct body");

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
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                })
            });
        let value = get_expr_from_node(ctx, scope, Some(ty), &element.value);

        let value = if !ty.is_assignable_with(value.ty) {
            ctx.errors.type_mismatch(element.value.pos(), ty, value.ty);
            Expr {
                ty: ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                }),
                kind: ExprKind::Invalid,
                pos: element.pos,
                assignable: false,
            }
        } else {
            value
        };

        values.insert(field_name, value);
    }

    let mut full_values = BumpVec::with_capacity_in(struct_body.fields.len(), ctx.arena);
    for (field_name, type_id) in &struct_body.fields {
        if let Some(value) = values.remove(field_name) {
            full_values.push(value)
        } else {
            full_values.push(Expr {
                ty: type_id,
                kind: ExprKind::Zero,
                pos: node.pos,
                assignable: false,
            })
        }
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
    let value = get_expr_from_node_internal(ctx, scope, &node.value);

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
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.value.pos(),
            assignable: true,
        };
    };
    let struct_body = struct_type.body.get().expect("missing struct body");

    let selection_name = ctx.define_symbol(&node.selection.value);
    let Some((idx, _, field_type_id)) = struct_body.fields.get_full(&selection_name) else {
        ctx.errors
            .undeclared_field(node.selection.pos, &node.selection.value);
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.value.pos(),
            assignable: true,
        };
    };

    let assignable = value.assignable;
    if is_ptr {
        Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Ptr(field_type_id),
            }),
            kind: ExprKind::GetElementAddr(ctx.arena.alloc(value), idx),
            pos: node.value.pos(),
            assignable,
        }
    } else {
        Expr {
            ty: field_type_id,
            kind: ExprKind::GetElement(ctx.arena.alloc(value), idx),
            pos: node.value.pos(),
            assignable,
        }
    }
}

fn get_expr_from_index_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &IndexExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node_internal(ctx, scope, &node.value);
    let ty = value.ty;

    match ty.repr {
        TypeRepr::ArrayPtr(element) => {
            let index = get_expr_from_node(
                ctx,
                scope,
                Some(ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Int(true, BitSize::ISize),
                })),
                &node.index,
            );
            let index_type = index.ty;

            if !index_type.is_int() {
                ctx.errors.non_int_index(node.index.pos());
                return Expr {
                    ty: element,
                    kind: ExprKind::Invalid,
                    pos: node.value.pos(),
                    assignable: false,
                };
            }

            Expr {
                ty: ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Ptr(element),
                }),
                kind: ExprKind::GetIndex(ctx.arena.alloc(value), ctx.arena.alloc(index)),
                pos: node.value.pos(),
                assignable: false,
            }
        }
        TypeRepr::Unknown => Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            }),
            kind: ExprKind::Invalid,
            pos: node.value.pos(),
            assignable: false,
        },
        _ => {
            ctx.errors.not_indexable(node.value.pos());
            Expr {
                ty: ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                }),
                kind: ExprKind::Invalid,
                pos: node.value.pos(),
                assignable: false,
            }
        }
    }
}
