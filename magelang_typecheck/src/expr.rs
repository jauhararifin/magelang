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
use num::{BigInt, ToPrimitive, Zero};
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
        let ty = self.ty.monomorphize(ctx, type_args);

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
                let ty = ty.monomorphize(ctx, type_args);
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
                    .map(|ty| ty.monomorphize(ctx, type_args))
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
                into_type.monomorphize(ctx, type_args),
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
pub struct Float<T>(T);

impl<T> From<T> for Float<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> Float<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }
}

impl<T> Float<T>
where
    T: Copy,
{
    pub fn get(&self) -> T {
        self.0
    }
}

impl Hash for Float<f32> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as u64).hash(state)
    }
}

impl Hash for Float<f64> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as u64).hash(state)
    }
}

impl<T> PartialEq for Float<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for Float<T> where T: PartialEq {}

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
    // ShiftLeft and ShiftRight should have the same type for its left and right operand.
    // But, from user perspective, the types don't have to be equal, the type checker
    // should convert it to (by casting or constant evaluation) the type of left operand.
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

impl ExprKind<'_> {
    fn is_const(&self) -> bool {
        matches!(
            self,
            Self::ConstInt(..)
                | Self::ConstI8(..)
                | Self::ConstI16(..)
                | Self::ConstI32(..)
                | Self::ConstI64(..)
                | Self::ConstIsize(..)
                | Self::ConstFloat(..)
                | Self::ConstF32(..)
                | Self::ConstF64(..)
                | Self::ConstBool(..)
        )
    }
}

pub(crate) fn get_expr_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &ExprNode,
) -> Expr<'a> {
    // TODO: cast untyped expression to typed one
    get_expr_from_node_internal(ctx, scope, expected_type, node)
}

fn get_expr_from_node_internal<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &ExprNode,
) -> Expr<'a> {
    match node {
        ExprNode::Path(node) => get_expr_from_path(ctx, scope, expected_type, node),
        ExprNode::Number(num_lit) => get_expr_from_number_lit(ctx, expected_type, num_lit),
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
        ExprNode::Char(char_lit) => get_expr_from_char_lit(ctx, expected_type, char_lit),
        ExprNode::String(string_lit) => get_expr_from_string_lit(ctx, string_lit),
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
        ExprNode::Grouped(node) => get_expr_from_node_internal(ctx, scope, expected_type, node),
    }
}

fn get_expr_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &PathNode,
) -> Expr<'a> {
    let Some(object) = get_value_object_from_path(ctx, scope, &node.path) else {
        return Expr {
            ty: expected_type.unwrap_or(ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Unknown,
            })),
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

                let instance_ty = func_obj.ty.monomorphize(ctx, type_args);

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
    expected_type: Option<&'a Type<'a>>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    if let Some(ty) = expected_type {
        match ty.repr {
            TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => {
                return get_expr_from_int_lit(ctx, expected_type, number_lit);
            }
            TypeRepr::Float(..) => {
                return get_expr_from_float_lit(ctx, expected_type, number_lit);
            }
            _ => (),
        }
    };

    if number_lit.value.is_int() {
        get_expr_from_int_lit(ctx, expected_type, number_lit)
    } else {
        get_expr_from_float_lit(ctx, expected_type, number_lit)
    }
}

fn get_expr_from_int_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    expected_type: Option<&'a Type<'a>>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    let (mut sign, mut bit_size) = (true, BitSize::ISize);
    if let Some(ty) = expected_type {
        if let TypeRepr::Int(s, b) = ty.repr {
            sign = s;
            bit_size = b;
        } else if matches!(ty.repr, TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..)) {
            sign = false;
            bit_size = BitSize::ISize;
        }
    }

    let value = match (sign, bit_size) {
        (true, BitSize::ISize) => i64::try_from(&number_lit.value)
            .map(|v| v as u64)
            .map(ExprKind::ConstIsize),
        (true, BitSize::I64) => i64::try_from(&number_lit.value)
            .map(|v| v as u64)
            .map(ExprKind::ConstI64),
        (true, BitSize::I32) => i32::try_from(&number_lit.value)
            .map(|v| v as u32)
            .map(ExprKind::ConstI32),
        (true, BitSize::I16) => i16::try_from(&number_lit.value)
            .map(|v| v as u16)
            .map(ExprKind::ConstI16),
        (true, BitSize::I8) => i8::try_from(&number_lit.value)
            .map(|v| v as u8)
            .map(ExprKind::ConstI8),
        (false, BitSize::ISize) => u64::try_from(&number_lit.value).map(ExprKind::ConstIsize),
        (false, BitSize::I64) => u64::try_from(&number_lit.value).map(ExprKind::ConstI64),
        (false, BitSize::I32) => u32::try_from(&number_lit.value).map(ExprKind::ConstI32),
        (false, BitSize::I16) => u16::try_from(&number_lit.value).map(ExprKind::ConstI16),
        (false, BitSize::I8) => u8::try_from(&number_lit.value).map(ExprKind::ConstI8),
    };

    let kind = match value {
        Ok(v) => v,
        Err(TryFromNumberError::OutOfRange) => {
            ctx.errors.overflowed_int_literal(number_lit.pos);
            ExprKind::Invalid
        }
        Err(..) => {
            ctx.errors.invalid_int_literal(number_lit.pos);
            ExprKind::Invalid
        }
    };

    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(sign, bit_size),
    });
    Expr {
        ty,
        kind,
        pos: number_lit.pos,
        assignable: false,
    }
}

fn get_expr_from_float_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    expected_type: Option<&'a Type<'a>>,
    number_lit: &NumberLit,
) -> Expr<'a> {
    let mut float_type = FloatType::F64;
    if let Some(ty) = expected_type {
        if let TypeRepr::Float(float_ty) = ty.repr {
            float_type = float_ty;
        }
    }

    let kind = match float_type {
        FloatType::F32 => {
            f32::try_from(&number_lit.value).map(|val| ExprKind::ConstF32(Float::new(val)))
        }
        FloatType::F64 => {
            f64::try_from(&number_lit.value).map(|val| ExprKind::ConstF64(Float::new(val)))
        }
    };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_float_literal(number_lit.pos, err);
            ExprKind::Invalid
        }
    };

    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Float(float_type),
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
    expected_type: Option<&'a Type<'a>>,
    char_lit: &CharLit,
) -> Expr<'a> {
    let ch = char_lit.value;
    if let Some(ty) = expected_type {
        if let TypeRepr::Int(sign, bit_size) = ty.repr {
            let value = match (sign, bit_size) {
                (true, BitSize::ISize) => ExprKind::ConstIsize(ch as u64),
                (true, BitSize::I64) => ExprKind::ConstI64(ch as u64),
                (true, BitSize::I32) => ExprKind::ConstI32(ch as u32),
                (true, BitSize::I16) => ExprKind::ConstI16(ch as u16),
                (true, BitSize::I8) => ExprKind::ConstI8(ch as u8),
                (false, BitSize::ISize) => ExprKind::ConstIsize(ch as u64),
                (false, BitSize::I64) => ExprKind::ConstI64(ch as u64),
                (false, BitSize::I32) => ExprKind::ConstI32(ch as u32),
                (false, BitSize::I16) => ExprKind::ConstI16(ch as u16),
                (false, BitSize::I8) => ExprKind::ConstI8(ch as u8),
            };
            return Expr {
                ty,
                kind: value,
                pos: char_lit.pos,
                assignable: false,
            };
        }
    };

    let ty = expected_type.unwrap_or(ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I32),
    }));
    Expr {
        ty,
        kind: ExprKind::ConstI32(ch as u32),
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
    expected_type: Option<&'a Type<'a>>,
    node: &BinaryExprNode,
) -> Expr<'a> {
    let a = get_expr_from_node_internal(ctx, scope, expected_type, &node.a);
    let b = get_expr_from_node_internal(ctx, scope, Some(a.ty), &node.b);

    let a = ctx.arena.alloc(a);
    let b = ctx.arena.alloc(b);

    // There are 5 types of binary operator
    // * Equality:   == !=
    //   this compares two types and returns boolean
    //   the two types must have the same type
    // * Comparison: < <= > >=
    //   this compares two numeric type and returns boolean
    // * Arithmetic: + - *
    //   this compares two numeric type and returns its own type
    // * Div:        /
    //   div is special case of arithmetic since dividing by zero
    //   is not allowed
    // * Mod:        %
    //   mod is a special case of bit operation since mod by zeo
    //   is not allowed
    // * Bits:       & | ^ << >>
    //   this compares two integers type and returns its own type
    // * Shifts:     << >>
    //   this shifts the first integer with the second integer and
    //   returns the first integer type
    // * Bools:      && ||
    //   this compares two booleans and returns boolean

    match node.op {
        BinaryOp::Add => get_expr_from_binary_arithmetic_exprs::<E, AddBinopEval>(ctx, a, b),
        BinaryOp::Sub => get_expr_from_binary_arithmetic_exprs::<E, SubBinopEval>(ctx, a, b),
        BinaryOp::Mul => get_expr_from_binary_arithmetic_exprs::<E, MulBinopEval>(ctx, a, b),
        BinaryOp::Div => get_expr_from_binary_div_exprs(ctx, a, b),
        BinaryOp::Mod => get_expr_from_binary_mod_exprs(ctx, a, b),
        BinaryOp::BitOr => get_expr_from_binary_bit_exprs::<E, BitOrBinopEval>(ctx, a, b),
        BinaryOp::BitAnd => get_expr_from_binary_bit_exprs::<E, BitAndBinopEval>(ctx, a, b),
        BinaryOp::BitXor => get_expr_from_binary_bit_exprs::<E, BitXorBinopEval>(ctx, a, b),

        // for shifts operation, its quite complicated:
        // * both operand must be integer, but they can have different type
        // * the integer sign matter
        // * when compiled to wasm, both operand must either be i32 or i64
        // * for shift right, it keeps the sign, so it uses shr_s version in wasm
        // * negative shifts are illegal. It fails in rust and go
        // * shift with more bits than its left operand is also illegal. This fails in rust, but
        //   not in go
        BinaryOp::ShiftLeft => todo!(),
        BinaryOp::ShiftRight => todo!(),

        BinaryOp::And => get_expr_from_binary_boolean_exprs::<E, AndBinopEval>(ctx, a, b),
        BinaryOp::Or => get_expr_from_binary_boolean_exprs::<E, OrBinopEval>(ctx, a, b),

        BinaryOp::Eq => get_expr_from_binary_equality_exprs::<E, EqBinopEval>(ctx, a, b),
        BinaryOp::NEq => get_expr_from_binary_equality_exprs::<E, NeqBinopEval>(ctx, a, b),

        BinaryOp::Gt => get_expr_from_binary_comparison_exprs::<E, GtBinopEval>(ctx, a, b),
        BinaryOp::GEq => get_expr_from_binary_comparison_exprs::<E, GteBinopEval>(ctx, a, b),
        BinaryOp::Lt => get_expr_from_binary_comparison_exprs::<E, LtBinopEval>(ctx, a, b),
        BinaryOp::LEq => get_expr_from_binary_comparison_exprs::<E, LteBinopEval>(ctx, a, b),
    }
}

trait BinopEval<A, B> {
    type Output;
    fn eval(a: A, b: B) -> Self::Output;
}

trait BinopInfo {
    fn name() -> &'static str;

    fn build_expr_kind<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a>;
}

macro_rules! impl_binop_info {
    ($name:ident, $s:expr, $kind:ident) => {
        impl BinopInfo for $name {
            fn name() -> &'static str {
                $s
            }

            fn build_expr_kind<'a>(a: &'a Expr<'a>, b: &'a Expr<'a>) -> ExprKind<'a> {
                ExprKind::$kind(a, b)
            }
        }
    };
}

macro_rules! impl_binop_eval_for_equality {
    ($name:ident, $a:ty, $b:ty, $op:expr) => {
        impl BinopEval<$a, $b> for $name {
            type Output = bool;
            fn eval(a: $a, b: $b) -> Self::Output {
                $op(a, b)
            }
        }
    };
}

macro_rules! impl_binop_eval_for_arithmetic {
    ($name:ident, $a:ty, $b:ty, $op:ident) => {
        impl BinopEval<$a, $b> for $name {
            type Output = $a;
            fn eval(a: $a, b: $b) -> Self::Output {
                a.$op(b)
            }
        }
    };
    ($name:ident, $a:ty, $b:ty, $out:ty, $op:ident) => {
        impl BinopEval<$a, $b> for $name {
            type Output = $out;
            fn eval(a: $a, b: $b) -> Self::Output {
                a.$op(b)
            }
        }
    };
}

struct EqBinopEval;
struct NeqBinopEval;
struct LtBinopEval;
struct GtBinopEval;
struct LteBinopEval;
struct GteBinopEval;
struct AddBinopEval;
struct SubBinopEval;
struct MulBinopEval;
struct BitAndBinopEval;
struct BitOrBinopEval;
struct BitXorBinopEval;
struct ShlBinopEval;
struct ShrBinopEval;
struct AndBinopEval;
struct OrBinopEval;

impl_binop_info!(EqBinopEval, "eq", Eq);
impl_binop_info!(NeqBinopEval, "neq", NEq);
impl_binop_info!(LtBinopEval, "lt", Lt);
impl_binop_info!(LteBinopEval, "lte", LEq);
impl_binop_info!(GtBinopEval, "gt", Gt);
impl_binop_info!(GteBinopEval, "gte", GEq);
impl_binop_info!(AddBinopEval, "add", Add);
impl_binop_info!(SubBinopEval, "sub", Sub);
impl_binop_info!(MulBinopEval, "mul", Mul);
impl_binop_info!(BitAndBinopEval, "bitand", BitAnd);
impl_binop_info!(BitOrBinopEval, "bitor", BitOr);
impl_binop_info!(BitXorBinopEval, "bitxor", BitXor);
impl_binop_info!(AndBinopEval, "and", And);
impl_binop_info!(OrBinopEval, "or", Or);

impl_binop_eval_for_equality!(EqBinopEval, &BigInt, &BigInt, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &u8, &u8, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &u16, &u16, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &u32, &u32, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &u64, &u64, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &bool, &bool, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &f32, &f32, std::cmp::PartialEq::eq);
impl_binop_eval_for_equality!(EqBinopEval, &f64, &f64, std::cmp::PartialEq::eq);

impl_binop_eval_for_equality!(NeqBinopEval, &BigInt, &BigInt, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &u8, &u8, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &u16, &u16, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &u32, &u32, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &u64, &u64, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &bool, &bool, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &f32, &f32, std::cmp::PartialEq::ne);
impl_binop_eval_for_equality!(NeqBinopEval, &f64, &f64, std::cmp::PartialEq::ne);

impl_binop_eval_for_equality!(LtBinopEval, &BigInt, &BigInt, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &u8, &u8, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &u16, &u16, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &u32, &u32, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &u64, &u64, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &bool, &bool, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &f32, &f32, std::cmp::PartialOrd::lt);
impl_binop_eval_for_equality!(LtBinopEval, &f64, &f64, std::cmp::PartialOrd::lt);

impl_binop_eval_for_equality!(GtBinopEval, &BigInt, &BigInt, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &u8, &u8, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &u16, &u16, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &u32, &u32, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &u64, &u64, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &bool, &bool, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &f32, &f32, std::cmp::PartialOrd::gt);
impl_binop_eval_for_equality!(GtBinopEval, &f64, &f64, std::cmp::PartialOrd::gt);

impl_binop_eval_for_equality!(LteBinopEval, &BigInt, &BigInt, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &u8, &u8, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &u16, &u16, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &u32, &u32, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &u64, &u64, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &bool, &bool, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &f32, &f32, std::cmp::PartialOrd::le);
impl_binop_eval_for_equality!(LteBinopEval, &f64, &f64, std::cmp::PartialOrd::le);

impl_binop_eval_for_equality!(GteBinopEval, &BigInt, &BigInt, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &u8, &u8, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &u16, &u16, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &u32, &u32, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &u64, &u64, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &bool, &bool, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &f32, &f32, std::cmp::PartialOrd::ge);
impl_binop_eval_for_equality!(GteBinopEval, &f64, &f64, std::cmp::PartialOrd::ge);

use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Rem, Sub};
impl_binop_eval_for_arithmetic!(AddBinopEval, &BigInt, &BigInt, BigInt, add);
impl_binop_eval_for_arithmetic!(AddBinopEval, u8, u8, wrapping_add);
impl_binop_eval_for_arithmetic!(AddBinopEval, u16, u16, wrapping_add);
impl_binop_eval_for_arithmetic!(AddBinopEval, u32, u32, wrapping_add);
impl_binop_eval_for_arithmetic!(AddBinopEval, u64, u64, wrapping_add);
impl_binop_eval_for_arithmetic!(AddBinopEval, f32, f32, add);
impl_binop_eval_for_arithmetic!(AddBinopEval, f64, f64, add);

impl_binop_eval_for_arithmetic!(SubBinopEval, &BigInt, &BigInt, BigInt, sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, u8, u8, wrapping_sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, u16, u16, wrapping_sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, u32, u32, wrapping_sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, u64, u64, wrapping_sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, f32, f32, sub);
impl_binop_eval_for_arithmetic!(SubBinopEval, f64, f64, sub);

impl_binop_eval_for_arithmetic!(MulBinopEval, &BigInt, &BigInt, BigInt, mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, u8, u8, wrapping_mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, u16, u16, wrapping_mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, u32, u32, wrapping_mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, u64, u64, wrapping_mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, f32, f32, mul);
impl_binop_eval_for_arithmetic!(MulBinopEval, f64, f64, mul);

// division operation and mod operation are not implemented because it will be a special case

impl_binop_eval_for_arithmetic!(AndBinopEval, bool, bool, bitand);
impl_binop_eval_for_arithmetic!(OrBinopEval, bool, bool, bitor);

impl_binop_eval_for_arithmetic!(BitAndBinopEval, &BigInt, &BigInt, BigInt, bitand);
impl_binop_eval_for_arithmetic!(BitAndBinopEval, u8, u8, bitand);
impl_binop_eval_for_arithmetic!(BitAndBinopEval, u16, u16, bitand);
impl_binop_eval_for_arithmetic!(BitAndBinopEval, u32, u32, bitand);
impl_binop_eval_for_arithmetic!(BitAndBinopEval, u64, u64, bitand);

impl_binop_eval_for_arithmetic!(BitOrBinopEval, &BigInt, &BigInt, BigInt, bitor);
impl_binop_eval_for_arithmetic!(BitOrBinopEval, u8, u8, bitor);
impl_binop_eval_for_arithmetic!(BitOrBinopEval, u16, u16, bitor);
impl_binop_eval_for_arithmetic!(BitOrBinopEval, u32, u32, bitor);
impl_binop_eval_for_arithmetic!(BitOrBinopEval, u64, u64, bitor);

impl_binop_eval_for_arithmetic!(BitXorBinopEval, &BigInt, &BigInt, BigInt, bitxor);
impl_binop_eval_for_arithmetic!(BitXorBinopEval, u8, u8, bitxor);
impl_binop_eval_for_arithmetic!(BitXorBinopEval, u16, u16, bitxor);
impl_binop_eval_for_arithmetic!(BitXorBinopEval, u32, u32, bitxor);
impl_binop_eval_for_arithmetic!(BitXorBinopEval, u64, u64, bitxor);

//impl_binop_eval_for_arithmetic!(ShlBinopEval, u8, u8, std::ops::Shl::<u8>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u8, u16, std::ops::Shl::<u16>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u8, u32, std::ops::Shl::<u32>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u8, u64, std::ops::Shl::<u64>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u16, u8, std::ops::Shl::<u8>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u16, u16, std::ops::Shl::<u16>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u16, u32, std::ops::Shl::<u32>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u16, u64, std::ops::Shl::<u64>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u32, u8, std::ops::Shl::<u8>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u32, u16, std::ops::Shl::<u16>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u32, u32, std::ops::Shl::<u32>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u32, u64, std::ops::Shl::<u64>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u64, u8, std::ops::Shl::<u8>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u64, u16, std::ops::Shl::<u16>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u64, u32, std::ops::Shl::<u32>::shl);
//impl_binop_eval_for_arithmetic!(ShlBinopEval, u64, u64, std::ops::Shl::<u64>::shl);
//
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u8, u8, std::ops::Shr::<u8>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u8, u16, std::ops::Shr::<u16>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u8, u32, std::ops::Shr::<u32>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u8, u64, std::ops::Shr::<u64>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u16, u8, std::ops::Shr::<u8>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u16, u16, std::ops::Shr::<u16>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u16, u32, std::ops::Shr::<u32>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u16, u64, std::ops::Shr::<u64>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u32, u8, std::ops::Shr::<u8>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u32, u16, std::ops::Shr::<u16>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u32, u32, std::ops::Shr::<u32>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u32, u64, std::ops::Shr::<u64>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u64, u8, std::ops::Shr::<u8>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u64, u16, std::ops::Shr::<u16>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u64, u32, std::ops::Shr::<u32>::shr);
//impl_binop_eval_for_arithmetic!(ShrBinopEval, u64, u64, std::ops::Shr::<u64>::shr);

fn get_expr_from_binary_equality_exprs<'a, E: ErrorReporter, T>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a>
where
    for<'b> T: BinopInfo
        + BinopEval<&'b u8, &'b u8, Output = bool>
        + BinopEval<&'b u16, &'b u16, Output = bool>
        + BinopEval<&'b u32, &'b u32, Output = bool>
        + BinopEval<&'b u64, &'b u64, Output = bool>
        + BinopEval<&'b f32, &'b f32, Output = bool>
        + BinopEval<&'b f64, &'b f64, Output = bool>
        + BinopEval<&'b bool, &'b bool, Output = bool>
        + BinopEval<&'b BigInt, &'b BigInt, Output = bool>,
{
    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    assert!(
        a.ty.repr.is_untyped() == b.ty.repr.is_untyped(),
        "a and b should either both typed or both untyped"
    );

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstFloat(va), ExprKind::ConstFloat(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstInt(..), ExprKind::ConstFloat(..))
        | (ExprKind::ConstFloat(..), ExprKind::ConstInt(..)) => {
            unreachable!("when both are untyped, both should be int or float")
        }
        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstF32(va), ExprKind::ConstF32(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstF64(va), ExprKind::ConstF64(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstBool(va), ExprKind::ConstBool(vb)) => Some(T::eval(va, vb)),
        _ => None,
    };

    let pos = a.pos;
    if let Some(val) = const_eval {
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Bool,
            }),
            kind: ExprKind::ConstBool(val),
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.kind.is_const() || !b.kind.is_const(),
        "either a or b should not be const. otherwise, it should've been returned by the const_eval above",
    );
    assert!(!a.ty.repr.is_untyped(), "a can't be untyped");
    assert!(!b.ty.repr.is_untyped(), "b can't be untyped");

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
    } else if a.ty.is_strictly_opaque() {
        let a_is_null = matches!(a.kind, ExprKind::Zero);
        let b_is_null = matches!(b.kind, ExprKind::Zero);
        if !a_is_null && !b_is_null && !a.ty.is_unknown() && !b.ty.is_unknown() {
            ctx.errors.compare_opaque(pos);
        }
    }

    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });

    Expr {
        ty,
        kind: T::build_expr_kind(a, b),
        pos,
        assignable: false,
    }
}

fn get_expr_from_binary_comparison_exprs<'a, E: ErrorReporter, T>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a>
where
    for<'b> T: BinopInfo
        + BinopEval<&'b u8, &'b u8, Output = bool>
        + BinopEval<&'b u16, &'b u16, Output = bool>
        + BinopEval<&'b u32, &'b u32, Output = bool>
        + BinopEval<&'b u64, &'b u64, Output = bool>
        + BinopEval<&'b f32, &'b f32, Output = bool>
        + BinopEval<&'b f64, &'b f64, Output = bool>
        + BinopEval<&'b bool, &'b bool, Output = bool>
        + BinopEval<&'b BigInt, &'b BigInt, Output = bool>,
{
    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    assert!(
        a.ty.repr.is_untyped() == b.ty.repr.is_untyped(),
        "a and b should either both typed or both untyped"
    );

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstFloat(va), ExprKind::ConstFloat(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstInt(..), ExprKind::ConstFloat(..))
        | (ExprKind::ConstFloat(..), ExprKind::ConstInt(..)) => {
            unreachable!("when both are untyped, both should be int or float")
        }
        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => Some(T::eval(va, vb)),
        (ExprKind::ConstF32(va), ExprKind::ConstF32(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstF64(va), ExprKind::ConstF64(vb)) => Some(T::eval(&va.0, &vb.0)),
        (ExprKind::ConstBool(va), ExprKind::ConstBool(vb)) => Some(T::eval(va, vb)),
        _ => None,
    };

    let pos = a.pos;
    if let Some(val) = const_eval {
        return Expr {
            ty: ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Bool,
            }),
            kind: ExprKind::ConstBool(val),
            pos,
            assignable: false,
        };
    }

    assert!(
        !a.kind.is_const() || !b.kind.is_const(),
        "either a or b should not be const. otherwise, it should've been returned by the const_eval above",
    );
    assert!(!a.ty.repr.is_untyped(), "a can't be untyped");
    assert!(!b.ty.repr.is_untyped(), "b can't be untyped");

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors.binop_type_mismatch(pos, T::name(), a.ty, b.ty);
    } else if !a.ty.is_arithmetic() {
        ctx.errors.binop_type_unsupported(a.pos, T::name(), a.ty);
    }

    let ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });

    Expr {
        ty,
        kind: T::build_expr_kind(a, b),
        pos,
        assignable: false,
    }
}

fn get_expr_from_binary_arithmetic_exprs<'a, E: ErrorReporter, T>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a>
where
    for<'b> T: BinopInfo
        + BinopEval<u8, u8, Output = u8>
        + BinopEval<u16, u16, Output = u16>
        + BinopEval<u32, u32, Output = u32>
        + BinopEval<u64, u64, Output = u64>
        + BinopEval<f32, f32, Output = f32>
        + BinopEval<f64, f64, Output = f64>
        + BinopEval<&'b BigInt, &'b BigInt, Output = BigInt>,
{
    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    assert!(
        a.ty.repr.is_untyped() == b.ty.repr.is_untyped(),
        "a and b should either both typed or both untyped, but a is {:?} and b is {:?}",
        a.ty,
        b.ty,
    );

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => {
            Some(ExprKind::ConstInt(T::eval(va, vb)))
        }
        (ExprKind::ConstFloat(va), ExprKind::ConstFloat(vb)) => {
            Some(ExprKind::ConstFloat(Float::new(T::eval(va.0, vb.0))))
        }
        (ExprKind::ConstInt(..), ExprKind::ConstFloat(..))
        | (ExprKind::ConstFloat(..), ExprKind::ConstInt(..)) => {
            unreachable!("when both are untyped, both should be int or float")
        }

        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => {
            Some(ExprKind::ConstI8(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => {
            Some(ExprKind::ConstI16(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => {
            Some(ExprKind::ConstI32(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => {
            Some(ExprKind::ConstI64(T::eval(*va, *vb)))
        }
        (ExprKind::ConstF32(va), ExprKind::ConstF32(vb)) => {
            Some(ExprKind::ConstF32(Float::new(T::eval(va.0, vb.0))))
        }
        (ExprKind::ConstF64(va), ExprKind::ConstF64(vb)) => {
            Some(ExprKind::ConstF64(Float::new(T::eval(va.0, vb.0))))
        }
        _ => None,
    };

    if let Some(kind) = const_eval {
        return Expr {
            ty: a.ty,
            kind,
            pos: a.pos,
            assignable: false,
        };
    }

    assert!(
        !a.kind.is_const() || !b.kind.is_const(),
        "either a or b should not be const. otherwise, it should've been returned by the const_eval above",
    );
    assert!(!a.ty.repr.is_untyped(), "a can't be untyped");
    assert!(!b.ty.repr.is_untyped(), "b can't be untyped");

    let mut ty = a.ty;

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors.binop_type_mismatch(a.pos, T::name(), a.ty, b.ty);
        ty = ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    } else if !a.ty.is_arithmetic() {
        ctx.errors.binop_type_unsupported(a.pos, T::name(), a.ty);
        ty = ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    }

    Expr {
        ty,
        kind: T::build_expr_kind(a, b),
        pos: a.pos,
        assignable: false,
    }
}

fn get_expr_from_binary_div_exprs<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    let invalid_expr = Expr {
        ty: ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        }),
        kind: ExprKind::Invalid,
        pos: a.pos,
        assignable: false,
    };

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return invalid_expr;
    }

    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    assert!(
        a.ty.repr.is_untyped() == b.ty.repr.is_untyped(),
        "a and b should either both typed or both untyped"
    );
    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "a and b shouldn't be an unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(a.pos, "div", a.ty, b.ty);
        return invalid_expr;
    } else if !a.ty.is_arithmetic() {
        ctx.errors.binop_type_unsupported(a.pos, "div", a.ty);
        return invalid_expr;
    }

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => {
            if vb.is_zero() {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else {
                Some(ExprKind::ConstInt(va / vb))
            }
        }
        (ExprKind::ConstFloat(va), ExprKind::ConstFloat(vb)) => {
            Some(ExprKind::ConstFloat(Float::new(va.0 / vb.0)))
        }
        (ExprKind::ConstInt(..), ExprKind::ConstFloat(..))
        | (ExprKind::ConstFloat(..), ExprKind::ConstInt(..)) => {
            unreachable!("when both are untyped, both should be int or float")
        }

        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI8(((*va as i8) / (*vb as i8)) as u8))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI8(*va / *vb))
            }
        }
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI16(((*va as i16) / (*vb as i16)) as u16))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI16(*va / *vb))
            }
        }
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI32(((*va as i32) / (*vb as i32)) as u32))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI32(*va / *vb))
            }
        }
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI64(((*va as i64) / (*vb as i64)) as u64))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI64(*va / *vb))
            }
        }

        (ExprKind::ConstF32(va), ExprKind::ConstF32(vb)) => {
            Some(ExprKind::ConstF32(Float::new(va.0 / vb.0)))
        }
        (ExprKind::ConstF64(va), ExprKind::ConstF64(vb)) => {
            Some(ExprKind::ConstF64(Float::new(va.0 / vb.0)))
        }
        _ => None,
    };

    if let Some(kind) = const_eval {
        return Expr {
            ty: a.ty,
            kind,
            pos: a.pos,
            assignable: false,
        };
    }

    assert!(
        !a.kind.is_const() || !b.kind.is_const(),
        "either a or b should not be const. otherwise, it should've been returned by the const_eval above",
    );
    assert!(!a.ty.repr.is_untyped(), "a can't be untyped");
    assert!(!b.ty.repr.is_untyped(), "b can't be untyped");

    let mut ty = a.ty;

    Expr {
        ty,
        kind: ExprKind::Div(a, b),
        pos: a.pos,
        assignable: false,
    }
}

fn get_expr_from_binary_mod_exprs<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a> {
    let invalid_expr = Expr {
        ty: ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        }),
        kind: ExprKind::Invalid,
        pos: a.pos,
        assignable: false,
    };

    if a.ty.is_unknown() || b.ty.is_unknown() {
        return invalid_expr;
    }

    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    assert!(
        a.ty.repr.is_untyped() == b.ty.repr.is_untyped(),
        "a and b should either both typed or both untyped"
    );
    assert!(
        !a.ty.is_unknown() && !b.ty.is_unknown(),
        "a and b shouldn't be an unknown type"
    );

    if a.ty != b.ty {
        ctx.errors.binop_type_mismatch(a.pos, "mod", a.ty, b.ty);
        return invalid_expr;
    } else if !a.ty.is_int() && !a.ty.is_untyped_int() {
        ctx.errors.binop_type_unsupported(a.pos, "mod", a.ty);
        return invalid_expr;
    }

    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    // a and b could be untyped, but it should report a compilation error

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => {
            if vb.is_zero() {
                ctx.errors.mod_by_zero(a.pos);
                return invalid_expr;
            } else {
                Some(ExprKind::ConstInt(va % vb))
            }
        }
        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI8(((*va as i8) % (*vb as i8)) as u8))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI8(*va % *vb))
            }
        }
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI16(((*va as i16) % (*vb as i16)) as u16))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI16(*va % *vb))
            }
        }
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI32(((*va as i32) % (*vb as i32)) as u32))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI32(*va % *vb))
            }
        }
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => {
            if *vb == 0 {
                ctx.errors.division_by_zero(a.pos);
                return invalid_expr;
            } else if a.ty.is_signed_int() {
                assert!(b.ty.is_signed_int());
                Some(ExprKind::ConstI64(((*va as i64) % (*vb as i64)) as u64))
            } else {
                assert!(!a.ty.is_signed_int() && !b.ty.is_signed_int());
                Some(ExprKind::ConstI64(*va % *vb))
            }
        }
        _ => None,
    };

    if let Some(kind) = const_eval {
        return Expr {
            ty: a.ty,
            kind,
            pos: a.pos,
            assignable: false,
        };
    }

    Expr {
        ty: a.ty,
        kind: ExprKind::Mod(a, b),
        pos: a.pos,
        assignable: false,
    }
}

fn get_expr_from_binary_bit_exprs<'a, E: ErrorReporter, T>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a>
where
    for<'b> T: BinopInfo
        + BinopEval<u8, u8, Output = u8>
        + BinopEval<u16, u16, Output = u16>
        + BinopEval<u32, u32, Output = u32>
        + BinopEval<u64, u64, Output = u64>
        + BinopEval<&'b BigInt, &'b BigInt, Output = BigInt>,
{
    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    // a and b could be untyped, but it should report a compilation error

    let const_eval = match (&a.kind, &b.kind) {
        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => {
            Some(ExprKind::ConstInt(T::eval(va, vb)))
        }
        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => {
            Some(ExprKind::ConstI8(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => {
            Some(ExprKind::ConstI16(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => {
            Some(ExprKind::ConstI32(T::eval(*va, *vb)))
        }
        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => {
            Some(ExprKind::ConstI64(T::eval(*va, *vb)))
        }
        _ => None,
    };

    if let Some(kind) = const_eval {
        return Expr {
            ty: a.ty,
            kind,
            pos: a.pos,
            assignable: false,
        };
    }

    // a and b might be a const since we don't evaluate float constant
    // also, a and b might an untyped expr

    let mut ty = a.ty;

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors.binop_type_mismatch(a.pos, T::name(), a.ty, b.ty);
        ty = ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    } else if !a.ty.is_int() && !a.ty.is_untyped_int() {
        ctx.errors.binop_type_unsupported(a.pos, T::name(), a.ty);
        ty = ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    }

    Expr {
        ty,
        kind: T::build_expr_kind(a, b),
        pos: a.pos,
        assignable: false,
    }
}

fn get_expr_from_binary_boolean_exprs<'a, E: ErrorReporter, T>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> Expr<'a>
where
    for<'b> T: BinopInfo + BinopEval<bool, bool, Output = bool>,
{
    let (a, b) = (
        cast_untyped_number_if_necessary(ctx, a, b),
        cast_untyped_number_if_necessary(ctx, b, a),
    );

    // a and b could be untyped, but it should report a compilation error

    let bool_ty = ctx.define_type(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });
    if let (ExprKind::ConstBool(va), ExprKind::ConstBool(vb)) = (&a.kind, &b.kind) {
        return Expr {
            ty: bool_ty,
            kind: ExprKind::ConstBool(T::eval(*va, *vb)),
            pos: a.pos,
            assignable: false,
        };
    }

    // a and b might be a const since we don't evaluate float constant
    // also, a and b might an untyped expr

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors.binop_type_mismatch(a.pos, T::name(), a.ty, b.ty);
    } else if !a.ty.is_bool() && !a.ty.is_bool() {
        ctx.errors.binop_type_unsupported(a.pos, T::name(), a.ty);
    }

    Expr {
        ty: bool_ty,
        kind: T::build_expr_kind(a, b),
        pos: a.pos,
        assignable: false,
    }
}

// if a is either an untyped integer or untyped float, while b is typed integer or typed float,
// a should be casted into the respective typed integer or float.
// if a is an untyped integer while b is untyped float, a will be casted to untyped float
fn cast_untyped_number_if_necessary<'a, E>(
    ctx: &Context<'a, '_, E>,
    a: &'a Expr<'a>,
    b: &'a Expr<'a>,
) -> &'a Expr<'a> {
    let result = if let ExprKind::ConstInt(ref v) = a.kind {
        match &b.ty.repr {
            TypeRepr::Int(.., BitSize::I8) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI8(v.to_u8().expect("bigint should be convertable to u8")),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I16) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI16(v.to_u16().expect("bigint should be convertable to u16")),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I32) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI32(v.to_u32().expect("bigint should be convertable to u32")),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I64) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI64(v.to_u64().expect("bigint should be convertable to u64")),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::ISize) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstIsize(
                    v.to_u64().expect("bigint should be convertable to u64"),
                ),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::UntypedFloat => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstFloat(Float::new(
                    v.to_f64()
                        .expect("bigint should be convertable to f64")
                        .into(),
                )),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Float(FloatType::F32) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstF32(
                    v.to_f32()
                        .expect("bigint should be convertable to f32")
                        .into(),
                ),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Float(FloatType::F64) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstF64(
                    v.to_f64()
                        .expect("bigint should be convertable to f64")
                        .into(),
                ),
                pos: a.pos,
                assignable: false,
            }),
            _ => None,
        }
    } else if let ExprKind::ConstFloat(ref v) = a.kind {
        match &b.ty.repr {
            TypeRepr::Int(.., BitSize::I8) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI8(v.0 as u8),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I16) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI16(v.0 as u16),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I32) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI32(v.0 as u32),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::I64) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstI64(v.0 as u64),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Int(.., BitSize::ISize) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstIsize(v.0 as u64),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Float(FloatType::F32) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstF32((v.0 as f32).into()),
                pos: a.pos,
                assignable: false,
            }),
            TypeRepr::Float(FloatType::F64) => Some(Expr {
                ty: b.ty,
                kind: ExprKind::ConstF64(*v),
                pos: a.pos,
                assignable: false,
            }),
            _ => None,
        }
    } else {
        None
    };

    if let Some(result) = result {
        ctx.arena.alloc(result)
    } else {
        a
    }
}

//fn get_expr_from_eq_node<'a, E: ErrorReporter>(
//    ctx: &Context<'a, '_, E>,
//    pos: Pos,
//    a: &'a Expr<'a>,
//    b: &'a Expr<'a>,
//) -> Expr<'a> {
//    let bool_ty = ctx.define_type(Type {
//        kind: TypeKind::Anonymous,
//        repr: TypeRepr::Bool,
//    });
//
//    let x = cast_untyped_number_if_necessary(ctx, a, b);
//    let y = cast_untyped_number_if_necessary(ctx, b, a);
//    let (a, b) = (x, y);
//
//    match (&a.kind, &b.kind) {
//        (ExprKind::ConstInt(va), ExprKind::ConstInt(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstI8(va), ExprKind::ConstI8(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstI16(va), ExprKind::ConstI16(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstI32(va), ExprKind::ConstI32(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstI64(va), ExprKind::ConstI64(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstIsize(va), ExprKind::ConstIsize(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstF32(va), ExprKind::ConstF32(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va.0 == vb.0),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstF64(va), ExprKind::ConstF64(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va.0 == vb.0),
//            pos,
//            assignable: false,
//        },
//        (ExprKind::ConstBool(va), ExprKind::ConstBool(vb)) => Expr {
//            ty: bool_ty,
//            kind: ExprKind::ConstBool(va == vb),
//            pos,
//            assignable: false,
//        },
//        _ => {
//            // if an untyped number meets a typed int, we need to cast the untyped number to the
//            // correct int type
//            // if an untyped number meets a typed float, we need to cast the untyped number to the
//            // correct float type
//            if matches!(a.ty.repr, TypeRepr::UntypedInt) && b.ty.repr.is_int() {
//                todo!();
//            } else if a.ty.repr.is_int() && matches!(b.ty.repr, TypeRepr::UntypedInt) {
//                todo!();
//            } else if matches!(a.ty.repr, TypeRepr::UntypedFloat) && b.ty.is_int() {
//                todo!();
//            } else if a.ty.repr.is_int() && matches!(b.ty.repr, TypeRepr::UntypedFloat) {
//                todo!();
//            } else if matches!(a.ty.repr, TypeRepr::UntypedInt) && b.ty.repr.is_float() {
//                todo!();
//            } else if a.ty.repr.is_float() && matches!(b.ty.repr, TypeRepr::UntypedInt) {
//                todo!();
//            } else if matches!(a.ty.repr, TypeRepr::UntypedFloat) && b.ty.is_float() {
//                todo!();
//            } else if a.ty.repr.is_float() && matches!(b.ty.repr, TypeRepr::UntypedFloat) {
//                todo!();
//            }
//
//            // handle non constant
//            if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
//                ctx.errors.binop_type_mismatch(pos, "eq", a.ty, b.ty);
//            } else if a.ty.is_strictly_opaque() {
//                let a_is_null = matches!(a.kind, ExprKind::Zero);
//                let b_is_null = matches!(b.kind, ExprKind::Zero);
//                if !a_is_null && !b_is_null && !a.ty.is_unknown() && !b.ty.is_unknown() {
//                    ctx.errors.compare_opaque(pos);
//                }
//            }
//
//            Expr {
//                ty: bool_ty,
//                kind: ExprKind::Eq(a, b),
//                pos,
//                assignable: false,
//            }
//        }
//    }
//}

fn get_expr_from_deref_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &DerefExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node_internal(ctx, scope, expected_type, &node.value);
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
    expected_type: Option<&'a Type<'a>>,
    node: &UnaryExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node_internal(ctx, scope, expected_type, &node.value);
    let ty = value.ty;

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
    expected_type: Option<&'a Type<'a>>,
    node: &CallExprNode,
) -> Expr<'a> {
    let func_expr = get_expr_from_node_internal(ctx, scope, expected_type, &node.callee);
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
        let arg_expr =
            get_expr_from_node_internal(ctx, scope, func_type.params.get(i).cloned(), arg);
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
    expected_type: Option<&'a Type<'a>>,
    node: &CastExprNode,
) -> Expr<'a> {
    let target_type = get_type_from_node(ctx, scope, &node.target);

    let value = get_expr_from_node_internal(ctx, scope, expected_type, &node.value);
    let value_type = value.ty;

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
        let value = get_expr_from_node_internal(ctx, scope, Some(ty), &element.value);

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
    let value = get_expr_from_node_internal(ctx, scope, None, &node.value);

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
    expected_type: Option<&'a Type<'a>>,
    node: &IndexExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node_internal(
        ctx,
        scope,
        expected_type.map(|elem_ty| {
            ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::ArrayPtr(elem_ty),
            })
        }),
        &node.value,
    );
    let ty = value.ty;

    match ty.repr {
        TypeRepr::ArrayPtr(element) => {
            let index = get_expr_from_node_internal(
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
