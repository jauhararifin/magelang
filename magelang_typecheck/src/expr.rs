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
        self.source.eq(other.source)
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
    // TODO: do it more efficiently. Doing this way will make the
    // constant evaluation performed in O(N^2), whereas we actually
    // can do it in O(N).
    if let Some(constant) = get_const_from_node(node) {
        match constant {
            Constant::Number(val) => {
                if let Some(expected_type) = expected_type {
                    match expected_type.repr {
                        TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..) => {
                            return get_expr_from_int_lit(
                                ctx,
                                Some(expected_type),
                                node.pos(),
                                &val,
                            );
                        }
                        TypeRepr::Float(..) => {
                            return get_expr_from_float_lit(
                                ctx,
                                Some(expected_type),
                                node.pos(),
                                &val.string_repr(),
                                &val,
                            );
                        }
                        _ => (),
                    }
                } else {
                    if val.is_int() {
                        return get_expr_from_int_lit(ctx, None, node.pos(), &val);
                    } else {
                        return get_expr_from_float_lit(
                            ctx,
                            None,
                            node.pos(),
                            &val.string_repr(),
                            &val,
                        );
                    }
                }
            }
            Constant::Boolean(val) => {
                return Expr {
                    ty: ctx.define_type(Type {
                        kind: TypeKind::Anonymous,
                        repr: TypeRepr::Bool,
                    }),
                    kind: ExprKind::ConstBool(val),
                    pos: node.pos(),
                    assignable: false,
                };
            }
        }
    }

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
        ExprNode::Grouped(node) => get_expr_from_node(ctx, scope, expected_type, node),
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
                return get_expr_from_int_lit(
                    ctx,
                    expected_type,
                    number_lit.pos,
                    &number_lit.value,
                );
            }
            TypeRepr::Float(..) => {
                return get_expr_from_float_lit(
                    ctx,
                    expected_type,
                    number_lit.pos,
                    &number_lit.raw,
                    &number_lit.value,
                );
            }
            _ => (),
        }
    };

    if number_lit.value.is_int() {
        get_expr_from_int_lit(ctx, expected_type, number_lit.pos, &number_lit.value)
    } else {
        get_expr_from_float_lit(
            ctx,
            expected_type,
            number_lit.pos,
            &number_lit.raw,
            &number_lit.value,
        )
    }
}

fn get_expr_from_int_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    expected_type: Option<&'a Type<'a>>,
    pos: Pos,
    number_value: &Number,
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
        (true, BitSize::ISize) => i64::try_from(number_value)
            .map(|v| v as u64)
            .map(ExprKind::ConstIsize),
        (true, BitSize::I64) => i64::try_from(number_value)
            .map(|v| v as u64)
            .map(ExprKind::ConstI64),
        (true, BitSize::I32) => i32::try_from(number_value)
            .map(|v| v as u32)
            .map(ExprKind::ConstI32),
        (true, BitSize::I16) => i16::try_from(number_value)
            .map(|v| v as u16)
            .map(ExprKind::ConstI16),
        (true, BitSize::I8) => i8::try_from(number_value)
            .map(|v| v as u8)
            .map(ExprKind::ConstI8),
        (false, BitSize::ISize) => u64::try_from(number_value).map(ExprKind::ConstIsize),
        (false, BitSize::I64) => u64::try_from(number_value).map(ExprKind::ConstI64),
        (false, BitSize::I32) => u32::try_from(number_value).map(ExprKind::ConstI32),
        (false, BitSize::I16) => u16::try_from(number_value).map(ExprKind::ConstI16),
        (false, BitSize::I8) => u8::try_from(number_value).map(ExprKind::ConstI8),
    };

    let kind = match value {
        Ok(v) => v,
        Err(TryFromNumberError::OutOfRange) => {
            ctx.errors.overflowed_int_literal(pos);
            ExprKind::Invalid
        }
        Err(..) => {
            ctx.errors.invalid_int_literal(pos);
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
        pos,
        assignable: false,
    }
}

fn get_expr_from_float_lit<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    expected_type: Option<&'a Type<'a>>,
    pos: Pos,
    raw: &str,
    number_value: &Number,
) -> Expr<'a> {
    let mut float_type = FloatType::F64;
    if let Some(ty) = expected_type {
        if let TypeRepr::Float(float_ty) = ty.repr {
            float_type = float_ty;
        }
    }

    let value_sym = ctx.define_symbol(raw);
    let kind =
        match float_type {
            FloatType::F32 => f32::try_from(number_value)
                .map(|val| ExprKind::ConstF32(Float::new(value_sym, val))),
            FloatType::F64 => f64::try_from(number_value)
                .map(|val| ExprKind::ConstF64(Float::new(value_sym, val))),
        };

    let kind = match kind {
        Ok(v) => v,
        Err(err) => {
            ctx.errors.invalid_float_literal(pos, err);
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
        pos,
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
    let a = get_expr_from_node(ctx, scope, expected_type, &node.a);
    let b = get_expr_from_node(ctx, scope, Some(a.ty), &node.b);

    let op_name = match node.op {
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
    };

    let estimated_type = match node.op {
        BinaryOp::Eq
        | BinaryOp::NEq
        | BinaryOp::Gt
        | BinaryOp::GEq
        | BinaryOp::Lt
        | BinaryOp::LEq
        | BinaryOp::And
        | BinaryOp::Or => ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Bool,
        }),
        _ => a.ty,
    };

    if !a.ty.is_unknown() && !b.ty.is_unknown() && a.ty != b.ty {
        ctx.errors
            .binop_type_mismatch(node.a.pos(), op_name, a.ty, b.ty);
        return Expr {
            ty: estimated_type,
            kind: ExprKind::Invalid,
            pos: node.a.pos(),
            assignable: false,
        };
    }

    let result_ty = match node.op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            if a.ty.is_arithmetic() {
                a.ty
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                })
            }
        }
        BinaryOp::Eq | BinaryOp::NEq => {
            if a.ty.is_strictly_opaque() {
                let a_is_null = matches!(a.kind, ExprKind::Zero);
                let b_is_null = matches!(b.kind, ExprKind::Zero);
                if !a_is_null && !b_is_null && !a.ty.is_unknown() && !b.ty.is_unknown() {
                    ctx.errors.compare_opaque(node.a.pos());
                }
            }

            ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Bool,
            })
        }
        BinaryOp::Gt | BinaryOp::GEq | BinaryOp::Lt | BinaryOp::LEq => {
            if a.ty.is_arithmetic() {
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Bool,
                })
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                })
            }
        }
        BinaryOp::Mod
        | BinaryOp::BitOr
        | BinaryOp::BitAnd
        | BinaryOp::BitXor
        | BinaryOp::ShiftLeft
        | BinaryOp::ShiftRight => {
            if a.ty.is_int() {
                a.ty
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                })
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            if a.ty.is_bool() {
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Bool,
                })
            } else {
                ctx.errors
                    .binop_type_unsupported(node.a.pos(), op_name, a.ty);
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                })
            }
        }
    };

    let a = ctx.arena.alloc(a);
    let b = ctx.arena.alloc(b);
    let expr_kind = match node.op {
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
    };

    Expr {
        ty: result_ty,
        kind: expr_kind,
        pos: node.a.pos(),
        assignable: false,
    }
}

fn get_expr_from_deref_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected_type: Option<&'a Type<'a>>,
    node: &DerefExprNode,
) -> Expr<'a> {
    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
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
    let value = get_expr_from_node(ctx, scope, expected_type, &node.value);
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
    let func_expr = get_expr_from_node(ctx, scope, expected_type, &node.callee);
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
    expected_type: Option<&'a Type<'a>>,
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
    let value = get_expr_from_node(ctx, scope, None, &node.value);

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
    let value = get_expr_from_node(
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
