use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::interner::Interner;
use crate::ty::{BitSize, FloatType, FuncType, IntSign, StructType, Type, TypeKind, TypeRepr};
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, Pos, TypeParameterNode};
use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::hash::Hash;

pub(crate) type GenericTypeInterner<'a> = Interner<'a, GenericType<'a>>;

// TODO: consider creating a new-type for type-args to implement hash, eq, and partial-eq
// to improve performance.
pub type TypeArgs<'a> = [&'a Type<'a>];
pub(crate) type TypeArgsInterner<'a> = Interner<'a, TypeArgs<'a>>;

#[derive(PartialEq, Eq)]
pub(crate) struct GenericType<'a> {
    pub(crate) kind: GenericTypeKind<'a>,
    pub(crate) params: &'a [TypeArg<'a>],
    pub(crate) repr: GenericTypeRepr<'a>,

    mono_cache: RefCell<HashMap<&'a TypeArgs<'a>, &'a Type<'a>>>,
}

impl<'a> Hash for GenericType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

impl<'a> GenericType<'a> {
    pub(crate) fn new(
        kind: GenericTypeKind<'a>,
        params: &'a [TypeArg<'a>],
        repr: GenericTypeRepr<'a>,
    ) -> Self {
        Self {
            kind,
            params,
            repr,
            mono_cache: RefCell::default(),
        }
    }

    pub(crate) fn init_body<E: ErrorReporter>(&'a self, ctx: &Context<'a, E>) {
        todo!();
    }

    pub fn as_struct(&self) -> Option<&GenericStructType<'a>> {
        self.repr.as_struct()
    }

    pub(crate) fn monomorphize_shallow<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        {
            let mut mono_cache = self.mono_cache.borrow_mut();
            if let Some(ty) = mono_cache.get(type_args) {
                return ty;
            }

            if let GenericTypeRepr::Struct(..) = &self.repr {
                let GenericTypeKind::User(def_id) = self.kind else {
                    unreachable!("generic struct must be defined by the user");
                };

                let ty = ctx.types.define(Type {
                    kind: TypeKind::Inst(def_id, type_args),
                    repr: TypeRepr::Struct(StructType {
                        body: OnceCell::default(),
                    }),
                });

                mono_cache.insert(type_args, ty);
                return ty;
            }
        }

        let kind = match self.kind {
            GenericTypeKind::User(def_id) => TypeKind::Inst(def_id, type_args),
            GenericTypeKind::Anonymous => TypeKind::Anonymous,
        };

        let repr = match &self.repr {
            GenericTypeRepr::Unknown => TypeRepr::Unknown,
            GenericTypeRepr::Struct(..) => {
                unreachable!("generic struct already handled specially")
            }
            GenericTypeRepr::Func(func_type) => {
                TypeRepr::Func(func_type.monomorphize(ctx, type_args))
            }
            GenericTypeRepr::Void => TypeRepr::Void,
            GenericTypeRepr::Opaque => TypeRepr::Opaque,
            GenericTypeRepr::Bool => TypeRepr::Bool,
            GenericTypeRepr::Int(sign, size) => TypeRepr::Int(*sign, *size),
            GenericTypeRepr::Float(float_ty) => TypeRepr::Float(*float_ty),
            GenericTypeRepr::Ptr(el) => TypeRepr::Ptr(el.monomorphize_shallow(ctx, type_args)),
            GenericTypeRepr::ArrayPtr(el) => {
                TypeRepr::ArrayPtr(el.monomorphize_shallow(ctx, type_args))
            }
            GenericTypeRepr::TypeArg(arg) => {
                return type_args
                    .get(arg.index)
                    .expect("missing type arg at the index")
            }
        };

        ctx.types.define(Type { kind, repr })
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum GenericTypeKind<'a> {
    User(DefId<'a>),
    Anonymous,
}

#[derive(PartialEq, Eq)]
pub enum GenericTypeRepr<'a> {
    Unknown,
    Struct(GenericStructType<'a>),
    Func(GenericFuncType<'a>),
    Void,
    Opaque,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(&'a GenericType<'a>),
    ArrayPtr(&'a GenericType<'a>),
    TypeArg(TypeArg<'a>),
}

impl<'a> GenericTypeRepr<'a> {
    pub fn as_struct(&self) -> Option<&GenericStructType<'a>> {
        if let Self::Struct(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct GenericStructType<'a> {
    pub body: OnceCell<GenericStructBody<'a>>,
}

#[derive(PartialEq, Eq)]
pub struct GenericStructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a GenericType<'a>>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct GenericFuncType<'a> {
    pub params: &'a [&'a GenericType<'a>],
    pub return_type: &'a GenericType<'a>,
}

impl<'a> GenericFuncType<'a> {
    pub(crate) fn monomorphize<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> FuncType<'a> {
        let mut params = BumpVec::with_capacity_in(self.params.len(), ctx.arena);
        for ty in self.params {
            params.push(ty.monomorphize_shallow(ctx, type_args));
        }
        let return_type = self.return_type.monomorphize_shallow(ctx, type_args);
        FuncType {
            params: params.into_bump_slice(),
            return_type,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeArg<'a> {
    pub(crate) index: usize,
    pub(crate) name: Symbol<'a>,
}

impl<'a> TypeArg<'a> {
    pub(crate) fn new(index: usize, name: Symbol<'a>) -> Self {
        Self { index, name }
    }
}

pub(crate) fn get_typeparams<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    nodes: &[TypeParameterNode],
) -> &'a [TypeArg<'a>] {
    let mut type_params = BumpVec::with_capacity_in(nodes.len(), ctx.arena);
    let mut param_pos = HashMap::<Symbol, Pos>::default();
    for (i, type_param) in nodes.iter().enumerate() {
        let name = ctx.symbols.define(type_param.name.value.as_str());
        type_params.push(TypeArg::new(i, name));
        if let Some(declared_at) = param_pos.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(type_param.name.pos, declared_at, name);
        } else {
            param_pos.insert(name, type_param.name.pos);
        }
    }
    type_params.into_bump_slice()
}
