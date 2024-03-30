use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::interner::Interner;
use crate::ty::{BitSize, FloatType, IntSign, Type};
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, Pos, TypeParameterNode};
use std::cell::OnceCell;
use std::collections::HashMap;
use std::hash::Hash;

pub(crate) type GenericTypeInterner<'a> = Interner<'a, GenericType<'a>>;

// TODO: consider creating a new-type for type-args to implement hash, eq, and partial-eq
// to improve performance.
pub type TypeArgs<'a> = [&'a Type<'a>];
pub(crate) type TypeArgsInterner<'a> = Interner<'a, TypeArgs<'a>>;

#[derive(PartialEq, Eq)]
pub struct GenericType<'a> {
    pub kind: GenericTypeKind<'a>,
    pub params: &'a [TypeArg<'a>],
    pub repr: GenericTypeRepr<'a>,
}

impl<'a> Hash for GenericType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

impl<'a> GenericType<'a> {
    pub(crate) fn init_body<E: ErrorReporter>(&'a self, ctx: &Context<'a, E>) {
        todo!();
    }

    pub fn as_struct(&self) -> Option<&GenericStructType<'a>> {
        self.repr.as_struct()
    }

    pub(crate) fn monomorphize(&self, type_args: &'a TypeArgs<'a>) -> &'a Type<'a> {
        todo!();
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
