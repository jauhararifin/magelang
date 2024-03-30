use crate::interner::Interner;
use crate::ty::{BitSize, FloatType, IntSign};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use std::cell::OnceCell;
use std::hash::Hash;

pub(crate) type GenericTypeInterner<'a> = Interner<'a, GenericType<'a>>;

#[derive(PartialEq, Eq)]
pub struct GenericType<'a> {
    pub kind: GenericTypeKind<'a>,
    pub repr: GenericTypeRepr<'a>,
}

impl<'a> Hash for GenericType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
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
