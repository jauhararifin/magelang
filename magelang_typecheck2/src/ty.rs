use crate::interner::Interner;
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use std::cell::OnceCell;
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;

#[derive(PartialEq, Eq)]
pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub repr: TypeRepr<'a>,
}

impl<'a> Hash for Type<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum TypeKind<'a> {
    User(DefId<'a>),
    Inst(DefId<'a>),
    Anonymous,
}

#[derive(PartialEq, Eq)]
pub enum TypeRepr<'a> {
    Unknown,
    Struct(StructType<'a>),
    Func(FuncType<'a>),
    Void,
    Opaque,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(&'a Type<'a>),
    ArrayPtr(&'a Type<'a>),
}

#[derive(PartialEq, Eq)]
pub struct StructType<'a> {
    pub body: OnceCell<StructBody<'a>>,
}

#[derive(PartialEq, Eq)]
pub struct StructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a Type<'a>>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct FuncType<'a> {
    pub params: &'a [&'a Type<'a>],
    pub return_type: &'a Type<'a>,
}

pub type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FloatType {
    F32,
    F64,
}
