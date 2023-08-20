use crate::interner::{Id, Interner};
use crate::name::DefId;
use crate::symbols::SymbolId;
use indexmap::IndexMap;
use magelang_syntax::StructNode;
use std::cell::OnceCell;
use std::hash::Hash;

pub type TypeInterner = Interner<Type>;
pub type TypeId = Id<Type>;

pub type TypeArgsInterner = Interner<[TypeId]>;
pub type TypeArgsId = Id<[TypeId]>;

pub type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Unknown,
    NamedStruct(NamedStructType),
    NamedStructInst(NamedStructInstType),
    Func(FuncType),
    Void,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(TypeId),
    ArrayPtr(TypeId),
    TypeArg(TypeArg),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedStructType {
    pub def_id: DefId,
    pub node: StructNode,
    pub body: OnceCell<StructBody>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructBody {
    pub fields: IndexMap<SymbolId, TypeId>,
}

impl Hash for StructBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.fields {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl Hash for NamedStructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedStructInstType {
    pub def_id: DefId,
    pub type_args: TypeArgsId,
    pub body: StructBody,
}

impl Hash for NamedStructInstType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
        self.type_args.hash(state);
    }
}

#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeArg {
    pub index: usize,
    pub symbol: SymbolId,
}
