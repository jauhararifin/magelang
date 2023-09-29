use crate::interner::{Interned, Interner};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use std::cell::OnceCell;
use std::fmt::Display;
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;
pub(crate) type InternType<'a> = Interned<'a, Type<'a>>;

pub(crate) type TypeArgsInterner<'a> = Interner<'a, [InternType<'a>]>;
pub(crate) type InternTypeArgs<'a> = Interned<'a, [InternType<'a>]>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) enum Type<'a> {
    Unknown,
    Struct(StructType<'a>),
    Inst(InstType<'a>),
    Func(FuncType<'a>),
    Void,
    Opaque,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(InternType<'a>),
    ArrayPtr(InternType<'a>),
    TypeArg(TypeArg<'a>),
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "{{unknown}}"),
            Type::Struct(ty) => ty.def_id.fmt(f),
            Type::Inst(ty) => ty.fmt(f),
            Type::Func(func_type) => write!(f, "{}", func_type),
            Type::Void => write!(f, "void"),
            Type::Opaque => write!(f, "opaque"),
            Type::Bool => write!(f, "bool"),
            Type::Int(sign, size) => {
                write!(
                    f,
                    "{}{}",
                    if *sign { "i" } else { "u" },
                    match size {
                        BitSize::I8 => "8",
                        BitSize::I16 => "16",
                        BitSize::I32 => "32",
                        BitSize::I64 => "64",
                        BitSize::ISize => "size",
                    }
                )
            }
            Type::Float(float_ty) => match float_ty {
                FloatType::F32 => write!(f, "f32"),
                FloatType::F64 => write!(f, "f64"),
            },
            Type::Ptr(ty) => {
                write!(f, "*{}", ty)
            }
            Type::ArrayPtr(ty) => {
                write!(f, "[*]{}", ty)
            }
            Type::TypeArg(arg) => {
                write!(f, "{}", arg.name)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct StructType<'a> {
    pub def_id: DefId<'a>,
    pub body: OnceCell<StructBody<'a>>,
    pub sized: bool,
}

impl<'a> Hash for StructType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct StructBody<'a> {
    fields: IndexMap<Symbol<'a>, InternType<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct InstType<'a> {
    pub def_id: DefId<'a>,
    pub type_args: InternTypeArgs<'a>,
    pub body: OnceCell<InternType<'a>>,
}

impl<'a> Hash for InstType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
        self.type_args.hash(state);
    }
}

impl<'a> Display for InstType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.def_id.fmt(f)?;
        write!(f, "::<")?;
        for ty in self.type_args.iter() {
            ty.fmt(f)?;
            write!(f, ",")?;
        }
        write!(f, ">")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct FuncType<'a> {
    pub params: Vec<InternType<'a>>,
    pub return_type: InternType<'a>,
}

impl<'a> Display for FuncType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for ty in &self.params {
            ty.fmt(f)?;
            write!(f, ",")?;
        }
        write!(f, "):")?;
        self.return_type.fmt(f)
    }
}

type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum FloatType {
    F32,
    F64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct TypeArg<'a> {
    index: usize,
    name: Symbol<'a>,
}
