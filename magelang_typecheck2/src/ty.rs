//! The type model.
//!
//! A `Type` is always fully applied: `Vector<i32>` is a type, `Vector` alone is a struct
//! *definition* (see `def.rs`) and is not representable as a `Type`. Types are interned,
//! so equal types share one allocation and can be compared by pointer.

use crate::{DefId, Symbol};
use indexmap::IndexMap;
use std::cell::OnceCell;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};

pub type TypeArgs<'a> = [&'a Type<'a>];

pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub repr: TypeRepr<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeKind<'a> {
    /// A struct definition applied to its type arguments (empty for a non-generic struct).
    /// Named types are nominal: two named types are equal iff they have the same
    /// definition and the same type arguments.
    Named {
        def_id: DefId<'a>,
        type_args: &'a TypeArgs<'a>,
    },
    /// Every other type, including all function types. Anonymous types are structural.
    Anonymous,
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TypeKind::Named { .. }, TypeKind::Named { .. }) => self.kind == other.kind,
            (TypeKind::Anonymous, TypeKind::Anonymous) => self.repr == other.repr,
            _ => false,
        }
    }
}

impl<'a> Eq for Type<'a> {}

impl<'a> Hash for Type<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.kind {
            TypeKind::Named { .. } => self.kind.hash(state),
            TypeKind::Anonymous => self.repr.hash(state),
        }
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Named { def_id, type_args } => {
                Display::fmt(def_id, f)?;
                if !type_args.is_empty() {
                    write!(f, "::<")?;
                    for (i, ty) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ",")?;
                        }
                        Display::fmt(ty, f)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            TypeKind::Anonymous => Display::fmt(&self.repr, f),
        }
    }
}

impl<'a> Debug for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Named types print their name only: printing the body would recurse forever
        // on self-referential structs.
        match &self.kind {
            TypeKind::Named { .. } => Display::fmt(self, f),
            TypeKind::Anonymous => Debug::fmt(&self.repr, f),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeRepr<'a> {
    /// The type of an expression that already produced an error. It is compatible with
    /// everything, so one mistake doesn't cascade into a chain of diagnostics.
    Unknown,
    Void,
    Opaque,
    Bool,
    UntypedInt,
    Int(IntSign, BitSize),
    UntypedFloat,
    Float(FloatType),
    Ptr(&'a Type<'a>),
    ArrayPtr(&'a Type<'a>),
    Func(FuncType<'a>),
    /// Only ever appears under `TypeKind::Named`.
    Struct(StructType<'a>),
    /// A type parameter. Only exists while a generic definition is checked against its
    /// own parameters; it never reaches code generation.
    Param(TypeParam<'a>),
}

impl<'a> TypeRepr<'a> {
    pub fn as_func(&self) -> Option<&FuncType<'a>> {
        if let Self::Func(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        if let Self::Struct(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

impl<'a> Display for TypeRepr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeRepr::Unknown => write!(f, "{{unknown}}"),
            TypeRepr::Struct(ty) => {
                if let Some(body) = ty.body.get() {
                    write!(f, "struct{{")?;
                    for (name, ty) in body.fields.iter() {
                        write!(f, "{name}: {ty},")?;
                    }
                    write!(f, "}}")
                } else {
                    write!(f, "struct{{missing body}}")
                }
            }
            TypeRepr::Func(func_type) => write!(f, "{}", func_type),
            TypeRepr::Void => write!(f, "void"),
            TypeRepr::Opaque => write!(f, "opaque"),
            TypeRepr::Bool => write!(f, "bool"),
            TypeRepr::UntypedInt => write!(f, "untyped int"),
            TypeRepr::Int(sign, size) => {
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
            TypeRepr::UntypedFloat => write!(f, "untyped float"),
            TypeRepr::Float(FloatType::F32) => write!(f, "f32"),
            TypeRepr::Float(FloatType::F64) => write!(f, "f64"),
            TypeRepr::Ptr(ty) => write!(f, "*{}", ty),
            TypeRepr::ArrayPtr(ty) => write!(f, "[*]{}", ty),
            TypeRepr::Param(param) => write!(f, "{}", param.name),
        }
    }
}

#[derive(Debug)]
pub struct StructType<'a> {
    /// Set right after the instance is created (see `instance::struct_instance`). Only a
    /// by-value reference to the struct from inside its own body can observe it empty.
    pub body: OnceCell<StructBody<'a>>,
}

// Struct types only ever appear under `TypeKind::Named`, whose identity is the
// definition and the type arguments, so the body never takes part in equality/hashing.
impl<'a> PartialEq for StructType<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.body.get() == other.body.get()
    }
}

impl<'a> Eq for StructType<'a> {}

impl<'a> Hash for StructType<'a> {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a Type<'a>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType<'a> {
    pub params: &'a [&'a Type<'a>],
    pub return_type: &'a Type<'a>,
}

impl<'a> Display for FuncType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (i, ty) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            Display::fmt(ty, f)?;
        }
        write!(f, "):")?;
        Display::fmt(&self.return_type, f)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeParam<'a> {
    pub index: usize,
    pub name: Symbol<'a>,
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

impl<'a> Type<'a> {
    pub(crate) fn anonymous(repr: TypeRepr<'a>) -> Self {
        Self {
            kind: TypeKind::Anonymous,
            repr,
        }
    }

    pub fn as_func(&self) -> Option<&FuncType<'a>> {
        self.repr.as_func()
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        self.repr.as_struct()
    }

    pub(crate) fn def_id(&self) -> Option<DefId<'a>> {
        match self.kind {
            TypeKind::Named { def_id, .. } => Some(def_id),
            TypeKind::Anonymous => None,
        }
    }

    pub fn is_unknown(&self) -> bool {
        matches!(self.repr, TypeRepr::Unknown)
    }

    // The predicates below are true for `Unknown` as well, so that an expression that
    // already failed doesn't produce a second error at every use.

    pub fn is_usize(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Int(false, BitSize::ISize))
    }

    pub fn is_void(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Void)
    }

    pub fn is_integral(&self) -> bool {
        self.is_unknown()
            || matches!(
                self.repr,
                TypeRepr::Int(..) | TypeRepr::Ptr(..) | TypeRepr::ArrayPtr(..)
            )
    }

    pub fn is_float(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Float(..))
    }

    pub fn is_int(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Int(..))
    }

    pub fn is_f32(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Float(FloatType::F32))
    }

    pub fn is_f64(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Float(FloatType::F64))
    }

    pub fn is_byte_array(&self) -> bool {
        self.is_unknown()
            || if let TypeRepr::ArrayPtr(element_ty) = self.repr {
                matches!(element_ty.repr, TypeRepr::Int(false, BitSize::I8))
            } else {
                false
            }
    }

    pub fn is_bool(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Bool)
    }

    pub fn is_opaque(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Opaque)
    }

    pub fn is_strictly_opaque(&self) -> bool {
        matches!(self.repr, TypeRepr::Opaque)
    }

    pub(crate) fn is_arithmetic(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Int(..) | TypeRepr::Float(..))
    }

    /// Whether a value of type `other` can be stored in a slot of type `self`. Named types
    /// are nominal, anonymous types are structural, and `Unknown` accepts everything.
    pub(crate) fn is_assignable_with(&self, other: &Self) -> bool {
        self.is_unknown() || other.is_unknown() || self == other
    }

    /// Whether a type parameter occurs anywhere in this type. Such types only exist
    /// while a generic definition is checked and must never reach code generation.
    pub(crate) fn has_param(&self) -> bool {
        match &self.repr {
            TypeRepr::Param(..) => true,
            TypeRepr::Ptr(ty) | TypeRepr::ArrayPtr(ty) => ty.has_param(),
            TypeRepr::Func(func_type) => {
                func_type.params.iter().any(|ty| ty.has_param())
                    || func_type.return_type.has_param()
            }
            TypeRepr::Struct(..) => match self.kind {
                TypeKind::Named { type_args, .. } => type_args.iter().any(|ty| ty.has_param()),
                TypeKind::Anonymous => false,
            },
            _ => false,
        }
    }

    /// The nesting depth of type arguments, used to stop runaway instantiation such as
    /// `fn f<T>() { f::<Wrap<T>>(); }`.
    pub(crate) fn depth(&self) -> usize {
        match &self.repr {
            TypeRepr::Ptr(ty) | TypeRepr::ArrayPtr(ty) => ty.depth(),
            TypeRepr::Func(func_type) => func_type
                .params
                .iter()
                .map(|ty| ty.depth())
                .max()
                .unwrap_or(0)
                .max(func_type.return_type.depth()),
            TypeRepr::Struct(..) => match self.kind {
                TypeKind::Named { type_args, .. } => {
                    1 + type_args.iter().map(|ty| ty.depth()).max().unwrap_or(0)
                }
                TypeKind::Anonymous => 0,
            },
            _ => 0,
        }
    }
}
