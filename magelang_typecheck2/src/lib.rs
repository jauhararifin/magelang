//! Type checking and semantic analysis for Magelang.
//!
//! `analyze` turns the main package and everything it imports into a `Module`: a typed IR
//! in which every generic definition has been replaced by its concrete instances. The
//! work is organised in phases (see `analyze.rs`):
//!
//! 1. load    – parse the main package and, transitively, its imports; circular imports
//! 2. declare – collect every top-level definition (`def.rs`); redeclarations
//! 3. signatures – global types, function signatures, and every struct's identity instance
//! 4. bodies  – global initializers; generic function bodies checked against their own
//!    type parameters
//! 5. instantiate – check the body of every concrete function instance, queueing the
//!    instances those bodies request
//! 6. validate – infinite-size structs, global initialization order
//! 7. build   – produce the `Module`
//!
//! Generics: a generic definition is not a type. A `Type` is always fully applied
//! (`Vector<i32>`). An instance is produced by resolving the definition's syntax again
//! with its type parameters bound to the arguments (`instance.rs`, `resolve.rs`), never by
//! substituting into previously checked output.

mod analyze;
mod cycle;
mod def;
mod errors;
mod expr;
mod func;
mod global_init;
mod instance;
mod interner;
mod loader;
mod path;
mod resolve;
mod scope;
mod statement;
mod ty;

use interner::Interner;
use magelang_syntax::Pos;
use std::fmt::Display;
use std::rc::Rc;

pub use analyze::analyze;
pub use def::Annotation;
pub use expr::{Expr, ExprKind, Float};
pub use statement::{IfStatement, Statement, WhileStatement};
pub use ty::{
    BitSize, FloatType, FuncType, IntSign, StructBody, StructType, Type, TypeArgs, TypeKind,
    TypeParam, TypeRepr,
};

pub(crate) type SymbolInterner<'a> = Interner<'a, str>;
pub type Symbol<'a> = &'a str;

/// The identity of a top-level definition: its package and its name.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DefId<'a> {
    pub package: Symbol<'a>,
    pub name: Symbol<'a>,
}

impl<'a> Display for DefId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.package, self.name)
    }
}

#[derive(Debug)]
pub struct Module<'a> {
    /// False if any error was reported; the module must not be compiled then.
    pub is_valid: bool,
    pub packages: Vec<Package<'a>>,
    pub global_init_order: Vec<DefId<'a>>,
}

#[derive(Debug)]
pub struct Package<'a> {
    pub name: Symbol<'a>,
    pub globals: Vec<Global<'a>>,
    pub functions: Vec<Func<'a>>,
}

#[derive(Debug)]
pub struct Global<'a> {
    pub name: DefId<'a>,
    pub ty: &'a Type<'a>,
    pub value: Expr<'a>,
    pub annotations: Rc<[Annotation]>,
}

/// A concrete function: a non-generic function (`typeargs == None`) or one instance of a
/// generic function.
#[derive(Debug)]
pub struct Func<'a> {
    pub name: DefId<'a>,
    pub pos: Pos,
    pub typeargs: Option<&'a TypeArgs<'a>>,
    pub ty: &'a Type<'a>,
    pub statement: &'a Statement<'a>,
    pub annotations: Rc<[Annotation]>,
}
