mod analyze;
mod errors;
mod expr;
mod global_init;
mod interner;
mod path;
mod scope;
mod statement;
mod ty;

use interner::Interner;
use magelang_syntax::Pos;
use std::fmt::Display;
use std::rc::Rc;

pub use analyze::{analyze, Annotation, FuncObject, GlobalObject, ValueObject};

pub(crate) type SymbolInterner<'a> = Interner<'a, str>;
pub type Symbol<'a> = &'a str;

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

pub use expr::{Expr, ExprKind};
pub use statement::{IfStatement, Statement, WhileStatement};
pub use ty::{
    BitSize, FloatType, FuncType, InstType, IntSign, StructBody, StructType, Type, TypeArg,
    TypeArgs, TypeKind, TypeRepr,
};

#[derive(Debug)]
pub struct Module<'a> {
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

#[derive(Debug)]
pub struct Func<'a> {
    pub name: DefId<'a>,
    pub pos: Pos,
    pub typeargs: Option<&'a TypeArgs<'a>>,
    pub ty: &'a Type<'a>,
    pub statement: &'a Statement<'a>,
    pub annotations: Rc<[Annotation]>,
}
