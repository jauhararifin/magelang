mod analyze;
mod errors;
mod expr;
mod interner;
mod path;
mod scope;
mod statement;
mod ty;
mod value;

use interner::{Interned, Interner};
use std::fmt::Display;
use std::rc::Rc;

pub use analyze::{analyze, Annotation, FuncObject, GlobalObject, ValueObject};

pub(crate) type SymbolInterner<'a> = Interner<'a, str>;
pub type Symbol<'a> = Interned<'a, str>;

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

pub use expr::{Expr, ExprKind, InternExpr};
pub use statement::{IfStatement, InternStatement, Statement, WhileStatement};
pub use ty::{
    BitSize, FloatType, FuncType, InstType, IntSign, InternType, InternTypeArgs, StructType, Type,
    TypeArg,
};

pub struct Module<'a> {
    pub packages: Vec<Package<'a>>,
}

pub struct Package<'a> {
    pub name: Symbol<'a>,
    pub globals: Vec<Global<'a>>,
    pub functions: Vec<Func<'a>>,
}

pub struct Global<'a> {
    pub name: DefId<'a>,
    pub ty: InternType<'a>,
    pub value: InternExpr<'a>,
    pub annotations: Rc<[Annotation]>,
}

pub struct Func<'a> {
    pub name: DefId<'a>,
    pub typeargs: Option<InternTypeArgs<'a>>,
    pub ty: InternType<'a>,
    pub statement: InternStatement<'a>,
}
