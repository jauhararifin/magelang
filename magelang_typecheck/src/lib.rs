mod analyze;
mod errors;
mod expr;
mod interner;
mod path;
mod scope;
mod ty;
mod value;

use interner::{Interned, Interner};
use std::fmt::Display;

pub use analyze::analyze;

pub(crate) type SymbolInterner<'a> = Interner<'a, str>;
pub(crate) type Symbol<'a> = Interned<'a, str>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub(crate) struct DefId<'a> {
    pub package: Symbol<'a>,
    pub name: Symbol<'a>,
}

impl<'a> Display for DefId<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.package, self.name)
    }
}
