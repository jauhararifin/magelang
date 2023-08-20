use crate::interner::{Id, Interner};

pub type SymbolInterner = Interner<str>;
pub type SymbolId = Id<str>;
