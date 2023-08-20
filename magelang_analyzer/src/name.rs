pub use crate::symbols::SymbolId;
pub use crate::ty::TypeArgsId;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct DefId {
    pub package: SymbolId,
    pub name: SymbolId,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Name {
    Def(DefId),
    Instance(DefId, TypeArgsId),
}
