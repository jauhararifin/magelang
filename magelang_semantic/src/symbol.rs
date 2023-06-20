use std::rc::Rc;

pub trait SymbolDb {
    fn define_symbol(&self, symbol: Rc<str>) -> SymbolId;
    fn get_symbol(&self, symbol_id: SymbolId) -> Rc<str>;
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct SymbolId(usize);

impl From<usize> for SymbolId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<SymbolId> for usize {
    fn from(value: SymbolId) -> Self {
        value.0
    }
}
