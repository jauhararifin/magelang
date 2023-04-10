use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub struct SymbolId(usize);

pub struct SymbolLoader {
    symbol_to_id: RefCell<HashMap<Rc<str>, SymbolId>>,
    id_to_symbol: RefCell<HashMap<SymbolId, Rc<str>>>,
}

impl Default for SymbolLoader {
    fn default() -> Self {
        Self {
            symbol_to_id: RefCell::new(HashMap::new()),
            id_to_symbol: RefCell::new(HashMap::new()),
        }
    }
}

impl SymbolLoader {
    pub fn declare_symbol<T: AsRef<str>>(&self, symbol: T) -> SymbolId {
        let symbol: Rc<str> = symbol.as_ref().into();
        let mut symbol_to_id = self.symbol_to_id.borrow_mut();
        let mut id_to_symbol = self.id_to_symbol.borrow_mut();
        if let Some(id) = symbol_to_id.get(&symbol) {
            *id
        } else {
            let id = SymbolId(symbol_to_id.len());
            symbol_to_id.insert(symbol.clone(), id);
            id_to_symbol.insert(id, symbol);
            id
        }
    }

    pub fn get_symbol(&self, symbol_id: SymbolId) -> Option<Rc<str>> {
        self.id_to_symbol.borrow().get(&symbol_id).map(Rc::clone)
    }
}
