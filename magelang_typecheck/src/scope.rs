use crate::Symbol;
use crate::analyze::Object;
use indexmap::IndexMap;
use std::rc::Rc;

pub(crate) struct Scope<'a> {
    internal: Rc<ScopeInternal<'a>>,
}

struct ScopeInternal<'a> {
    table: IndexMap<Symbol<'a>, Object<'a>>,
    parent: Option<Rc<ScopeInternal<'a>>>,
}

impl<'a> Default for Scope<'a> {
    fn default() -> Self {
        Self { internal: Rc::new(ScopeInternal { table: IndexMap::default(), parent: None }) }
    }
}

impl<'a> Clone for Scope<'a> {
    fn clone(&self) -> Self {
        Self { internal: self.internal.clone() }
    }
}

impl<'a> Scope<'a> {
    pub(crate) fn new(table: IndexMap<Symbol<'a>, Object<'a>>) -> Self {
        let internal = Rc::new(ScopeInternal { table, parent: None });
        Self { internal }
    }

    pub(crate) fn new_child(&self, table: IndexMap<Symbol<'a>, Object<'a>>) -> Self {
        let internal = Rc::new(ScopeInternal { table, parent: Some(self.internal.clone()) });
        Self { internal }
    }

    pub(crate) fn lookup(&self, name: Symbol<'_>) -> Option<&Object<'a>> {
        let mut internal = Some(&self.internal);
        while let Some(s) = internal {
            if let Some(object) = s.table.get(name) {
                return Some(object);
            }
            internal = s.parent.as_ref();
        }

        None
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Symbol<'_>, &Object<'a>)> {
        self.internal.table.iter().map(|(sym, item)| (*sym, item))
    }

    pub(crate) fn into_iter(self) -> impl Iterator<Item = (Symbol<'a>, Object<'a>)> {
        Rc::into_inner(self.internal).unwrap().table.into_iter()
    }
}
