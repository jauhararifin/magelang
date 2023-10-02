use crate::Symbol;
use indexmap::IndexMap;
use std::rc::Rc;

pub(crate) struct Scope<'a, T> {
    internal: Rc<ScopeInternal<'a, T>>,
}

struct ScopeInternal<'a, T> {
    table: IndexMap<Symbol<'a>, T>,
    parent: Option<Rc<ScopeInternal<'a, T>>>,
}

impl<'a, T> Default for Scope<'a, T> {
    fn default() -> Self {
        Self {
            internal: Rc::new(ScopeInternal {
                table: IndexMap::default(),
                parent: None,
            }),
        }
    }
}

impl<'a, T> Clone for Scope<'a, T> {
    fn clone(&self) -> Self {
        Self {
            internal: self.internal.clone(),
        }
    }
}

impl<'a, T> Scope<'a, T> {
    pub(crate) fn new(table: IndexMap<Symbol<'a>, T>) -> Self {
        let internal = Rc::new(ScopeInternal {
            table,
            parent: None,
        });
        Self { internal }
    }

    pub(crate) fn new_child(&self, table: IndexMap<Symbol<'a>, T>) -> Self {
        let internal = Rc::new(ScopeInternal {
            table,
            parent: Some(self.internal.clone()),
        });
        Self { internal }
    }

    pub(crate) fn lookup(&self, name: Symbol<'a>) -> Option<&T> {
        let mut internal = Some(&self.internal);
        while let Some(s) = internal {
            if let Some(object) = s.table.get(&name) {
                return Some(object);
            } else {
                internal = s.parent.as_ref();
            }
        }

        None
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Symbol, &T)> {
        self.internal.table.iter().map(|(sym, item)| (*sym, item))
    }
}
