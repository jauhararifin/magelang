use bumpalo::Bump;
use indexmap::IndexSet;
use std::cell::RefCell;
use std::hash::Hash;

pub(crate) struct Interner<'a, T: ?Sized> {
    internal: RefCell<InternerInternal<'a, T>>,
}

struct InternerInternal<'a, T: ?Sized> {
    bump: &'a Bump,
    items: IndexSet<&'a T>,
}

impl<'a, T: ?Sized> Interner<'a, T> {
    pub(crate) fn new(bump: &'a Bump) -> Self {
        Self {
            internal: RefCell::new(InternerInternal {
                bump,
                items: IndexSet::default(),
            }),
        }
    }
}

impl<'a, T: Hash + Eq> Interner<'a, T> {
    pub(crate) fn define<Q>(&self, item: Q) -> &'a T
    where
        Q: std::borrow::Borrow<T>,
        T: From<Q>,
    {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item.borrow()) {
            item
        } else {
            let item = internal.bump.alloc(item.into());
            internal.items.insert(item);
            item
        }
    }
}

impl<'a> Interner<'a, str> {
    pub(crate) fn define(&self, item: &str) -> &'a str {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item) {
            item
        } else {
            let item = internal.bump.alloc_str(item);
            internal.items.insert(item);
            item
        }
    }
}

impl<'a, T> Interner<'a, [T]>
where
    T: Hash + Eq + Clone,
{
    pub(crate) fn define(&self, item: &[T]) -> &'a [T] {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item) {
            item
        } else {
            let item = internal.bump.alloc_slice_clone(item);
            internal.items.insert(item);
            item
        }
    }
}
