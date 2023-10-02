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
    pub(crate) fn define<Q>(&self, item: Q) -> Interned<'a, T>
    where
        Q: std::borrow::Borrow<T>,
        T: From<Q>,
    {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item.borrow()) {
            Interned(item)
        } else {
            let item = internal.bump.alloc(item.into());
            internal.items.insert(item);
            Interned(item)
        }
    }
}

impl<'a> Interner<'a, str> {
    pub(crate) fn define(&self, item: &str) -> Interned<'a, str> {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item) {
            Interned(item)
        } else {
            let item = internal.bump.alloc_str(item);
            internal.items.insert(item);
            Interned(item)
        }
    }
}

impl<'a, T> Interner<'a, [T]>
where
    T: Hash + Eq + Clone,
{
    pub(crate) fn define(&self, item: &[T]) -> Interned<'a, [T]> {
        let mut internal = self.internal.borrow_mut();
        if let Some(item) = internal.items.get(item) {
            Interned(item)
        } else {
            let item = internal.bump.alloc_slice_clone(item);
            internal.items.insert(item);
            Interned(item)
        }
    }
}

pub struct Interned<'a, T: ?Sized>(&'a T);

impl<'a, T: ?Sized> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<'a, T: ?Sized> PartialEq for Interned<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0 as *const T;
        let b = other.0 as *const T;
        a == b
    }
}

impl<'a, T: ?Sized> Eq for Interned<'a, T> {}

impl<'a, T: ?Sized> Hash for Interned<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, T: ?Sized> Copy for Interned<'a, T> {}

impl<'a, T: ?Sized> std::fmt::Debug for Interned<'a, T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, T: ?Sized> std::fmt::Display for Interned<'a, T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, T: ?Sized> std::ops::Deref for Interned<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T: ?Sized> std::convert::AsRef<T> for Interned<'a, T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}
