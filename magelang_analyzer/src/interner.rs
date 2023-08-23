use indexmap::IndexSet;
use std::cell::RefCell;
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(PartialEq, Eq, Hash)]
pub struct Id<Item: ?Sized>(pub usize, PhantomData<fn() -> Item>);

impl<Item: ?Sized> From<Id<Item>> for usize {
    fn from(value: Id<Item>) -> Self {
        value.0
    }
}

impl<Item: ?Sized> std::fmt::Debug for Id<Item> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Id<{}>{:?}", std::any::type_name::<Self>(), self.0)
    }
}

impl<Item: ?Sized> Default for Id<Item> {
    fn default() -> Self {
        Self(0, PhantomData)
    }
}

impl<Item: ?Sized> Clone for Id<Item> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Item: ?Sized> Copy for Id<Item> {}

pub struct Interner<Item: ?Sized> {
    internal: RefCell<InternerInternal<Item>>,
}

impl<Item: ?Sized> Default for Interner<Item> {
    fn default() -> Self {
        Self {
            internal: RefCell::default(),
        }
    }
}

impl<Item: ?Sized> Interner<Item> {
    pub fn get(&self, id: Id<Item>) -> Rc<Item> {
        self.internal.borrow().get(id)
    }

    pub fn take(&self) -> Vec<Rc<Item>> {
        self.internal.borrow_mut().take()
    }
}

pub trait SizedInterner<Item> {
    fn define(&self, item: Item) -> Id<Item>;
}

pub trait UnsizedInterner<'a, Item: ?Sized> {
    fn define(&self, item: &'a Item) -> Id<Item>;
}

impl<Item> SizedInterner<Item> for Interner<Item>
where
    Item: Eq + Hash,
{
    fn define(&self, item: Item) -> Id<Item> {
        self.internal.borrow_mut().define(item)
    }
}

impl<'a, Item> UnsizedInterner<'a, Item> for Interner<Item>
where
    Item: Eq + Hash + ?Sized + 'a,
    Rc<Item>: From<&'a Item>,
{
    fn define(&self, item: &'a Item) -> Id<Item> {
        self.internal.borrow_mut().define(item)
    }
}

struct InternerInternal<Item: ?Sized> {
    items: IndexSet<Rc<Item>>,
}

impl<Item: ?Sized> Default for InternerInternal<Item> {
    fn default() -> Self {
        Self {
            items: IndexSet::default(),
        }
    }
}

impl<Item: ?Sized> InternerInternal<Item> {
    fn get(&self, id: Id<Item>) -> Rc<Item> {
        self.items.get_index(id.0).unwrap().clone()
    }

    fn take(&mut self) -> Vec<Rc<Item>> {
        self.items.drain(..).collect()
    }
}

trait SizedInternerInternal<Item> {
    fn define(&mut self, item: Item) -> Id<Item>;
}

trait UnsizedInternerInternal<'a, Item: ?Sized> {
    fn define(&mut self, item: &'a Item) -> Id<Item>;
}

impl<Item> SizedInternerInternal<Item> for InternerInternal<Item>
where
    Item: Eq + Hash,
{
    fn define(&mut self, item: Item) -> Id<Item> {
        if let Some(id) = self.items.get_index_of(&item) {
            Id(id, PhantomData)
        } else {
            let id = Id(self.items.len(), PhantomData);
            let item: Rc<Item> = item.into();
            self.items.insert(item);
            id
        }
    }
}

impl<'a, Item> UnsizedInternerInternal<'a, Item> for InternerInternal<Item>
where
    Item: Eq + Hash + ?Sized + 'a,
    Rc<Item>: From<&'a Item>,
{
    fn define(&mut self, item: &'a Item) -> Id<Item> {
        if let Some(id) = self.items.get_index_of(item) {
            Id(id, PhantomData)
        } else {
            let id = Id(self.items.len(), PhantomData);
            let item = Rc::<Item>::from(item);
            self.items.insert(item.clone());
            id
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_string_interner() {
        let interner = Interner::<String>::default();

        let id_a = interner.define("StringA".to_string());
        assert_eq!(Id(0, PhantomData), id_a);

        let id_b = interner.define("StringB".to_string());
        assert_eq!(Id(1, PhantomData), id_b);

        let id_c = interner.define("StringC".to_string());
        assert_eq!(Id(2, PhantomData), id_c);

        let id_b_2 = interner.define("StringB".to_string());
        assert_eq!(Id(1, PhantomData), id_b_2);

        let s = interner.get(id_a);
        assert_eq!("StringA", s.as_str());

        let s = interner.get(id_b);
        assert_eq!("StringB", s.as_str());

        let s = interner.get(id_c);
        assert_eq!("StringC", s.as_str());

        let s = interner.get(id_b_2);
        assert_eq!("StringB", s.as_str());

        let all = interner.take();
        assert_eq!(
            vec!["StringA", "StringB", "StringC"],
            all.iter().map(|v| v.as_str()).collect::<Vec<_>>(),
        );
    }

    #[test]
    fn test_unsized_interner() {
        let interner = Interner::<[i32]>::default();

        let id_a = interner.define(&[0, 1, 2]);
        assert_eq!(Id(0, PhantomData), id_a);

        let id_b = interner.define(&[3, 4, 5]);
        assert_eq!(Id(1, PhantomData), id_b);

        let id_c = interner.define(&[6, 7, 8]);
        assert_eq!(Id(2, PhantomData), id_c);

        let id_b_2 = interner.define(&[3, 4, 5]);
        assert_eq!(Id(1, PhantomData), id_b_2);

        let s = interner.get(id_a);
        assert_eq!(&[0, 1, 2], s.as_ref());

        let s = interner.get(id_b);
        assert_eq!(&[3, 4, 5], s.as_ref());

        let s = interner.get(id_c);
        assert_eq!(&[6, 7, 8], s.as_ref());

        let s = interner.get(id_b_2);
        assert_eq!(&[3, 4, 5], s.as_ref());

        let mut all = interner.take();
        assert_eq!(&[6, 7, 8], all.pop().unwrap().as_ref());
        assert_eq!(&[3, 4, 5], all.pop().unwrap().as_ref());
        assert_eq!(&[0, 1, 2], all.pop().unwrap().as_ref());
    }
}
