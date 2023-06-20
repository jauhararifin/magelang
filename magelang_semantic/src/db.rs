use crate::def::{get_items_by_package, DefDb};
use crate::error::{ErrorAccumulator, Loc};
use crate::package::{get_ast_by_package, get_package_path, get_stdlib_path, AstInfo, PackageDb, PackageId, PathId};
use crate::symbol::{SymbolDb, SymbolId};
use indexmap::IndexMap;
use magelang_syntax::ItemNode;
use std::cell::OnceCell;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;
use std::rc::Rc;

pub struct Db {
    errors: RefCell<Vec<(Loc, String)>>,

    symbol_interner: Interner<Rc<str>>,

    path_interner: Interner<Rc<Path>>,
    stdlib_cache: OnceCell<Rc<Path>>,
    package_cache: Cache<PackageId, Rc<Path>>,
    ast_cache: Cache<PackageId, Rc<AstInfo>>,

    package_items: Cache<PackageId, Rc<IndexMap<SymbolId, Rc<ItemNode>>>>,
}

impl ErrorAccumulator for Db {
    fn report_error(&self, location: Loc, message: String) {
        self.errors.borrow_mut().push((location, message));
    }
}

impl SymbolDb for Db {
    fn define_symbol(&self, symbol: Rc<str>) -> SymbolId {
        self.symbol_interner.define(symbol).into()
    }

    fn get_symbol(&self, symbol_id: SymbolId) -> Rc<str> {
        self.symbol_interner.get(symbol_id.into())
    }
}

impl PackageDb for Db {
    fn define_path(&self, path: Rc<Path>) -> PathId {
        self.path_interner.define(path).into()
    }

    fn get_path(&self, path_id: PathId) -> Rc<Path> {
        self.path_interner.get(path_id.into())
    }

    fn get_stdlib_path(&self) -> Rc<Path> {
        self.stdlib_cache.get_or_init(get_stdlib_path).clone()
    }

    fn get_package_path(&self, package_id: PackageId) -> Rc<Path> {
        self.package_cache
            .get_or_init(package_id, || get_package_path(self, package_id))
    }

    fn get_package_ast(&self, package_id: PackageId) -> Rc<AstInfo> {
        self.ast_cache
            .get_or_init(package_id, || get_ast_by_package(self, package_id))
    }
}

impl DefDb for Db {
    fn get_items_by_package(&self, package_id: PackageId) -> Rc<IndexMap<SymbolId, Rc<ItemNode>>> {
        self.package_items
            .get_or_init(package_id, || get_items_by_package(self, package_id))
    }
}

struct Interner<T> {
    id_to_item: RefCell<Vec<T>>,
    item_to_id: RefCell<HashMap<T, usize>>,
}

impl<T> Interner<T>
where
    T: Clone + Hash + Eq,
{
    pub fn define(&self, item: T) -> usize {
        let mut id_to_item = self.id_to_item.borrow_mut();
        let mut item_to_id = self.item_to_id.borrow_mut();

        let entry = item_to_id.entry(item.clone());
        if let Entry::Vacant(entry) = entry {
            let id = id_to_item.len();
            id_to_item.push(item);
            entry.insert(id);
            id
        } else {
            let id = item_to_id.get(&item).unwrap();
            *id
        }
    }

    pub fn get(&self, id: usize) -> T {
        self.id_to_item
            .borrow()
            .get(id)
            .expect("the provided id is not allocated yet")
            .clone()
    }
}

struct Cache<K, V> {
    cache: RefCell<HashMap<K, V>>,
}

impl<K, V> Default for Cache<K, V> {
    fn default() -> Self {
        Self {
            cache: RefCell::default(),
        }
    }
}

impl<K, V> Cache<K, V>
where
    K: Hash + Eq,
    V: Clone,
{
    pub fn get_or_init(&self, key: K, init: impl FnOnce() -> V) -> V {
        {
            let cache = self.cache.borrow_mut();
            if cache.contains_key(&key) {
                let value = cache.get(&key).unwrap().clone();
                return value;
            }
        }

        let value = init();
        self.cache.borrow_mut().insert(key, value.clone());
        value
    }
}
