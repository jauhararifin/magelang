use crate::ast::{get_ast_by_path, AstDb, AstInfo, ItemNode, Loc, PathId};
use crate::builtin::get_builtin_scope;
use crate::def::{get_items_by_package, DefDb, FuncId, GenFuncId, GlobalId};
use crate::error::ErrorAccumulator;
use crate::expr::{get_global_expr, Expr, ExprDb};
use crate::native::{get_generic_native_func, get_generic_native_func_inst, get_native_func, NativeDb, NativeFunc};
use crate::package::{get_ast_by_package, get_package_path, get_stdlib_path, PackageDb, PackageId};
use crate::scope::{get_package_scope, Scope, ScopeDb};
use crate::stmt::{get_func_body, get_generic_func_body, get_generic_func_inst_body, FuncBody, StatementDb};
use crate::symbol::{SymbolDb, SymbolId};
use crate::ty::{
    get_func_type, get_generic_func_inst_type_id, get_generic_func_type, get_generic_struct_field, get_global_type,
    get_struct_field, FuncTypeId, StructField, StructTypeId, Type, TypeArgsId, TypeDb, TypeId,
};
use indexmap::IndexMap;
use std::cell::OnceCell;
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;
use std::rc::Rc;

#[derive(Default)]
pub struct Db {
    errors: RefCell<Vec<(Loc, String)>>,

    symbol_interner: Interner<Rc<str>>,

    stdlib_cache: OnceCell<Rc<Path>>,
    package_cache: Cache<PackageId, Rc<Path>>,
    ast_cache: Cache<PackageId, Rc<AstInfo>>,

    path_interner: Interner<Rc<Path>>,
    ast_by_path: Cache<PathId, Rc<AstInfo>>,

    package_items: Cache<PackageId, Rc<IndexMap<SymbolId, Rc<ItemNode>>>>,

    type_interner: Interner<Rc<Type>>,
    typeargs_interner: Interner<Rc<[TypeId]>>,
    global_type_cache: Cache<GlobalId, TypeId>,
    func_type_cache: Cache<FuncId, FuncTypeId>,
    struct_field_cache: Cache<StructTypeId, Rc<StructField>>,
    generic_func_type_cache: Cache<GenFuncId, FuncTypeId>,
    generic_func_inst_cache: Cache<(GenFuncId, TypeArgsId), FuncTypeId>,

    builtin_scope: OnceCell<Rc<Scope>>,
    package_scope_cache: Cache<PackageId, Rc<Scope>>,

    global_expr_cache: Cache<GlobalId, Rc<Expr>>,

    func_body_cache: Cache<FuncId, Rc<FuncBody>>,
    generic_func_body_cache: Cache<GenFuncId, Rc<FuncBody>>,
    generic_func_inst_body_cache: Cache<(GenFuncId, TypeArgsId), Rc<FuncBody>>,

    native_func_cache: Cache<FuncId, NativeFunc>,
    generic_native_func_cache: Cache<GenFuncId, NativeFunc>,
    generic_native_func_inst_cache: Cache<(GenFuncId, TypeArgsId), NativeFunc>,
}

impl ErrorAccumulator for Db {
    fn report_error(&self, location: Loc, message: String) {
        self.errors.borrow_mut().push((location, message));
    }
}

impl Db {
    pub fn take_errors(&self) -> Vec<(Loc, String)> {
        let errors = self.errors.take();
        errors
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

impl AstDb for Db {
    fn define_path(&self, path: Rc<Path>) -> PathId {
        self.path_interner.define(path).into()
    }

    fn get_path(&self, path_id: PathId) -> Rc<Path> {
        self.path_interner.get(path_id.into())
    }

    fn get_ast_by_path(&self, path_id: PathId) -> Rc<AstInfo> {
        self.ast_by_path.get_or_init(path_id, || get_ast_by_path(self, path_id))
    }
}

impl DefDb for Db {
    fn get_items_by_package(&self, package_id: PackageId) -> Rc<IndexMap<SymbolId, Rc<ItemNode>>> {
        self.package_items
            .get_or_init(package_id, || get_items_by_package(self, package_id))
    }
}

impl TypeDb for Db {
    fn define_type(&self, value: Rc<Type>) -> TypeId {
        self.type_interner.define(value).into()
    }

    fn get_type(&self, type_id: TypeId) -> Rc<Type> {
        self.type_interner.get(type_id.into())
    }

    fn define_typeargs(&self, value: Rc<[TypeId]>) -> TypeArgsId {
        self.typeargs_interner.define(value).into()
    }

    fn get_typeargs(&self, typeargs_id: TypeArgsId) -> Rc<[TypeId]> {
        self.typeargs_interner.get(typeargs_id.into())
    }

    fn get_global_type_id(&self, global_id: GlobalId) -> TypeId {
        self.global_type_cache
            .get_or_init(global_id, || get_global_type(self, global_id))
    }

    fn get_func_type_id(&self, func_id: FuncId) -> FuncTypeId {
        self.func_type_cache
            .get_or_init(func_id, || get_func_type(self, func_id))
    }

    fn get_struct_field(&self, struct_type_id: StructTypeId) -> Rc<StructField> {
        self.struct_field_cache
            .get_or_init(struct_type_id, || get_struct_field(self, struct_type_id))
    }

    fn get_generic_struct_field(&self, struct_id: crate::def::StructId) -> Rc<StructField> {
        get_generic_struct_field(self, struct_id)
    }

    fn get_generic_func_type_id(&self, gen_func_id: GenFuncId) -> FuncTypeId {
        self.generic_func_type_cache
            .get_or_init(gen_func_id, || get_generic_func_type(self, gen_func_id))
    }

    fn get_generic_func_inst_type_id(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> FuncTypeId {
        self.generic_func_inst_cache
            .get_or_init((gen_func_id, typeargs_id), || {
                get_generic_func_inst_type_id(self, gen_func_id, typeargs_id)
            })
    }
}

impl ScopeDb for Db {
    fn get_builtin_scope(&self) -> Rc<Scope> {
        self.builtin_scope.get_or_init(|| get_builtin_scope(self)).clone()
    }

    fn get_package_scope(&self, package_id: PackageId) -> Rc<Scope> {
        self.package_scope_cache
            .get_or_init(package_id, || get_package_scope(self, package_id))
    }
}

impl ExprDb for Db {
    fn get_global_expr(&self, global_id: GlobalId) -> Rc<Expr> {
        self.global_expr_cache
            .get_or_init(global_id, || get_global_expr(self, global_id))
    }
}

impl StatementDb for Db {
    fn get_func_body(&self, func_id: FuncId) -> Rc<FuncBody> {
        self.func_body_cache
            .get_or_init(func_id, || get_func_body(self, func_id))
    }

    fn get_generic_func_body(&self, gen_func_id: GenFuncId) -> Rc<FuncBody> {
        self.generic_func_body_cache
            .get_or_init(gen_func_id, || get_generic_func_body(self, gen_func_id))
    }

    fn get_generic_func_inst_body(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> Rc<FuncBody> {
        self.generic_func_inst_body_cache
            .get_or_init((gen_func_id, typeargs_id), || {
                get_generic_func_inst_body(self, gen_func_id, typeargs_id)
            })
    }
}

impl NativeDb for Db {
    fn get_native_func(&self, func_id: FuncId) -> NativeFunc {
        self.native_func_cache
            .get_or_init(func_id, || get_native_func(self, func_id))
    }

    fn get_generic_native_func(&self, gen_func_id: GenFuncId) -> NativeFunc {
        self.generic_native_func_cache
            .get_or_init(gen_func_id, || get_generic_native_func(self, gen_func_id))
    }

    fn get_generic_native_func_inst(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> NativeFunc {
        self.generic_native_func_inst_cache
            .get_or_init((gen_func_id, typeargs_id), || {
                get_generic_native_func_inst(self, gen_func_id, typeargs_id)
            })
    }
}

struct Interner<T> {
    id_to_item: RefCell<Vec<T>>,
    item_to_id: RefCell<HashMap<T, usize>>,
}

impl<T> Default for Interner<T> {
    fn default() -> Self {
        Self {
            id_to_item: RefCell::new(Vec::default()),
            item_to_id: RefCell::new(HashMap::default()),
        }
    }
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
