use crate::error::{ErrorAccumulator, Loc, Location};
use crate::package::{PackageDb, PackageId};
use crate::symbol::{SymbolDb, SymbolId};
use indexmap::IndexMap;
use magelang_syntax::{AstNode, ItemNode};
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct DefId {
    pub package: PackageId,
    pub name: SymbolId,
}

impl DefId {
    pub fn new(package: PackageId, name: SymbolId) -> Self {
        Self { package, name }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GlobalId(DefId);

impl GlobalId {
    pub fn package(&self) -> PackageId {
        self.0.package
    }

    pub fn name(&self) -> SymbolId {
        self.0.name
    }
}

impl From<GlobalId> for DefId {
    fn from(value: GlobalId) -> Self {
        value.0
    }
}

impl From<DefId> for GlobalId {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct StructId(DefId);

impl From<DefId> for StructId {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct FuncId(DefId);

impl FuncId {
    pub fn package(&self) -> PackageId {
        self.0.package
    }

    pub fn name(&self) -> SymbolId {
        self.0.name
    }
}

impl From<FuncId> for DefId {
    fn from(value: FuncId) -> Self {
        value.0
    }
}

impl From<DefId> for FuncId {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GenStructId(DefId);

impl From<DefId> for GenStructId {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GenFuncId(DefId);

impl From<DefId> for GenFuncId {
    fn from(value: DefId) -> Self {
        Self(value)
    }
}

pub trait DefDb: PackageDb + ErrorAccumulator + SymbolDb {
    fn get_ast_by_def_id(&self, def_id: DefId) -> Option<Rc<ItemNode>> {
        let items = self.get_items_by_package(def_id.package);
        items.get(&def_id.name).cloned()
    }

    fn get_items_by_package(&self, package_id: PackageId) -> Rc<IndexMap<SymbolId, Rc<ItemNode>>>;
}

pub fn get_items_by_package(db: &impl DefDb, package_id: PackageId) -> Rc<IndexMap<SymbolId, Rc<ItemNode>>> {
    let ast_info = db.get_package_ast(package_id);

    let mut name_to_pos: IndexMap<SymbolId, Location> = IndexMap::default();
    let mut name_to_ast: IndexMap<SymbolId, Rc<ItemNode>> = IndexMap::default();
    for item in &ast_info.root.items {
        let name = db.define_symbol(item.name().into());
        let loc = Loc::new(ast_info.path, item.get_pos());
        if let Some(declared_at) = name_to_pos.get(&name) {
            db.redeclared_symbol(item.name(), declared_at, loc);
        } else {
            name_to_ast.insert(name, item.clone());
            let location = db.get_location(&ast_info, item.get_pos());
            name_to_pos.insert(name, location);
        }
    }

    name_to_ast.into()
}
