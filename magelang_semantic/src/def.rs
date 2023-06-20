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

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GlobalId(DefId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct FuncId(DefId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GenStructId(DefId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct GenFuncId(DefId);

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
