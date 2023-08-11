use crate::ast::ItemNode;
use crate::def::DefId;
use crate::package::PackageId;
use crate::scope::Object;
use crate::stmt::StatementDb;
use crate::ty::StructType;
use indexmap::IndexSet;
use std::collections::HashSet;

pub fn check_main_package(db: &impl StatementDb, package_id: PackageId) {
    let all_packages = get_all_packages(db, package_id);
    for package_id in all_packages {
        db.get_package_scope(package_id);
        let path = db.get_package_path(package_id);
        let path_id = db.define_path(path);
        let ast = db.get_ast_by_path(path_id);
        let mut declared = HashSet::new();
        for item in &ast.root.items {
            let name = db.define_symbol(item.name().into());
            if !declared.contains(&name) {
                check_item(db, DefId::new(package_id, name), item);
            }
            declared.insert(name);
        }
    }

    check_main_func(db, package_id);
}

fn get_all_packages(db: &impl StatementDb, package_id: PackageId) -> Vec<PackageId> {
    let mut stack = Vec::default();
    let mut in_stack = HashSet::<PackageId>::default();
    let mut result = IndexSet::<PackageId>::default();

    stack.push(package_id);
    in_stack.insert(package_id);
    while let Some(current_id) = stack.pop() {
        if result.contains(&current_id) {
            continue;
        }
        result.insert(current_id);
        for dep in get_package_dependencies(db, current_id) {
            if !in_stack.contains(&dep) {
                stack.push(dep);
                in_stack.insert(dep);
            }
        }
    }

    result.into_iter().collect()
}

fn get_package_dependencies(db: &impl StatementDb, package_id: PackageId) -> Vec<PackageId> {
    let mut result = Vec::default();
    let scope = db.get_package_scope(package_id);
    for object in scope.symbols.values() {
        if let Object::Import(imported_package_id) = object {
            result.push(*imported_package_id);
        }
    }
    result
}

fn check_item(db: &impl StatementDb, def_id: DefId, node: &ItemNode) {
    match node {
        ItemNode::Import(..) => (),
        ItemNode::Struct(node) => {
            if node.type_params.is_empty() {
                check_struct_item(db, def_id)
            } else {
                check_struct_gen_item(db, def_id)
            }
        }
        ItemNode::Global(..) => check_global_item(db, def_id),
        ItemNode::Function(node) => {
            if node.signature.type_params.is_empty() {
                check_func_item(db, def_id)
            } else {
                check_func_gen_item(db, def_id)
            }
        }
        ItemNode::NativeFunction(node) => {
            if node.type_params.is_empty() {
                check_native_func_item(db, def_id)
            } else {
                check_native_func_gen_item(db, def_id)
            }
        }
    }
}

fn check_struct_item(db: &impl StatementDb, def_id: DefId) {
    let struct_type = StructType::Concrete(def_id.into());
    let struct_type_id = db.define_struct_type(struct_type);
    db.get_struct_field(struct_type_id);
}

fn check_struct_gen_item(db: &impl StatementDb, def_id: DefId) {
    db.get_generic_struct_field(def_id.into());
}

fn check_global_item(db: &impl StatementDb, def_id: DefId) {
    db.get_global_type_id(def_id.into());
    db.get_global_expr(def_id.into());
}

fn check_func_item(db: &impl StatementDb, def_id: DefId) {
    db.get_func_type_id(def_id.into());
    db.get_func_body(def_id.into());
}

fn check_func_gen_item(db: &impl StatementDb, def_id: DefId) {
    db.get_generic_func_type_id(def_id.into());
    db.get_generic_func_body(def_id.into());
}

fn check_native_func_item(db: &impl StatementDb, def_id: DefId) {
    db.get_func_type_id(def_id.into());
    db.get_func_body(def_id.into());
}

fn check_native_func_gen_item(db: &impl StatementDb, def_id: DefId) {
    db.get_generic_func_type_id(def_id.into());
    db.get_generic_func_body(def_id.into());
}

fn check_main_func(db: &impl StatementDb, package_id: PackageId) {
    db.get_main_func(package_id);
}
