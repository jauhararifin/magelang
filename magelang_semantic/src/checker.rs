use crate::ast::ItemNode;
use crate::ast::Loc;
use crate::def::DefId;
use crate::package::PackageId;
use crate::scope::Object;
use crate::stmt::StatementDb;
use indexmap::IndexSet;
use magelang_syntax::Pos;
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
    check_struct_gen_item(db, def_id);
}

fn check_struct_gen_item(db: &impl StatementDb, def_id: DefId) {
    let scope = db.get_package_scope(def_id.package);
    let object = scope.get(def_id.name).unwrap();
    let type_id = object.as_type().unwrap();
    db.get_struct_field(type_id.into());
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
    let path = db.get_package_path(package_id);
    let path_id = db.define_path(path);
    let pos = Loc::new(path_id, Pos::new(1));

    let scope = db.get_package_scope(package_id);
    let main_symbol = db.define_symbol("main".into());
    let Some(main_object) = scope.get(main_symbol) else {
        db.missing_main(pos);
        return;
    };
    let Object::Func{func_id: main_func_id, is_native} = main_object else {
        db.missing_main(pos);
        return;
    };
    if is_native {
        db.invalid_main_signature(pos);
        return;
    }
    let type_id = db.get_func_type_id(main_func_id);
    let func_ty = db.get_func_type(type_id);
    if !func_ty.params.is_empty() || func_ty.return_type != db.define_void_type() {
        db.invalid_main_signature(pos);
        return;
    }
}
