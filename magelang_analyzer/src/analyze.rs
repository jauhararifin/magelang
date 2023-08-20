use crate::errors::SemanticError;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::path::{get_package_path, get_stdlib_path};
use crate::symbols::{SymbolId, SymbolInterner};
use crate::ty::{TypeArgsId, TypeArgsInterner, TypeInterner};
use crate::value::value_from_string_lit;
use indexmap::IndexMap;
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode};
use std::collections::HashSet;
use std::path::Path;

pub fn analyze(
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) {
    let mut symbols = SymbolInterner::default();
    let mut types = TypeInterner::default();
    let mut typeargs = TypeArgsInterner::default();

    let stdlib_path = get_stdlib_path();
    let main_package = symbols.define(main_package);
    let package_asts = get_all_package_asts(
        file_manager,
        error_manager,
        &symbols,
        &stdlib_path,
        main_package,
    );
}

fn get_all_package_asts(
    files: &mut FileManager,
    errors: &impl ErrorReporter,
    symbols: &SymbolInterner,
    stdlib_path: &Path,
    main_package: SymbolId,
) -> IndexMap<SymbolId, PackageNode> {
    let mut stack = vec![main_package];
    let mut in_stack = HashSet::<SymbolId>::from([main_package]);
    let mut package_asts = IndexMap::<SymbolId, PackageNode>::default();
    while let Some(package_name) = stack.pop() {
        if package_asts.contains_key(&package_name) {
            continue;
        }

        let path = get_package_path(stdlib_path, &symbols.get(package_name));
        let file = match files.open(path.clone()) {
            Ok(file) => file,
            Err(err) => {
                let file = files.add_file(path.clone(), String::from(""));
                errors.cannot_open_file(file.offset.with_offset(0), &path, err);
                file
            }
        };
        let root = parse(errors, &file);
        let root = package_asts.entry(package_name).or_insert(root);

        let import_paths = root
            .items
            .iter()
            .filter_map(ItemNode::as_import)
            .filter_map(|node| value_from_string_lit(node.path.value.as_str()))
            .filter_map(|bytes| String::from_utf8(bytes).ok());
        for import_path in import_paths {
            let package_path = symbols.define(import_path.as_str());
            if !in_stack.contains(&package_path) {
                stack.push(package_path);
                in_stack.insert(package_path);
            }
        }
    }

    package_asts
}
