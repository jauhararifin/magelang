//! Phase 1: load the main package and everything it imports, and reject circular imports.

use crate::errors::SemanticError;
use crate::path::get_package_path;
use crate::{Symbol, SymbolInterner};
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode, Pos};
use std::collections::HashSet;
use std::path::Path;

/// Parses `main_package` and, transitively, every package it imports. The result is in
/// load order (main package first); that order is also the order of the packages in the
/// produced `Module`.
pub(crate) fn load_packages<'a>(
    files: &mut FileManager,
    errors: &impl ErrorReporter,
    symbols: &SymbolInterner<'a>,
    stdlib_path: &Path,
    main_package: Symbol<'a>,
) -> IndexMap<Symbol<'a>, PackageNode> {
    let mut stack = vec![main_package];
    let mut in_stack = HashSet::<Symbol>::from([main_package]);
    let mut package_asts = IndexMap::<Symbol, PackageNode>::default();
    while let Some(package_name) = stack.pop() {
        if package_asts.contains_key(&package_name) {
            continue;
        }

        let path = get_package_path(stdlib_path, package_name);
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
            .filter_map(|node| std::str::from_utf8(&node.path.value).ok());

        for import_path in import_paths {
            let package_path = symbols.define(import_path);
            if !in_stack.contains(&package_path) {
                stack.push(package_path);
                in_stack.insert(package_path);
            }
        }
    }

    package_asts
}

pub(crate) fn check_circular_imports<'a, E: ErrorReporter>(
    errors: &E,
    symbols: &SymbolInterner<'a>,
    package_asts: &IndexMap<Symbol<'a>, PackageNode>,
) {
    let mut graph = IndexMap::<Symbol<'a>, Vec<(Symbol<'a>, Pos)>>::default();
    for (package_name, package_ast) in package_asts.iter() {
        let imports = package_ast
            .items
            .iter()
            .filter_map(ItemNode::as_import)
            .filter_map(|node| {
                let package = std::str::from_utf8(&node.path.value).ok()?;
                Some((symbols.define(package), node.path.pos))
            })
            .collect::<Vec<_>>();
        graph.insert(*package_name, imports);
    }

    let mut visited = IndexSet::<Symbol>::default();
    let mut in_chain = IndexSet::<Symbol>::default();
    for package_name in graph.keys() {
        if visited.contains(package_name) {
            continue;
        }
        visit_import(errors, &graph, package_name, &mut visited, &mut in_chain);
    }
}

fn visit_import<'a, E: ErrorReporter>(
    errors: &E,
    graph: &IndexMap<Symbol<'a>, Vec<(Symbol<'a>, Pos)>>,
    package_name: Symbol<'a>,
    visited: &mut IndexSet<Symbol<'a>>,
    in_chain: &mut IndexSet<Symbol<'a>>,
) {
    visited.insert(package_name);
    in_chain.insert(package_name);

    let Some(imports) = graph.get(package_name) else {
        in_chain.shift_remove(package_name);
        return;
    };

    for (imported_package, pos) in imports {
        if !graph.contains_key(imported_package) {
            continue;
        }

        if in_chain.contains(imported_package) {
            report_circular_import(errors, in_chain, imported_package, *pos);
            continue;
        }

        if !visited.contains(imported_package) {
            visit_import(errors, graph, imported_package, visited, in_chain);
        }
    }

    in_chain.shift_remove(package_name);
}

fn report_circular_import<E: ErrorReporter>(
    errors: &E,
    in_chain: &IndexSet<Symbol>,
    start: Symbol,
    pos: Pos,
) {
    let chain = in_chain
        .iter()
        .skip_while(|name| **name != start)
        .map(|name| name.to_string())
        .collect::<Vec<_>>();
    errors.circular_import(pos, &chain);
}
