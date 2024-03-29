use crate::errors::SemanticError;
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::{DefId, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode, Pos};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::path::Path;

pub fn analyze<'a>(
    arena: &'a Bump,
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) {
    let symbols = SymbolInterner::new(arena);
    let stdlib_path = get_stdlib_path();

    let main_package = symbols.define(main_package);
    let package_asts = get_all_package_asts(
        file_manager,
        error_manager,
        &symbols,
        &stdlib_path,
        main_package,
    );

    let mut import_items = IndexMap::<Symbol, Vec<ItemNode>>::default();
    let mut struct_items = IndexMap::<Symbol, Vec<ItemNode>>::default();
    let mut value_items = IndexMap::<Symbol, Vec<ItemNode>>::default();
    for (package_name, package_ast) in package_asts {
        let imports = import_items.entry(package_name).or_default();
        let structs = struct_items.entry(package_name).or_default();
        let values = value_items.entry(package_name).or_default();
        for item in package_ast.items {
            match item {
                ItemNode::Import(..) => imports.push(item),
                ItemNode::Struct(..) => structs.push(item),
                ItemNode::Function(..) | ItemNode::Global(..) => values.push(item),
            }
        }
    }

    let mut ctx = Context {
        arena,
        files: file_manager,
        errors: error_manager,
        symbols,
        package_scopes: IndexMap::default(),
    };

    let import_scopes = build_imports(&ctx, import_items);
    for (package, scope) in import_scopes {
        let s = ctx.package_scopes.entry(package).or_default();
        s.import_scopes = scope;
    }
}

fn get_all_package_asts<'a>(
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

pub(crate) struct Context<'a, E> {
    pub(crate) arena: &'a Bump,
    pub(crate) files: &'a FileManager,
    pub(crate) errors: &'a E,

    pub(crate) symbols: SymbolInterner<'a>,

    pub(crate) package_scopes: IndexMap<Symbol<'a>, Scopes<'a>>,
}

#[derive(Default, Clone)]
pub(crate) struct Scopes<'a> {
    pub(crate) import_scopes: Scope<'a, ImportObject<'a>>,
    pub(crate) type_scopes: Scope<'a, TypeObject<'a>>,
}

pub(crate) struct ImportObject<'a> {
    pub(crate) package: Symbol<'a>,
}

pub(crate) struct TypeObject<'a> {
    pub(crate) _phantom: PhantomData<&'a ()>,
}

fn build_imports<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: IndexMap<Symbol<'a>, Vec<ItemNode>>,
) -> IndexMap<Symbol<'a>, Scope<'a, ImportObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<ImportObject>>::default();

    for (package_name, items) in package_asts {
        let mut table = IndexMap::<Symbol, ImportObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        for item in items {
            let Some(import_node) = item.as_import() else {
                continue;
            };

            let object_name = ctx.symbols.define(item.name());
            let object_id = DefId {
                package: package_name,
                name: object_name,
            };

            let package_path = match std::str::from_utf8(&import_node.path.value) {
                Ok(v) => v,
                Err(..) => {
                    ctx.errors.invalid_utf8_package(import_node.path.pos);
                    continue;
                }
            };
            let package = ctx.symbols.define(package_path);

            let pos = item.pos();
            if let Some(declared_at) = object_pos.get(&object_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors.redeclared_symbol(pos, declared_at, object_name);
                continue;
            }
            object_pos.insert(object_id, pos);

            table.insert(object_name, ImportObject { package });
        }

        let scope = Scope::<ImportObject>::new(table);
        package_scopes.insert(package_name, scope);
    }

    package_scopes
}
