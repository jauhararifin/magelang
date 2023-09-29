use crate::errors::SemanticError;
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::ty::{InternType, InternTypeArgs, Type, TypeArgsInterner, TypeInterner};
use crate::value::value_from_string_lit;
use crate::{DefId, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode, Pos};
use std::collections::{HashMap, HashSet};
use std::path::Path;

pub fn analyze(
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) {
    let arena = Bump::default();

    let symbols = SymbolInterner::new(&arena);
    let types = TypeInterner::new(&arena);
    let typeargs = TypeArgsInterner::new(&arena);

    let stdlib_path = get_stdlib_path();
    let main_package = symbols.define(main_package);
    let package_asts = get_all_package_asts(
        file_manager,
        error_manager,
        &symbols,
        &stdlib_path,
        main_package,
    );

    let interners = Interners {
        symbols: &symbols,
        types: &types,
        typeargs: &typeargs,
    };
    let mut ctx = Context {
        files: file_manager,
        errors: error_manager,
        interners,
        scopes: IndexMap::default(),
        current_scope: Scopes::default(),
    };

    let import_scope = build_imports(&ctx, &package_asts);
    ctx.set_import_scope(import_scope);
}

struct Context<'a, E> {
    files: &'a FileManager,
    errors: &'a E,

    interners: Interners<'a>,
    scopes: IndexMap<Symbol<'a>, Scopes<'a>>,
    current_scope: Scopes<'a>,
}

impl<'a, E> Context<'a, E> {
    fn define_symbol(&self, symbol: &str) -> Symbol<'a> {
        self.interners.symbols.define(symbol)
    }

    fn define_type(&self, ty: Type<'a>) -> InternType<'a> {
        self.interners.types.define(ty)
    }

    fn define_typeargs(&self, typeargs: &[InternType<'a>]) -> InternTypeArgs<'a> {
        self.interners.typeargs.define(typeargs)
    }

    fn set_import_scope(
        &mut self,
        import_scopes: IndexMap<Symbol<'a>, Scope<'a, ImportObject<'a>>>,
    ) {
        for (package, scope) in import_scopes {
            let s = self.scopes.entry(package).or_default();
            s.import_scopes = scope;
        }
    }
}

struct Interners<'a> {
    symbols: &'a SymbolInterner<'a>,
    types: &'a TypeInterner<'a>,
    typeargs: &'a TypeArgsInterner<'a>,
}

#[derive(Default)]
struct Scopes<'a> {
    import_scopes: Scope<'a, ImportObject<'a>>,
    type_scopes: Scope<'a, TypeObject<'a>>,
    value_scopes: Scope<'a, ValueObject<'a>>,
}

struct ImportObject<'a> {
    package: Symbol<'a>,
}

struct TypeObject<'a> {
    ty: InternType<'a>,
}

enum ValueObject<'a> {
    Global(GlobalObject<'a>),
    Func,
    Local,
}

struct GlobalObject<'a> {
    def_id: DefId<'a>,
    ty: InternType<'a>,
}

fn get_all_package_asts<'a>(
    files: &mut FileManager,
    errors: &impl ErrorReporter,
    symbols: &'a SymbolInterner,
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

        let path = get_package_path(stdlib_path, &package_name);
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
            let package_path = symbols.define(&import_path);
            if !in_stack.contains(&package_path) {
                stack.push(package_path.clone());
                in_stack.insert(package_path);
            }
        }
    }

    package_asts
}

fn build_imports<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: &IndexMap<Symbol<'a>, PackageNode>,
) -> IndexMap<Symbol<'a>, Scope<'a, ImportObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<ImportObject>>::default();

    for (package_name, package_ast) in package_asts {
        let mut table = IndexMap::<Symbol, ImportObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        for item in &package_ast.items {
            let Some(import_node) = item.as_import() else {
                continue;
            };

            let object_name = ctx.define_symbol(item.name());
            let object_id = DefId {
                package: package_name.clone(),
                name: object_name.clone(),
            };

            let Some(package_path) = value_from_string_lit(&import_node.path.value) else {
                ctx.errors.invalid_utf8_package(import_node.path.pos);
                continue;
            };

            let package_path = match String::from_utf8(package_path) {
                Ok(v) => v,
                Err(..) => {
                    ctx.errors.invalid_utf8_package(import_node.path.pos);
                    continue;
                }
            };
            let package = ctx.define_symbol(package_path.as_str());

            let pos = item.pos();
            if let Some(declared_at) = object_pos.get(&object_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors.redeclared_symbol(pos, declared_at, &object_name);
                continue;
            }
            object_pos.insert(object_id, pos);

            table.insert(object_name, ImportObject { package });
        }

        let scope = Scope::<ImportObject>::new(table);
        package_scopes.insert(package_name.clone(), scope);
    }

    package_scopes
}
