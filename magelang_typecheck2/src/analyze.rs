use crate::errors::SemanticError;
use crate::generic_ty::{
    get_typeparams, GenericStructType, GenericType, GenericTypeInterner, GenericTypeKind,
    GenericTypeRepr, TypeArgsInterner,
};
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::ty::{
    check_circular_type, BitSize, FloatType, StructType, Type, TypeInterner, TypeKind, TypeRepr,
};
use crate::{DefId, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode, Pos, StructNode};
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
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

    let types = TypeInterner::new(arena);
    let typeargs = TypeArgsInterner::new(arena);
    let generic_types = GenericTypeInterner::new(arena);

    let mut ctx = Context {
        arena,
        files: file_manager,
        errors: error_manager,
        symbols,
        types,
        typeargs,
        generic_types,
        package_scopes: IndexMap::default(),
    };

    let import_scopes = build_imports(&ctx, import_items);
    for (package, scope) in import_scopes {
        let s = ctx.package_scopes.entry(package).or_default();
        s.import_scopes = scope;
    }

    let type_scopes = build_type_scopes(&ctx, struct_items);
    for (package, scope) in type_scopes {
        let s = ctx.package_scopes.entry(package).or_default();
        s.type_scopes = scope;
    }

    generate_type_body(&ctx);
    check_circular_type(&ctx);
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
    pub(crate) types: TypeInterner<'a>,
    pub(crate) typeargs: TypeArgsInterner<'a>,
    pub(crate) generic_types: GenericTypeInterner<'a>,

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
    pub(crate) kind: TypeObjectKind<'a>,
    pub(crate) node: Option<StructNode>,
}

pub(crate) enum TypeObjectKind<'a> {
    Regular(&'a Type<'a>),
    Generic(&'a GenericType<'a>),
}

impl<'a> From<&'a Type<'a>> for TypeObject<'a> {
    fn from(ty: &'a Type<'a>) -> Self {
        Self {
            kind: TypeObjectKind::Regular(ty),
            node: None,
        }
    }
}

impl<'a> TypeObject<'a> {
    fn init_body<E: ErrorReporter>(&self, ctx: &Context<'a, E>) {
        match self.kind {
            TypeObjectKind::Regular(ty) => ty.init_body(ctx),
            TypeObjectKind::Generic(generic_ty) => generic_ty.init_body(ctx),
        }
    }
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

fn build_type_scopes<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: IndexMap<Symbol<'a>, Vec<ItemNode>>,
) -> IndexMap<Symbol<'a>, Scope<'a, TypeObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<TypeObject<'a>>>::default();

    let builtin_scope = get_builtin_scope(ctx);
    for (package_name, items) in package_asts {
        let mut table = IndexMap::<Symbol, TypeObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        for item in items {
            let ItemNode::Struct(struct_node) = item else {
                continue;
            };

            let object_name = ctx.symbols.define(&struct_node.name.value);
            let def_id = DefId {
                package: package_name,
                name: object_name,
            };

            let pos = struct_node.pos;
            if let Some(declared_at) = object_pos.get(&def_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors.redeclared_symbol(pos, declared_at, object_name);
                continue;
            }
            object_pos.insert(def_id, pos);

            if struct_node.type_params.is_empty() {
                let ty = ctx.types.define(Type {
                    kind: TypeKind::User(def_id),
                    repr: TypeRepr::Struct(StructType {
                        body: OnceCell::default(),
                    }),
                });
                let object = TypeObject {
                    kind: TypeObjectKind::Regular(ty),
                    node: Some(struct_node),
                };

                table.insert(object_name, object);
            } else {
                let params = get_typeparams(ctx, &struct_node.type_params);
                let generic_ty = ctx.generic_types.define(GenericType::new(
                    GenericTypeKind::User(def_id),
                    params,
                    GenericTypeRepr::Struct(GenericStructType {
                        body: OnceCell::default(),
                    }),
                ));

                let object = TypeObject {
                    kind: TypeObjectKind::Generic(generic_ty),
                    node: Some(struct_node),
                };

                table.insert(object_name, object);
            }
        }

        let scope = builtin_scope.new_child(table);
        package_scopes.insert(package_name, scope);
    }

    package_scopes
}

fn get_builtin_scope<'a, E: ErrorReporter>(ctx: &Context<'a, E>) -> Scope<'a, TypeObject<'a>> {
    let i8_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(true, BitSize::I8),
    });
    let i16_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(true, BitSize::I16),
    });
    let i32_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(true, BitSize::I32),
    });
    let i64_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(true, BitSize::I64),
    });
    let isize_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(true, BitSize::ISize),
    });
    let u8_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I8),
    });
    let u16_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I16),
    });
    let u32_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I32),
    });
    let u64_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::I64),
    });
    let usize_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Int(false, BitSize::ISize),
    });
    let f32_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Float(FloatType::F32),
    });
    let f64_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Float(FloatType::F64),
    });
    let void_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Void,
    });
    let opaque_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Opaque,
    });
    let bool_type = ctx.types.define(Type {
        kind: TypeKind::Anonymous,
        repr: TypeRepr::Bool,
    });

    let builtin_scope = Scope::new(IndexMap::from([
        (ctx.symbols.define("i8"), i8_type.into()),
        (ctx.symbols.define("i16"), i16_type.into()),
        (ctx.symbols.define("i32"), i32_type.into()),
        (ctx.symbols.define("i64"), i64_type.into()),
        (ctx.symbols.define("isize"), isize_type.into()),
        (ctx.symbols.define("u8"), u8_type.into()),
        (ctx.symbols.define("u16"), u16_type.into()),
        (ctx.symbols.define("u32"), u32_type.into()),
        (ctx.symbols.define("u64"), u64_type.into()),
        (ctx.symbols.define("f32"), f32_type.into()),
        (ctx.symbols.define("f64"), f64_type.into()),
        (ctx.symbols.define("usize"), usize_type.into()),
        (ctx.symbols.define("void"), void_type.into()),
        (ctx.symbols.define("opaque"), opaque_type.into()),
        (ctx.symbols.define("bool"), bool_type.into()),
    ]));

    builtin_scope
}

fn generate_type_body<E: ErrorReporter>(ctx: &Context<'_, E>) {
    for scopes in ctx.package_scopes.values() {
        for (_, type_object) in scopes.type_scopes.iter() {
            let TypeObjectKind::Regular(ty) = &type_object.kind else {
                continue;
            };
            ty.init_body(ctx);
            assert!(ty.as_struct().unwrap().body.get().is_some());
        }
    }

    for scopes in ctx.package_scopes.values() {
        for (_, type_object) in scopes.type_scopes.iter() {
            let TypeObjectKind::Generic(generic_ty) = &type_object.kind else {
                continue;
            };
            generic_ty.init_body(ctx);
            assert!(generic_ty.as_struct().unwrap().body.get().is_some());
        }
    }
}
