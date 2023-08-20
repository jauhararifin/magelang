use crate::errors::SemanticError;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::*;
use crate::symbols::{SymbolId, SymbolInterner};
use crate::ty::{NamedStructType, StructBody, Type, TypeArg, TypeArgsInterner, TypeInterner};
use crate::value::value_from_string_lit;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{
    parse, ErrorReporter, FileManager, FunctionNode, GlobalNode, ImportNode, ItemNode, PackageNode,
    Pos, SignatureNode, StructNode, TypeParameterNode,
};
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

pub fn analyze(
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) {
    let symbols = SymbolInterner::default();
    let types = TypeInterner::default();
    let typeargs = TypeArgsInterner::default();

    let stdlib_path = get_stdlib_path();
    let main_package = symbols.define(main_package);
    let package_asts = get_all_package_asts(
        file_manager,
        error_manager,
        &symbols,
        &stdlib_path,
        main_package,
    );

    let ctx = Context {
        files: file_manager,
        errors: error_manager,
        symbols: &symbols,
        types: &types,
        typeargs: &typeargs,
    };

    let object_nodes = build_object_nodes(&ctx, package_asts);
    let symbol_tables = build_symbol_table(&ctx, object_nodes);
    let builtin_scope = get_builtin_scope(&ctx);
    let package_scopes = build_package_scope(builtin_scope, symbol_tables);
    generate_struct_bodies(&ctx, &package_scopes);
}

pub struct Context<'a, E> {
    pub files: &'a FileManager,
    pub errors: &'a E,
    pub symbols: &'a SymbolInterner,
    pub types: &'a TypeInterner,
    pub typeargs: &'a TypeArgsInterner,
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

fn build_object_nodes<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    package_asts: IndexMap<SymbolId, PackageNode>,
) -> IndexMap<DefId, ItemNode> {
    let mut object_nodes = IndexMap::<DefId, ItemNode>::default();
    let mut object_pos = HashMap::<DefId, Pos>::default();

    for (package_name, package_ast) in package_asts {
        for item in package_ast.items {
            let object_name = ctx.symbols.define(item.name());
            let object_id = DefId {
                package: package_name,
                name: object_name,
            };

            let pos = item.pos();
            if let Some(declared_at) = object_pos.get(&object_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors
                    .redeclared_symbol(pos, declared_at, &ctx.symbols.get(object_name));
                continue;
            }

            object_nodes.insert(object_id, item);
            object_pos.insert(object_id, pos);
        }
    }

    object_nodes
}

fn build_symbol_table<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    object_nodes: IndexMap<DefId, ItemNode>,
) -> IndexMap<DefId, Object> {
    let mut symbol_table = IndexMap::<DefId, Object>::default();
    for (def_id, item_node) in object_nodes {
        let object = match item_node {
            ItemNode::Import(node) => init_import_object(ctx, node),
            ItemNode::Struct(node) => Some(init_struct_objects(ctx, def_id, node)),
            ItemNode::Global(global_node) => Some(init_global_object(def_id, global_node)),
            ItemNode::Function(func_node) => Some(init_function_object(ctx, def_id, func_node)),
        };
        let Some(object) = object else { continue };

        symbol_table.insert(def_id, object);
    }
    symbol_table
}

fn init_import_object<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    import_node: ImportNode,
) -> Option<Object> {
    let import_path = import_node.path.value.as_str();
    let Some(package_path) = value_from_string_lit(import_path) else {
        ctx.errors.invalid_utf8_package(import_node.path.pos);
        return None;
    };
    let package_path = match String::from_utf8(package_path) {
        Ok(v) => v,
        Err(..) => {
            ctx.errors.invalid_utf8_package(import_node.path.pos);
            return None;
        }
    };
    let package_name = ctx.symbols.define(package_path.as_str());
    Some(Object::Import(ImportObject {
        package: package_name,
    }))
}

fn init_struct_objects<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    def_id: DefId,
    struct_node: StructNode,
) -> Object {
    if struct_node.type_params.is_empty() {
        let ty = Type::NamedStruct(NamedStructType {
            def_id,
            body: OnceCell::default(),
        });
        let type_id = ctx.types.define(ty);
        Object::Struct(StructObject {
            def_id,
            node: struct_node,
            type_id,
        })
    } else {
        let type_params = get_typeparams_from_node(ctx, &struct_node.type_params);
        Object::GenericStruct(GenericStructObject {
            def_id,
            type_params,
            node: struct_node,
            body: OnceCell::default(),
        })
    }
}

fn get_typeparams_from_node<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    node: &[TypeParameterNode],
) -> IndexSet<SymbolId> {
    let mut type_param_pos = HashMap::<SymbolId, Pos>::default();
    for type_param_node in node {
        let type_name = type_param_node.name.value.as_str();
        let type_param_name = ctx.symbols.define(type_name);
        let pos = type_param_node.name.pos;
        if let Some(defined_at) = type_param_pos.get(&type_param_name) {
            ctx.errors.redeclared_symbol(
                pos,
                ctx.files.location(*defined_at),
                &type_param_node.name.value,
            );
        } else {
            type_param_pos.insert(type_param_name, pos);
        }
    }

    type_param_pos.into_keys().collect()
}

fn init_global_object(def_id: DefId, global_node: GlobalNode) -> Object {
    Object::Global(GlobalObject {
        def_id,
        node: global_node,
        ty: OnceCell::default(),
    })
}

fn init_function_object<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    def_id: DefId,
    func_node: FunctionNode,
) -> Object {
    if func_node.body.is_none() {
        return init_native_function_object(ctx, def_id, func_node.signature);
    }

    let annotations = build_annotations_from_node(ctx, &func_node.signature).into();
    if func_node.signature.type_params.is_empty() {
        Object::Func(FuncObject {
            def_id,
            signature: func_node.signature,
            body_node: func_node.body,
            ty: OnceCell::default(),
            annotations,
        })
    } else {
        let type_params = get_typeparams_from_node(ctx, &func_node.signature.type_params);
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature: func_node.signature,
            body_node: func_node.body,
            type_params,
            ty: OnceCell::default(),
            annotations,
        })
    }
}

fn build_annotations_from_node<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    signature: &SignatureNode,
) -> Vec<Annotation> {
    let mut annotations = Vec::default();
    for annotation_node in &signature.annotations {
        let mut arguments = Vec::default();
        let mut valid = true;
        for arg in &annotation_node.arguments {
            let Some(arg_value) = value_from_string_lit(&arg.value) else {
                ctx.errors.invalid_utf8_string(arg.pos);
                valid = false;
                continue;
            };

            let Ok(arg_value) = String::from_utf8(arg_value) else {
                ctx.errors.invalid_utf8_string(arg.pos);
                valid = false;
                continue;
            };

            arguments.push(arg_value);
        }
        if valid {
            annotations.push(Annotation {
                name: annotation_node.name.value.clone(),
                arguments,
            })
        }
    }
    annotations
}

fn init_native_function_object<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    def_id: DefId,
    signature: SignatureNode,
) -> Object {
    let annotations = build_annotations_from_node(ctx, &signature).into();
    if signature.type_params.is_empty() {
        Object::Func(FuncObject {
            def_id,
            signature,
            body_node: None,
            ty: OnceCell::default(),
            annotations,
        })
    } else {
        let type_params = get_typeparams_from_node(ctx, &signature.type_params);
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature,
            body_node: None,
            type_params,
            ty: OnceCell::default(),
            annotations,
        })
    }
}

fn build_package_scope(
    builtin_scope: Rc<Scope>,
    symbol_table: IndexMap<DefId, Object>,
) -> HashMap<SymbolId, Rc<Scope>> {
    let mut package_table = HashMap::<SymbolId, IndexMap<SymbolId, Object>>::default();
    for (def_id, object) in symbol_table {
        let table = package_table.entry(def_id.package).or_default();
        table.insert(def_id.name, object);
    }

    let mut package_scopes = HashMap::<SymbolId, Rc<Scope>>::default();
    for (package_name, table) in package_table {
        package_scopes.insert(package_name, Rc::new(builtin_scope.new_child(table)));
    }

    package_scopes
}

fn generate_struct_bodies<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    package_scopes: &HashMap<SymbolId, Rc<Scope>>,
) {
    for (_, scope) in package_scopes.iter() {
        for (_, object) in scope.iter() {
            match object {
                Object::Struct(struct_object) => {
                    let scope = scope.clone();
                    let body = get_struct_body_from_node(ctx, &scope, &struct_object.node);

                    ctx.types
                        .get(struct_object.type_id)
                        .as_named_struct()
                        .expect("not a named struct")
                        .body
                        .set(body)
                        .expect("cannot set struct body");
                }
                Object::GenericStruct(generic_obj) => {
                    let scope =
                        build_scope_for_typeparam(ctx, scope, generic_obj.type_params.iter());
                    let body = get_struct_body_from_node(ctx, &scope, &generic_obj.node);
                    generic_obj
                        .body
                        .set(body)
                        .expect("cannot set generic struct body");
                }
                _ => continue,
            };
        }
    }
}

fn build_scope_for_typeparam<'ctx, 'a, E>(
    ctx: &Context<'ctx, E>,
    scope: &Rc<Scope>,
    type_params: impl Iterator<Item = &'a SymbolId>,
) -> Rc<Scope> {
    let mut typeparam_table = IndexMap::<SymbolId, Object>::default();
    for (index, typeparam) in type_params.enumerate() {
        let ty = ctx.types.define(Type::TypeArg(TypeArg {
            index,
            symbol: *typeparam,
        }));
        let type_param_obj = Object::Type(ty);
        typeparam_table.insert(*typeparam, type_param_obj);
    }
    Rc::new(scope.new_child(typeparam_table))
}

fn get_struct_body_from_node<'ctx, E: ErrorReporter>(
    ctx: &Context<'ctx, E>,
    scope: &Scope,
    node: &StructNode,
) -> StructBody {
    todo!()
}
