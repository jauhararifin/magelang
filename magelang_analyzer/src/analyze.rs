use crate::errors::SemanticError;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::{display_name, DefId, Name};
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::*;
use crate::symbols::{SymbolId, SymbolInterner};
use crate::ty::{
    get_func_type_from_node, get_type_from_node, NamedStructType, StructBody, Type,
    TypeArgsInterner, TypeId, TypeInterner,
};
use crate::value::value_from_string_lit;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{
    parse, ErrorReporter, FileManager, FunctionNode, GlobalNode, ImportNode, ItemNode, PackageNode,
    Pos, SignatureNode, StructNode,
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
    let package_scopes = build_package_scope(builtin_scope.clone(), symbol_tables);

    let typecheck_ctx = TypeCheckContext {
        files: file_manager,
        errors: error_manager,
        symbols: &symbols,
        types: &types,
        typeargs: &typeargs,
        package_scopes: &package_scopes,
        scope: builtin_scope,
    };
    generate_struct_bodies(&typecheck_ctx);
    check_circular_types(&typecheck_ctx);
    // TODO: check circular global initialization.
    generate_object_types(&typecheck_ctx);
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

fn build_object_nodes<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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

fn build_symbol_table<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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

fn init_import_object<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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
        pos: import_node.pos,
    }))
}

fn init_struct_objects<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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
        Object::GenericStruct(GenericStructObject {
            def_id,
            node: struct_node,
            body: OnceCell::default(),
        })
    }
}

fn init_global_object(def_id: DefId, global_node: GlobalNode) -> Object {
    Object::Global(GlobalObject {
        def_id,
        node: global_node,
        ty: OnceCell::default(),
    })
}

fn init_function_object<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature: func_node.signature,
            body_node: func_node.body,
            ty: OnceCell::default(),
            annotations,
        })
    }
}

fn build_annotations_from_node<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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

fn init_native_function_object<E: ErrorReporter>(
    ctx: &Context<'_, E>,
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
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature,
            body_node: None,
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

pub struct TypeCheckContext<'a, E> {
    pub files: &'a FileManager,
    pub errors: &'a E,
    pub symbols: &'a SymbolInterner,
    pub types: &'a TypeInterner,
    pub typeargs: &'a TypeArgsInterner,

    pub package_scopes: &'a HashMap<SymbolId, Rc<Scope>>,
    pub scope: Rc<Scope>,
}

impl<'a, E> TypeCheckContext<'a, E> {
    pub fn with_scope(&self, scope: Rc<Scope>) -> Self {
        Self {
            files: self.files,
            errors: self.errors,
            symbols: self.symbols,
            types: self.types,
            typeargs: self.typeargs,
            package_scopes: self.package_scopes,
            scope,
        }
    }
}

fn generate_struct_bodies<E: ErrorReporter>(ctx: &TypeCheckContext<'_, E>) {
    for (_, scope) in ctx.package_scopes.iter() {
        for (_, object) in scope.iter() {
            match object {
                Object::Struct(struct_object) => {
                    let ctx = ctx.with_scope(scope.clone());
                    let body = get_struct_body_from_node(&ctx, &struct_object.node);

                    ctx.types
                        .get(struct_object.type_id)
                        .as_named_struct()
                        .expect("not a named struct")
                        .body
                        .set(body)
                        .expect("cannot set struct body");
                }
                Object::GenericStruct(generic_obj) => {
                    let scope = build_scope_for_typeparam(ctx, &generic_obj.node.type_params);
                    let ctx = ctx.with_scope(scope);
                    let body = get_struct_body_from_node(&ctx, &generic_obj.node);
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

fn get_struct_body_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<'_, E>,
    struct_node: &StructNode,
) -> StructBody {
    let mut field_pos = HashMap::<SymbolId, Pos>::default();
    let mut fields = IndexMap::<SymbolId, TypeId>::default();
    for field_node in &struct_node.fields {
        let field_name = ctx.symbols.define(&field_node.name.value);
        let pos = field_node.pos;
        if let Some(defined_at) = field_pos.get(&field_name) {
            ctx.errors.redeclared_symbol(
                pos,
                ctx.files.location(*defined_at),
                &field_node.name.value,
            );
        } else {
            field_pos.insert(field_name, pos);
            let type_id = get_type_from_node(ctx, &field_node.ty);
            fields.insert(field_name, type_id);
        }
    }

    StructBody { fields }
}

fn check_circular_types<E: ErrorReporter>(ctx: &TypeCheckContext<'_, E>) {
    let dep_list = build_struct_dependency_list(ctx);

    let mut visited = IndexSet::<Name>::default();
    let mut in_chain = IndexSet::<Name>::default();
    for name in dep_list.keys() {
        if visited.contains(name) {
            continue;
        }

        let mut stack = vec![*name];
        while let Some(name) = stack.pop() {
            if in_chain.contains(&name) {
                in_chain.remove(&name);
                continue;
            }

            stack.push(name);
            visited.insert(name);
            in_chain.insert(name);

            for dep in dep_list.get(&name).unwrap_or(&IndexSet::default()).iter() {
                if !visited.contains(dep) {
                    stack.push(*dep);
                } else if in_chain.contains(dep) {
                    report_circular_type(ctx, &in_chain, *dep);
                }
            }
        }
    }
}

fn build_struct_dependency_list<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
) -> IndexMap<Name, IndexSet<Name>> {
    let mut adjlist = IndexMap::<Name, IndexSet<Name>>::default();
    let type_objects = ctx
        .package_scopes
        .iter()
        .flat_map(|(_, scope)| scope.iter())
        .filter_map(|(_, object)| object.type_id());

    for type_id in type_objects {
        let ty = ctx.types.get(type_id);
        let (struct_type, name) = match ty.as_ref() {
            Type::NamedStruct(struct_type) => (
                struct_type.body.get().expect("missing struct body"),
                Name::Def(struct_type.def_id),
            ),
            Type::NamedStructInst(struct_inst_type) => (
                &struct_inst_type.body,
                Name::Instance(struct_inst_type.def_id, struct_inst_type.type_args),
            ),
            _ => continue,
        };

        for (_, type_id) in &struct_type.fields {
            let ty = ctx.types.get(*type_id);
            let dep_name = match ty.as_ref() {
                Type::NamedStruct(named_struct) => Name::Def(named_struct.def_id),
                Type::NamedStructInst(named_struct_inst) => {
                    Name::Instance(named_struct_inst.def_id, named_struct_inst.type_args)
                }
                _ => continue,
            };
            adjlist.entry(name).or_default().insert(dep_name);
        }
    }

    adjlist
}

fn report_circular_type<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    in_chain: &IndexSet<Name>,
    start: Name,
) {
    let mut chain = Vec::default();
    let mut started = false;
    for name in in_chain {
        if name == &start {
            started = true;
        }
        if started {
            chain.push(*name);
        }
    }

    let mut chain_str = Vec::default();
    for name in chain {
        let display = display_name(ctx, &name);
        chain_str.push(display);
    }

    let def_id = match start {
        Name::Def(def_id) => def_id,
        Name::Instance(def_id, _) => def_id,
    };
    let pos = ctx
        .package_scopes
        .get(&def_id.package)
        .unwrap()
        .lookup(def_id.name)
        .unwrap()
        .pos()
        .unwrap();
    ctx.errors.circular_type(pos, &chain_str);
}

fn generate_object_types<E: ErrorReporter>(ctx: &TypeCheckContext<E>) {
    for (_, scope) in ctx.package_scopes.iter() {
        for (_, object) in scope.iter() {
            match object {
                Object::Func(func_object) => {
                    let func_type = get_func_type_from_node(ctx, &func_object.signature);
                    let type_id = ctx.types.define(Type::Func(func_type));
                    func_object
                        .ty
                        .set(type_id)
                        .expect("cannot set function type");
                }
                Object::GenericFunc(generic_obj) => {
                    let func_type = get_func_type_from_node(ctx, &generic_obj.signature);
                    let type_id = ctx.types.define(Type::Func(func_type));
                    generic_obj
                        .ty
                        .set(type_id)
                        .expect("cannot set function type");
                }
                Object::Global(global_obj) => {
                    let type_id = get_type_from_node(ctx, &global_obj.node.ty);
                    global_obj.ty.set(type_id).expect("cannot set global type");
                }
                _ => continue,
            };
        }
    }
}
