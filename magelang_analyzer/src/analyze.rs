use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::{display_name, DefId, Name};
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::*;
use crate::statements::{get_stmt_from_block_node, monomorphize_stmt, Statement};
use crate::symbols::{SymbolId, SymbolInterner};
use crate::tree_ir::{build_ir, Annotation, Package};
use crate::ty::{
    display_type_id, get_func_type_from_node, get_type_from_node, is_type_assignable,
    substitute_generic_args, NamedStructType, StructBody, Type, TypeArgsId, TypeArgsInterner,
    TypeId, TypeInterner,
};
use crate::value::value_from_string_lit;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{
    parse, BlockStatementNode, ErrorReporter, FileManager, FunctionNode, GlobalNode, ImportNode,
    ItemNode, PackageNode, Pos, SignatureNode, StructNode,
};
use std::cell::OnceCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::Path;
use std::rc::Rc;

pub fn analyze(
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) -> Option<Package> {
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
    generate_object_values(&typecheck_ctx);
    generate_function_bodies(&typecheck_ctx);
    monomorphize_functions(&typecheck_ctx);

    if error_manager.has_errors() {
        return None;
    }

    Some(build_ir(&typecheck_ctx))
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
        value: OnceCell::default(),
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
            body: OnceCell::default(),
        })
    } else {
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature: func_node.signature,
            body_node: func_node.body,
            ty: OnceCell::default(),
            annotations,
            body: OnceCell::default(),
            monomorphized: OnceCell::default(),
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
            body: OnceCell::default(),
        })
    } else {
        Object::GenericFunc(GenericFuncObject {
            def_id,
            signature,
            body_node: None,
            ty: OnceCell::default(),
            annotations,
            body: OnceCell::default(),
            monomorphized: OnceCell::default(),
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
        let ctx = ctx.with_scope(scope.clone());
        for (_, object) in scope.iter() {
            match object {
                Object::Func(func_object) => {
                    let func_type = get_func_type_from_node(&ctx, &func_object.signature);
                    let type_id = ctx.types.define(Type::Func(func_type));
                    func_object
                        .ty
                        .set(type_id)
                        .expect("cannot set function type");
                }
                Object::GenericFunc(generic_obj) => {
                    let func_type = get_func_type_from_node(&ctx, &generic_obj.signature);
                    let type_id = ctx.types.define(Type::Func(func_type));
                    generic_obj
                        .ty
                        .set(type_id)
                        .expect("cannot set function type");
                }
                Object::Global(global_obj) => {
                    let type_id = get_type_from_node(&ctx, &global_obj.node.ty);
                    global_obj.ty.set(type_id).expect("cannot set global type");
                }
                _ => continue,
            };
        }
    }
}

fn generate_object_values<E: ErrorReporter>(ctx: &TypeCheckContext<E>) {
    for (_, scope) in ctx.package_scopes.iter() {
        let ctx = ctx.with_scope(scope.clone());
        for (_, object) in scope.iter() {
            match object {
                Object::Global(global_obj) => {
                    let type_id = global_obj.ty.get().expect("cannot set global type");
                    let value_expr = if let Some(ref expr) = global_obj.node.value {
                        get_expr_from_node(&ctx, Some(*type_id), expr)
                    } else {
                        Expr {
                            ty: *type_id,
                            kind: ExprKind::Zero,
                            assignable: false,
                        }
                    };

                    if !is_type_assignable(&ctx, *type_id, value_expr.ty) {
                        let pos = global_obj
                            .node
                            .value
                            .as_ref()
                            .map(|expr| expr.pos())
                            .unwrap_or(global_obj.node.pos);
                        ctx.errors.type_mismatch(
                            pos,
                            display_type_id(&ctx, *type_id),
                            display_type_id(&ctx, value_expr.ty),
                        );
                    };

                    global_obj
                        .value
                        .set(value_expr)
                        .expect("cannot set global value expression");
                }
                _ => continue,
            };
        }
    }
}

fn generate_function_bodies<E: ErrorReporter>(ctx: &TypeCheckContext<E>) {
    for (_, scope) in ctx.package_scopes.iter() {
        let ctx = ctx.with_scope(scope.clone());
        for (_, object) in scope.iter() {
            match object {
                Object::Func(func_object) => {
                    let body = if let Some(ref body) = func_object.body_node {
                        get_func_body_from_node(&ctx, &func_object.signature, body)
                    } else {
                        Statement::Native
                    };
                    func_object
                        .body
                        .set(body)
                        .expect("cannot set function body");
                }
                Object::GenericFunc(generic_func_object) => {
                    let body = if let Some(ref body) = generic_func_object.body_node {
                        get_func_body_from_node(&ctx, &generic_func_object.signature, body)
                    } else {
                        Statement::Native
                    };
                    generic_func_object
                        .body
                        .set(body)
                        .expect("cannot set generic function body");
                }
                _ => continue,
            };
        }
    }
}

fn get_func_body_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    signature: &SignatureNode,
    body: &BlockStatementNode,
) -> Statement {
    let scope = build_scope_for_typeparam(ctx, &signature.type_params);
    let ctx = ctx.with_scope(scope);

    let mut symbol_table = IndexMap::default();
    let mut last_unused_local = 0;
    for param in &signature.parameters {
        let name = ctx.symbols.define(&param.name.value);
        if symbol_table.contains_key(&name) {
            continue;
        }
        let type_id = get_type_from_node(&ctx, &param.ty);
        symbol_table.insert(
            name,
            Object::Local(LocalObject {
                id: last_unused_local,
                ty: type_id,
            }),
        );
        last_unused_local += 1;
    }
    let scope = Rc::new(ctx.scope.new_child(symbol_table));
    let ctx = ctx.with_scope(scope);

    let return_type_id = if let Some(type_expr) = &signature.return_type {
        get_type_from_node(&ctx, type_expr)
    } else {
        ctx.types.define(Type::Void)
    };

    let result = get_stmt_from_block_node(&ctx, last_unused_local, return_type_id, false, body);
    let should_return = return_type_id != ctx.types.define(Type::Void);
    if should_return && !result.is_returning {
        ctx.errors.missing_return(signature.pos);
    }
    result.statement
}

fn monomorphize_functions<E>(ctx: &TypeCheckContext<E>) {
    #[derive(Debug)]
    enum Source<'a> {
        Expr(&'a Expr, Rc<[TypeId]>),
        Statement(&'a Statement, Rc<[TypeId]>),
        FuncInst(DefId, TypeArgsId),
    }

    let mut queue = VecDeque::<Source>::default();
    for scope in ctx.package_scopes.values() {
        for (_, object) in scope.iter() {
            match object {
                Object::Import(..)
                | Object::Struct(..)
                | Object::Type(..)
                | Object::Local(..)
                | Object::GenericStruct(..)
                | Object::GenericFunc(..) => continue,
                Object::Global(global_object) => {
                    let value = global_object
                        .value
                        .get()
                        .expect("missing global value expr");
                    queue.push_back(Source::Expr(value, Rc::from([])));
                }
                Object::Func(func_obj) => {
                    let body = func_obj.body.get().expect("missing func body");
                    queue.push_back(Source::Statement(body, Rc::from([])));
                }
            }
        }
    }

    let mut monomorphized_funcs = HashMap::<DefId, Vec<TypeArgsId>>::default();
    let mut func_insts = IndexSet::<(DefId, TypeArgsId)>::default();
    while let Some(item) = queue.pop_front() {
        match item {
            Source::Expr(expr, type_scope) => match &expr.kind {
                ExprKind::Invalid
                | ExprKind::ConstI8(..)
                | ExprKind::ConstI16(..)
                | ExprKind::ConstI32(..)
                | ExprKind::ConstI64(..)
                | ExprKind::ConstIsize(..)
                | ExprKind::ConstF32(..)
                | ExprKind::ConstF64(..)
                | ExprKind::ConstBool(..)
                | ExprKind::Zero
                | ExprKind::Bytes(..)
                | ExprKind::Local(..)
                | ExprKind::Global(..)
                | ExprKind::Func(..) => (),
                ExprKind::StructLit(_, values) => {
                    for val in values {
                        queue.push_back(Source::Expr(val, type_scope.clone()))
                    }
                }
                ExprKind::FuncInst(def_id, typeargs_id) => {
                    let typeargs = ctx.typeargs.get(*typeargs_id);
                    let substituted_typeargs = typeargs
                        .iter()
                        .map(|type_id| substitute_generic_args(ctx, &type_scope, *type_id))
                        .collect::<Vec<_>>();
                    let substituted_typearg_id = ctx.typeargs.define(&substituted_typeargs);
                    queue.push_back(Source::FuncInst(*def_id, substituted_typearg_id));
                }
                ExprKind::GetElement(expr, _) => queue.push_back(Source::Expr(expr, type_scope)),
                ExprKind::GetElementAddr(expr, _) => {
                    queue.push_back(Source::Expr(expr, type_scope))
                }
                ExprKind::GetIndex(arr, index) => {
                    queue.push_back(Source::Expr(arr, type_scope.clone()));
                    queue.push_back(Source::Expr(index, type_scope.clone()));
                }
                ExprKind::Deref(value) => {
                    queue.push_back(Source::Expr(value, type_scope));
                }
                ExprKind::Call(callee, args) => {
                    queue.push_back(Source::Expr(callee, type_scope.clone()));
                    for arg in args {
                        queue.push_back(Source::Expr(arg, type_scope.clone()));
                    }
                }
                ExprKind::Add(a, b)
                | ExprKind::Sub(a, b)
                | ExprKind::Mul(a, b)
                | ExprKind::Div(a, b)
                | ExprKind::Mod(a, b)
                | ExprKind::BitOr(a, b)
                | ExprKind::BitAnd(a, b)
                | ExprKind::BitXor(a, b)
                | ExprKind::ShiftLeft(a, b)
                | ExprKind::ShiftRight(a, b)
                | ExprKind::And(a, b)
                | ExprKind::Or(a, b)
                | ExprKind::Eq(a, b)
                | ExprKind::NEq(a, b)
                | ExprKind::Gt(a, b)
                | ExprKind::GEq(a, b)
                | ExprKind::Lt(a, b)
                | ExprKind::LEq(a, b) => {
                    queue.push_back(Source::Expr(a, type_scope.clone()));
                    queue.push_back(Source::Expr(b, type_scope.clone()));
                }
                ExprKind::Neg(value)
                | ExprKind::BitNot(value)
                | ExprKind::Not(value)
                | ExprKind::Cast(value, _) => queue.push_back(Source::Expr(value, type_scope)),
            },
            Source::Statement(stmt, type_scope) => match stmt {
                Statement::NewLocal(expr) => queue.push_back(Source::Expr(expr, type_scope)),
                Statement::Block(stmts) => {
                    for stmt in stmts {
                        queue.push_back(Source::Statement(stmt, type_scope.clone()));
                    }
                }
                Statement::If(if_stmt) => {
                    queue.push_back(Source::Expr(&if_stmt.cond, type_scope.clone()));
                    queue.push_back(Source::Statement(&if_stmt.body, type_scope.clone()));
                    if let Some(else_stmt) = &if_stmt.else_stmt {
                        queue.push_back(Source::Statement(else_stmt.as_ref(), type_scope.clone()));
                    }
                }
                Statement::While(while_stmt) => {
                    queue.push_back(Source::Expr(&while_stmt.cond, type_scope.clone()));
                    queue.push_back(Source::Statement(&while_stmt.body, type_scope.clone()));
                }
                Statement::Return(value) => {
                    if let Some(value) = value {
                        queue.push_back(Source::Expr(value, type_scope));
                    }
                }
                Statement::Expr(expr) => {
                    queue.push_back(Source::Expr(expr, type_scope));
                }
                Statement::Assign(target, value) => {
                    queue.push_back(Source::Expr(target, type_scope.clone()));
                    queue.push_back(Source::Expr(value, type_scope.clone()));
                }
                Statement::Native | Statement::Continue | Statement::Break => continue,
            },
            Source::FuncInst(def_id, typeargs_id) => {
                if func_insts.contains(&(def_id, typeargs_id)) {
                    continue;
                }
                func_insts.insert((def_id, typeargs_id));

                let typeargs = ctx.typeargs.get(typeargs_id);
                let generic_func = ctx
                    .package_scopes
                    .get(&def_id.package)
                    .expect("missing package scope")
                    .lookup(def_id.name)
                    .expect("missing object")
                    .as_generic_func()
                    .expect("not a generic func");

                queue.push_back(Source::Statement(
                    generic_func.body.get().expect("missing body"),
                    typeargs.clone(),
                ));

                monomorphized_funcs
                    .entry(def_id)
                    .or_default()
                    .push(typeargs_id);
            }
        }
    }

    for (def_id, typeargs_ids) in monomorphized_funcs {
        let generic_func_obj = ctx
            .package_scopes
            .get(&def_id.package)
            .expect("missing package scope")
            .lookup(def_id.name)
            .expect("missing object")
            .as_generic_func()
            .expect("object is not generic func");

        let mut monomorphized = IndexMap::<TypeArgsId, (TypeId, Statement)>::default();
        for typeargs_id in typeargs_ids {
            let body_template = generic_func_obj
                .body
                .get()
                .expect("missing generic func body");

            let typeargs = ctx.typeargs.get(typeargs_id);
            let monomorphized_type = substitute_generic_args(
                ctx,
                &typeargs,
                *generic_func_obj.ty.get().expect("missing func type"),
            );
            let monomorphized_stmt = monomorphize_stmt(ctx, &typeargs, body_template);
            monomorphized.insert(typeargs_id, (monomorphized_type, monomorphized_stmt));
        }

        generic_func_obj
            .monomorphized
            .set(monomorphized)
            .expect("cannot set monomorphization");
    }
}
