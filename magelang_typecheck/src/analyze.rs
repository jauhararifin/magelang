use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprInterner, ExprKind, InternExpr};
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::statement::{
    get_statement_from_block, InternStatement, Statement, StatementContext, StatementInterner,
};
use crate::ty::{
    check_circular_type, get_func_type_from_signature, get_type_from_node, get_typeparam_scope,
    get_typeparams, BitSize, FloatType, InternType, InternTypeArgs, StructType, Type, TypeArg,
    TypeArgsInterner, TypeInterner,
};
use crate::value::value_from_string_lit;
use crate::{DefId, Func, Global, Module, Package, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{
    parse, AnnotationNode, ErrorReporter, FileManager, FunctionNode, GlobalNode, ItemNode,
    PackageNode, Pos,
};
use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::Path;
use std::rc::Rc;

pub fn analyze<'a, 'b: 'a>(
    arena: &'a Bump,
    file_manager: &'b mut FileManager,
    error_manager: &'b impl ErrorReporter,
    main_package: &'b str,
) -> Module<'a> {
    let symbols = SymbolInterner::new(&arena);
    let types = TypeInterner::new(&arena);
    let typeargs = TypeArgsInterner::new(&arena);
    let exprs = ExprInterner::new(&arena);
    let statements = StatementInterner::new(&arena);
    let interners = Interners {
        symbols,
        types,
        typeargs,
        exprs,
        statements,
    };

    let stdlib_path = get_stdlib_path();
    let main_package = interners.symbols.define(main_package);
    let package_asts = get_all_package_asts(
        file_manager,
        error_manager,
        &interners.symbols,
        &stdlib_path,
        main_package,
    );

    let mut ctx = Context {
        files: file_manager,
        errors: error_manager,
        interners,
        scopes: IndexMap::default(),
    };

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

    let import_scopes = build_imports(&ctx, import_items);
    ctx.set_import_scope(import_scopes);

    let type_scopes = build_type_scopes(&ctx, struct_items);
    ctx.set_type_scope(type_scopes);

    generate_type_body(&ctx);
    monomorphize_types(&ctx);
    check_circular_type(&ctx);

    let value_scopes = build_value_scopes(&ctx, value_items);
    ctx.set_value_scope(value_scopes);

    // TODO: consider materialize all steps done above into a Header IR.
    // This can be useful for incremental compilation.

    generate_global_value(&ctx);
    generate_func_bodies(&ctx);
    monomorphize_statements(&ctx);

    // TODO: consider blocking circular import since it makes
    // deciding global initialization harder for incremental
    // compilation.

    let is_valid = !error_manager.has_errors();
    build_module(&ctx, is_valid)
}

pub struct Context<'a, E> {
    pub(crate) files: &'a FileManager,
    pub(crate) errors: &'a E,

    pub(crate) interners: Interners<'a>,
    pub(crate) scopes: IndexMap<Symbol<'a>, Scopes<'a>>,
}

impl<'a, E> Context<'a, E> {
    pub(crate) fn define_symbol(&self, symbol: &str) -> Symbol<'a> {
        self.interners.symbols.define(symbol)
    }

    pub(crate) fn define_type(&self, ty: Type<'a>) -> InternType<'a> {
        self.interners.types.define(ty)
    }

    pub(crate) fn define_typeargs(&self, typeargs: &[InternType<'a>]) -> InternTypeArgs<'a> {
        self.interners.typeargs.define(typeargs)
    }

    pub(crate) fn define_expr(&self, expr: Expr<'a>) -> InternExpr<'a> {
        self.interners.exprs.define(expr)
    }

    pub(crate) fn define_statement(&self, stmt: Statement<'a>) -> InternStatement<'a> {
        self.interners.statements.define(stmt)
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

    fn set_type_scope(&mut self, type_scopes: IndexMap<Symbol<'a>, Scope<'a, TypeObject<'a>>>) {
        for (package, scope) in type_scopes {
            let s = self.scopes.entry(package).or_default();
            s.type_scopes = scope;
        }
    }

    fn set_value_scope(&mut self, value_scopes: IndexMap<Symbol<'a>, Scope<'a, ValueObject<'a>>>) {
        for (package, scope) in value_scopes {
            let s = self.scopes.entry(package).or_default();
            s.value_scopes = scope;
        }
    }
}

pub struct Interners<'a> {
    symbols: SymbolInterner<'a>,
    types: TypeInterner<'a>,
    typeargs: TypeArgsInterner<'a>,
    exprs: ExprInterner<'a>,
    statements: StatementInterner<'a>,
}

#[derive(Default, Clone)]
pub(crate) struct Scopes<'a> {
    pub(crate) import_scopes: Scope<'a, ImportObject<'a>>,
    pub(crate) type_scopes: Scope<'a, TypeObject<'a>>,
    pub(crate) value_scopes: Scope<'a, ValueObject<'a>>,
}

impl<'a> Scopes<'a> {
    pub(crate) fn with_type_scope(&self, type_scopes: Scope<'a, TypeObject<'a>>) -> Self {
        Self {
            import_scopes: self.import_scopes.clone(),
            type_scopes,
            value_scopes: self.value_scopes.clone(),
        }
    }

    pub(crate) fn with_value_scope(&self, value_scopes: Scope<'a, ValueObject<'a>>) -> Self {
        Self {
            import_scopes: self.import_scopes.clone(),
            type_scopes: self.type_scopes.clone(),
            value_scopes,
        }
    }
}

pub struct ImportObject<'a> {
    pub(crate) package: Symbol<'a>,
}

pub struct TypeObject<'a> {
    pub(crate) ty: InternType<'a>,
}

impl<'a> From<InternType<'a>> for TypeObject<'a> {
    fn from(ty: InternType<'a>) -> Self {
        Self { ty }
    }
}

impl<'a> std::ops::Deref for TypeObject<'a> {
    type Target = InternType<'a>;
    fn deref(&self) -> &Self::Target {
        &self.ty
    }
}

#[derive(Debug)]
pub enum ValueObject<'a> {
    Global(GlobalObject<'a>),
    Func(FuncObject<'a>),
    Local(LocalObject<'a>),
}

#[derive(Debug)]
pub struct GlobalObject<'a> {
    pub(crate) def_id: DefId<'a>,
    pub ty: InternType<'a>,
    pub(crate) node: GlobalNode,
    pub value: OnceCell<InternExpr<'a>>,
    pub annotations: Rc<[Annotation]>,
}

#[derive(Debug)]
pub struct Annotation {
    pub name: String,
    pub arguments: Vec<String>,
}

#[derive(Debug)]
pub struct FuncObject<'a> {
    pub(crate) def_id: DefId<'a>,
    pub type_params: Vec<TypeArg<'a>>,
    pub ty: InternType<'a>,
    pub(crate) node: FunctionNode,
    pub body: OnceCell<InternStatement<'a>>,
    pub annotations: Rc<[Annotation]>,
    pub monomorphized: OnceCell<Vec<(InternTypeArgs<'a>, InternType<'a>, InternStatement<'a>)>>,
}

#[derive(Debug)]
pub struct LocalObject<'a> {
    pub id: usize,
    pub ty: InternType<'a>,
    pub name: Symbol<'a>,
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

            let object_name = ctx.define_symbol(&struct_node.name.value);
            let def_id = DefId {
                package: package_name,
                name: object_name.clone(),
            };

            let pos = struct_node.pos;
            if let Some(declared_at) = object_pos.get(&def_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors.redeclared_symbol(pos, declared_at, &object_name);
                continue;
            }
            object_pos.insert(def_id.clone(), pos);

            let type_params = get_typeparams(ctx, &struct_node.type_params);

            let ty = ctx.define_type(Type::Struct(StructType {
                def_id,
                type_params,
                body: OnceCell::default(),
                node: struct_node,
                mono_cache: RefCell::default(),
            }));
            let object: TypeObject = ty.into();

            table.insert(object_name, object);
        }

        let scope = builtin_scope.new_child(table);
        package_scopes.insert(package_name.clone(), scope);
    }

    package_scopes
}

fn get_builtin_scope<'a, E: ErrorReporter>(ctx: &Context<'a, E>) -> Scope<'a, TypeObject<'a>> {
    let i8_type = ctx.define_type(Type::Int(true, BitSize::I8));
    let i16_type = ctx.define_type(Type::Int(true, BitSize::I16));
    let i32_type = ctx.define_type(Type::Int(true, BitSize::I32));
    let i64_type = ctx.define_type(Type::Int(true, BitSize::I64));
    let isize_type = ctx.define_type(Type::Int(true, BitSize::ISize));
    let u8_type = ctx.define_type(Type::Int(false, BitSize::I8));
    let u16_type = ctx.define_type(Type::Int(false, BitSize::I16));
    let u32_type = ctx.define_type(Type::Int(false, BitSize::I32));
    let u64_type = ctx.define_type(Type::Int(false, BitSize::I64));
    let usize_type = ctx.define_type(Type::Int(false, BitSize::ISize));
    let f32_type = ctx.define_type(Type::Float(FloatType::F32));
    let f64_type = ctx.define_type(Type::Float(FloatType::F64));
    let void_type = ctx.define_type(Type::Void);
    let opaque_type = ctx.define_type(Type::Opaque);
    let bool_type = ctx.define_type(Type::Bool);

    let builtin_scope = Scope::new(IndexMap::from([
        (ctx.define_symbol("i8"), i8_type.into()),
        (ctx.define_symbol("i16"), i16_type.into()),
        (ctx.define_symbol("i32"), i32_type.into()),
        (ctx.define_symbol("i64"), i64_type.into()),
        (ctx.define_symbol("isize"), isize_type.into()),
        (ctx.define_symbol("u8"), u8_type.into()),
        (ctx.define_symbol("u16"), u16_type.into()),
        (ctx.define_symbol("u32"), u32_type.into()),
        (ctx.define_symbol("u64"), u64_type.into()),
        (ctx.define_symbol("f32"), f32_type.into()),
        (ctx.define_symbol("f64"), f64_type.into()),
        (ctx.define_symbol("usize"), usize_type.into()),
        (ctx.define_symbol("void"), void_type.into()),
        (ctx.define_symbol("opaque"), opaque_type.into()),
        (ctx.define_symbol("bool"), bool_type.into()),
    ]));

    builtin_scope
}

fn generate_type_body<'a, E: ErrorReporter>(ctx: &Context<'a, E>) {
    for scopes in ctx.scopes.values() {
        for (_, type_object) in scopes.type_scopes.iter() {
            let ty = type_object.ty;
            let Type::Struct(struct_type) = ty.as_ref() else {
                continue;
            };

            let typeargs = ctx.define_typeargs(
                struct_type
                    .type_params
                    .iter()
                    .cloned()
                    .map(Type::TypeArg)
                    .map(|ty| ctx.define_type(ty))
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
            struct_type.monomorphize(ctx, ty, typeargs);
        }
    }
}

fn monomorphize_types<'a, E: ErrorReporter>(ctx: &Context<'a, E>) {
    for scopes in ctx.scopes.values() {
        for (_, type_object) in scopes.type_scopes.iter() {
            let ty = type_object.ty;
            let Type::Struct(struct_type) = ty.as_ref() else {
                continue;
            };

            let body = struct_type.body.get().expect("missing struct body");
            for ty in body.fields.iter().filter_map(|(_, ty)| ty.as_inst()) {
                let generic_ty = ctx
                    .scopes
                    .get(&ty.def_id.package)
                    .expect("missing package scope")
                    .type_scopes
                    .lookup(ty.def_id.name)
                    .expect("missing type");
                generic_ty.monomorphize(ctx, generic_ty.ty, ty.type_args);
                assert!(ty.body.get().is_some());
            }
        }
    }
}

fn build_value_scopes<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: IndexMap<Symbol<'a>, Vec<ItemNode>>,
) -> IndexMap<Symbol<'a>, Scope<'a, ValueObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<ValueObject<'a>>>::default();

    for (package_name, items) in package_asts {
        let mut table = IndexMap::<Symbol, ValueObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        let scopes = ctx
            .scopes
            .get(&package_name)
            .expect("missing package scope");

        for item in items {
            let object_name = ctx.define_symbol(item.name());
            let def_id = DefId {
                package: package_name,
                name: object_name.clone(),
            };

            let pos = item.pos();
            if let Some(declared_at) = object_pos.get(&def_id) {
                let declared_at = ctx.files.location(*declared_at);
                ctx.errors.redeclared_symbol(pos, declared_at, &object_name);
                continue;
            }
            object_pos.insert(def_id.clone(), pos);

            let object = match item {
                ItemNode::Global(node) => {
                    let annotations: Rc<[Annotation]> =
                        build_annotations_from_node(ctx, &node.annotations).into();

                    let ty = get_type_from_node(ctx, scopes, &node.ty);
                    ValueObject::Global(GlobalObject {
                        def_id,
                        ty,
                        node,
                        value: OnceCell::default(),
                        annotations,
                    })
                }
                ItemNode::Function(func_node) => {
                    let annotations: Rc<[Annotation]> =
                        build_annotations_from_node(ctx, &func_node.signature.annotations).into();

                    let type_params = get_typeparams(ctx, &func_node.signature.type_params);

                    let func_type = get_func_type_from_signature(
                        ctx,
                        &scopes,
                        &type_params,
                        &func_node.signature,
                    );
                    let ty = ctx.define_type(Type::Func(func_type));

                    ValueObject::Func(FuncObject {
                        def_id,
                        type_params,
                        ty,
                        node: func_node,
                        body: OnceCell::default(),
                        annotations,
                        monomorphized: OnceCell::default(),
                    })
                }
                _ => unreachable!(),
            };

            table.insert(object_name, object);
        }

        let scope = Scope::new(table);
        package_scopes.insert(package_name, scope);
    }

    package_scopes
}

fn build_annotations_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    nodes: &[AnnotationNode],
) -> Vec<Annotation> {
    let mut annotations = Vec::default();
    for annotation_node in nodes {
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

fn generate_global_value<'a, E: ErrorReporter>(ctx: &Context<'a, E>) {
    for scope in ctx.scopes.values() {
        for (_, value_object) in scope.value_scopes.iter() {
            let ValueObject::Global(global_object) = value_object else {
                continue;
            };

            let ty = global_object.ty;
            let value_expr = if let Some(ref expr) = global_object.node.value {
                get_expr_from_node(ctx, scope, Some(ty), expr)
            } else {
                Expr {
                    ty,
                    kind: ExprKind::Zero,
                    assignable: false,
                }
            };

            if !ty.as_ref().is_assignable_with(value_expr.ty.as_ref()) {
                let pos = global_object
                    .node
                    .value
                    .as_ref()
                    .map(|expr| expr.pos())
                    .unwrap_or(global_object.node.pos);
                ctx.errors.type_mismatch(pos, ty.as_ref(), value_expr.ty);
            }

            let value = ctx.define_expr(value_expr);
            global_object
                .value
                .set(value)
                .expect("cannot set global value expression");
        }
    }
}

fn generate_func_bodies<'a, E: ErrorReporter>(ctx: &Context<'a, E>) {
    for scope in ctx.scopes.values() {
        for (_, value_object) in scope.value_scopes.iter() {
            let ValueObject::Func(func_object) = value_object else {
                continue;
            };

            let body = get_func_body(ctx, scope, func_object);
            let body = ctx.define_statement(body);
            func_object.body.set(body).expect("cannot set func body");
        }
    }
}

fn get_func_body<'a, 'b, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &'b Scopes<'a>,
    func_object: &FuncObject<'a>,
) -> Statement<'a> {
    let Some(ref body) = func_object.node.body else {
        return Statement::Native;
    };

    let func_type = func_object.ty.as_func().expect("not a function");

    let scope = get_typeparam_scope(ctx, scope, &func_object.type_params);

    let mut symbol_table = IndexMap::default();
    let mut last_unused_local = 0;
    for (i, param) in func_object.node.signature.parameters.iter().enumerate() {
        let name = ctx.define_symbol(&param.name.value);
        if symbol_table.contains_key(&name) {
            continue;
        }
        let ty = func_type.params[i];
        symbol_table.insert(
            name,
            ValueObject::Local(LocalObject {
                id: last_unused_local,
                ty,
                name,
            }),
        );
        last_unused_local += 1;
    }
    let new_scope = scope.value_scopes.new_child(symbol_table);
    let scope = scope.with_value_scope(new_scope);

    let return_type = func_type.return_type;
    let stmt_ctx = StatementContext::new(ctx, &scope, last_unused_local, return_type);
    let result = get_statement_from_block(&stmt_ctx, body);

    let should_return = return_type != ctx.define_type(Type::Void);
    if should_return && !result.is_returning {
        ctx.errors.missing_return(func_object.node.pos);
    }

    result.statement
}

fn monomorphize_statements<'a, E: ErrorReporter>(ctx: &Context<'a, E>) {
    let monomorphized_funcs = get_all_monomorphized_funcs(ctx);

    for (def_id, all_typeargs) in monomorphized_funcs {
        let generic_func = ctx
            .scopes
            .get(&def_id.package)
            .expect("package scope not found")
            .value_scopes
            .lookup(def_id.name)
            .expect("missing object");
        let ValueObject::Func(generic_func) = generic_func else {
            unreachable!("not a generic func");
        };
        assert!(!generic_func.type_params.is_empty());

        let mut monomorphized = Vec::<(InternTypeArgs, InternType, InternStatement)>::default();
        for typeargs in all_typeargs {
            let body = generic_func.body.get().expect("missing func body");
            let ty = generic_func.ty.monomorphize(ctx, generic_func.ty, typeargs);
            let monomorphized_body = body.monomorphize(ctx, typeargs);
            let monomorphized_body = ctx.define_statement(monomorphized_body);
            monomorphized.push((typeargs, ty, monomorphized_body));
        }

        generic_func
            .monomorphized
            .set(monomorphized)
            .expect("cannot set monomorphized functions");
    }
}

fn get_all_monomorphized_funcs<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
) -> Vec<(DefId<'a>, Vec<InternTypeArgs<'a>>)> {
    #[derive(Debug)]
    enum Source<'a, 'b> {
        Expr(&'b Expr<'a>, InternTypeArgs<'a>),
        Statement(&'b Statement<'a>, InternTypeArgs<'a>),
        FuncInst(DefId<'a>, InternTypeArgs<'a>),
    }

    let empty_typeargs = ctx.define_typeargs(&[]);

    let mut queue = VecDeque::<Source>::default();
    for scope in ctx.scopes.values() {
        for (_, value_object) in scope.value_scopes.iter() {
            match value_object {
                ValueObject::Func(func_object) => {
                    if func_object.type_params.is_empty() {
                        let body = func_object.body.get().expect("missing func body");
                        queue.push_back(Source::Statement(body, empty_typeargs));
                    }
                }
                ValueObject::Global(global_object) => {
                    let value = global_object
                        .value
                        .get()
                        .expect("missing global value expr");
                    queue.push_back(Source::Expr(value, empty_typeargs));
                }
                _ => continue,
            }
        }
    }

    let empty_scope = Scopes::default();
    let mut monomorphized_funcs = IndexMap::<DefId, Vec<InternTypeArgs>>::default();
    let mut func_insts = HashSet::<(DefId, InternTypeArgs)>::default();
    while let Some(item) = queue.pop_front() {
        match item {
            Source::Expr(expr, type_args) => match &expr.kind {
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
                        queue.push_back(Source::Expr(val, type_args))
                    }
                }
                ExprKind::FuncInst(def_id, inner_typeargs) => {
                    let substituted_typeargs = inner_typeargs
                        .iter()
                        .map(|ty| ty.monomorphize(ctx, *ty, type_args))
                        .collect::<Vec<_>>();
                    let substituted_typeargs = ctx.define_typeargs(&substituted_typeargs);
                    queue.push_back(Source::FuncInst(*def_id, substituted_typeargs));
                }
                ExprKind::GetElement(expr, _) => queue.push_back(Source::Expr(expr, type_args)),
                ExprKind::GetElementAddr(expr, _) => queue.push_back(Source::Expr(expr, type_args)),
                ExprKind::GetIndex(arr, index) => {
                    queue.push_back(Source::Expr(arr, type_args));
                    queue.push_back(Source::Expr(index, type_args));
                }
                ExprKind::Deref(value) => {
                    queue.push_back(Source::Expr(value, type_args));
                }
                ExprKind::Call(callee, args) => {
                    queue.push_back(Source::Expr(callee, type_args));
                    for arg in args {
                        queue.push_back(Source::Expr(arg, type_args));
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
                    queue.push_back(Source::Expr(a, type_args));
                    queue.push_back(Source::Expr(b, type_args));
                }
                ExprKind::Neg(value)
                | ExprKind::BitNot(value)
                | ExprKind::Not(value)
                | ExprKind::Cast(value, _) => queue.push_back(Source::Expr(value, type_args)),
            },
            Source::Statement(stmt, type_args) => match stmt {
                Statement::NewLocal(_, expr) => queue.push_back(Source::Expr(expr, type_args)),
                Statement::Block(stmts) => {
                    for stmt in stmts {
                        queue.push_back(Source::Statement(stmt, type_args));
                    }
                }
                Statement::If(if_stmt) => {
                    queue.push_back(Source::Expr(&if_stmt.cond, type_args));
                    queue.push_back(Source::Statement(&if_stmt.body, type_args));
                    if let Some(else_stmt) = &if_stmt.else_stmt {
                        queue.push_back(Source::Statement(else_stmt.as_ref(), type_args));
                    }
                }
                Statement::While(while_stmt) => {
                    queue.push_back(Source::Expr(&while_stmt.cond, type_args));
                    queue.push_back(Source::Statement(&while_stmt.body, type_args));
                }
                Statement::Return(value) => {
                    if let Some(value) = value {
                        queue.push_back(Source::Expr(value, type_args));
                    }
                }
                Statement::Expr(expr) => {
                    queue.push_back(Source::Expr(expr, type_args));
                }
                Statement::Assign(target, value) => {
                    queue.push_back(Source::Expr(target, type_args));
                    queue.push_back(Source::Expr(value, type_args));
                }
                Statement::Native | Statement::Continue | Statement::Break => continue,
            },
            Source::FuncInst(def_id, typeargs) => {
                if func_insts.contains(&(def_id, typeargs)) {
                    continue;
                }
                func_insts.insert((def_id, typeargs));

                let generic_func = ctx
                    .scopes
                    .get(&def_id.package)
                    .unwrap_or(&empty_scope)
                    .value_scopes
                    .lookup(def_id.name)
                    .expect("missing object");
                let ValueObject::Func(generic_func) = generic_func else {
                    unreachable!("not a generic func");
                };
                assert!(!generic_func.type_params.is_empty());

                queue.push_back(Source::Statement(
                    generic_func.body.get().expect("missing body"),
                    typeargs,
                ));

                monomorphized_funcs
                    .entry(def_id)
                    .or_default()
                    .push(typeargs);
            }
        }
    }

    monomorphized_funcs.into_iter().collect()
}

fn build_module<'a, 'b, E>(ctx: &'b Context<'a, E>, is_valid: bool) -> Module<'a> {
    let mut packages = Vec::default();
    for (name, scope) in ctx.scopes.iter() {
        let mut globals = Vec::default();
        let mut functions = Vec::default();
        for (_, value_object) in scope.value_scopes.iter() {
            match value_object {
                ValueObject::Global(global_object) => globals.push(Global {
                    name: global_object.def_id,
                    ty: global_object.ty,
                    value: *global_object
                        .value
                        .get()
                        .expect("missing global value expr"),
                    annotations: global_object.annotations.clone(),
                }),
                ValueObject::Func(func_object) => {
                    if func_object.type_params.is_empty() {
                        functions.push(Func {
                            name: func_object.def_id,
                            typeargs: None,
                            ty: func_object.ty,
                            statement: *func_object.body.get().expect("missing function body"),
                            annotations: func_object.annotations.clone(),
                        });
                    } else {
                        let empty = &Vec::default();
                        let monomorphized = func_object.monomorphized.get().unwrap_or(empty);
                        for (typeargs, ty, body) in monomorphized {
                            functions.push(Func {
                                name: func_object.def_id,
                                typeargs: Some(*typeargs),
                                ty: *ty,
                                statement: *body,
                                annotations: func_object.annotations.clone(),
                            });
                        }
                    }
                }
                _ => continue,
            }
        }
        packages.push(Package {
            name: *name,
            globals,
            functions,
        })
    }

    Module { packages, is_valid }
}