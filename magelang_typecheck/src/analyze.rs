use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::statement::{get_statement_from_block, Statement, StatementContext};
use crate::ty::{
    check_circular_type, generate_struct_size_info, get_func_type_from_signature,
    get_type_from_node, get_typeparam_scope, get_typeparams, BitSize, FloatType, InternType,
    InternTypeArgs, StructBody, StructType, Type, TypeArg, TypeArgsInterner, TypeInterner,
};
use crate::value::value_from_string_lit;
use crate::{DefId, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{
    parse, AnnotationNode, ErrorReporter, FileManager, FunctionNode, GlobalNode, ItemNode,
    PackageNode, Pos,
};
use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

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
    };

    let import_scopes = build_imports(&ctx, &package_asts);
    ctx.set_import_scope(import_scopes);

    let type_scopes = build_type_scopes(&ctx, &package_asts);
    ctx.set_type_scope(type_scopes);

    generate_type_body(&ctx);
    monomorphize_types(&ctx);
    check_circular_type(&ctx);
    generate_struct_size_info(&ctx);

    let value_scopes = build_value_scopes(&ctx, &package_asts);
    ctx.set_value_scope(value_scopes);

    // TODO: consider materialize all steps done above into a Header IR.
    // This can be useful for incremental compilation.

    generate_global_value(&ctx);
    generate_func_bodies(&ctx);

    // TODO: consider blocking circular import since it makes
    // deciding global initialization harder for incremental
    // compilation.
}

pub(crate) struct Context<'a, E> {
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

pub(crate) struct Interners<'a> {
    symbols: &'a SymbolInterner<'a>,
    types: &'a TypeInterner<'a>,
    typeargs: &'a TypeArgsInterner<'a>,
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

pub(crate) struct ImportObject<'a> {
    pub(crate) package: Symbol<'a>,
}

pub(crate) struct TypeObject<'a> {
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

pub(crate) enum ValueObject<'a> {
    Global(GlobalObject<'a>),
    Func(FuncObject<'a>),
    Local(LocalObject<'a>),
}

pub(crate) struct GlobalObject<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) ty: InternType<'a>,
    pub(crate) node: &'a GlobalNode,
    pub(crate) value: OnceCell<Expr<'a>>,
    pub(crate) annotations: Rc<[Annotation]>,
}

pub(crate) struct Annotation {
    name: String,
    arguments: Vec<String>,
}

pub(crate) struct FuncObject<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) type_params: Vec<TypeArg<'a>>,
    pub(crate) ty: InternType<'a>,
    pub(crate) node: &'a FunctionNode,
    pub(crate) body: OnceCell<Statement<'a>>,
    pub(crate) annotations: Rc<[Annotation]>,
}

pub(crate) struct LocalObject<'a> {
    pub(crate) id: usize,
    pub(crate) ty: InternType<'a>,
    pub(crate) name: Symbol<'a>,
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

fn build_type_scopes<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: &'a IndexMap<Symbol<'a>, PackageNode>,
) -> IndexMap<Symbol<'a>, Scope<'a, TypeObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<TypeObject<'a>>>::default();

    let builtin_scope = get_builtin_scope(ctx);
    for (package_name, package_ast) in package_asts {
        let mut table = IndexMap::<Symbol, TypeObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        for item in &package_ast.items {
            let ItemNode::Struct(struct_node) = item else {
                continue;
            };

            let object_name = ctx.define_symbol(item.name());
            let def_id = DefId {
                package: *package_name,
                name: object_name.clone(),
            };

            let pos = item.pos();
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

            let scope = get_typeparam_scope(ctx, scopes, &struct_type.type_params);

            let mut field_pos = HashMap::<Symbol, Pos>::default();
            let mut fields = IndexMap::<Symbol, InternType<'a>>::default();
            for field_node in &struct_type.node.fields {
                let field_name = ctx.define_symbol(field_node.name.value.as_str());
                let pos = field_node.pos;
                if let Some(defined_at) = field_pos.get(&field_name) {
                    ctx.errors.redeclared_symbol(
                        pos,
                        ctx.files.location(*defined_at),
                        &field_node.name.value,
                    );
                } else {
                    field_pos.insert(field_name.clone(), pos);
                    let ty = get_type_from_node(ctx, &scope, &field_node.ty);
                    fields.insert(field_name, ty.into());
                }
            }
            let struct_body = StructBody {
                fields,
                sized: OnceCell::default(),
            };

            struct_type
                .body
                .set(struct_body)
                .expect("cannot set struct body");
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
            let is_concrete = struct_type.type_params.is_empty();
            if !is_concrete {
                continue;
            }

            let body = struct_type.body.get().expect("missing struct body");
            for ty in body.fields.iter().filter_map(|(_, ty)| ty.as_inst()) {
                let generic_ty = ctx
                    .scopes
                    .get(&ty.def_id.package)
                    .expect("missing package scope")
                    .type_scopes
                    .lookup(ty.def_id.name)
                    .expect("missing type");
                generic_ty.monomorphize(ctx, ty.type_args);
                assert!(ty.body.get().is_some());
            }
        }
    }
}

fn build_value_scopes<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    package_asts: &'a IndexMap<Symbol<'a>, PackageNode>,
) -> IndexMap<Symbol<'a>, Scope<'a, ValueObject<'a>>> {
    let mut package_scopes = IndexMap::<Symbol, Scope<ValueObject<'a>>>::default();

    for (package_name, package_ast) in package_asts {
        let mut table = IndexMap::<Symbol, ValueObject>::default();
        let mut object_pos = HashMap::<DefId, Pos>::default();

        let scopes = ctx.scopes.get(package_name).expect("missing package scope");

        for item in &package_ast.items {
            match item {
                ItemNode::Global(..) | ItemNode::Function(..) => (),
                _ => continue,
            };

            let object_name = ctx.define_symbol(item.name());
            let def_id = DefId {
                package: *package_name,
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
                        node: &func_node,
                        body: OnceCell::default(),
                        annotations,
                    })
                }
                _ => unreachable!(),
            };

            table.insert(object_name, object);
        }

        let scope = Scope::new(table);
        package_scopes.insert(package_name.clone(), scope);
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

            if ty.as_ref().is_assignable_with(value_expr.ty.as_ref()) {
                let pos = global_object
                    .node
                    .value
                    .as_ref()
                    .map(|expr| expr.pos())
                    .unwrap_or(global_object.node.pos);
                ctx.errors.type_mismatch(pos, ty.as_ref(), value_expr.ty);
            }

            global_object
                .value
                .set(value_expr)
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
    let stmt_ctx = StatementContext::new(ctx, &scope, return_type);
    let result = get_statement_from_block(&stmt_ctx, body);

    let should_return = return_type != ctx.define_type(Type::Void);
    if should_return && !result.is_returning {
        ctx.errors.missing_return(func_object.node.pos);
    }

    result.statement
}
