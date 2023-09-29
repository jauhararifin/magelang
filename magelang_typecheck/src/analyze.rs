use crate::errors::SemanticError;
use crate::path::{get_package_path, get_stdlib_path};
use crate::scope::Scope;
use crate::ty::{
    get_type_from_node, BitSize, FloatType, InternType, InternTypeArgs, StructBody, StructType,
    Type, TypeArg, TypeArgsInterner, TypeInterner,
};
use crate::value::value_from_string_lit;
use crate::{DefId, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{parse, ErrorReporter, FileManager, ItemNode, PackageNode, Pos};
use std::cell::{OnceCell, RefCell};
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
    };

    let import_scopes = build_imports(&ctx, &package_asts);
    ctx.set_import_scope(import_scopes);

    let type_scopes = build_type_scopes(&ctx, &package_asts);
    ctx.set_type_scope(type_scopes);

    generate_type_body(&ctx);
    monomorphize_types(&ctx);

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
    fn with_type_scope(&self, type_scopes: Scope<'a, TypeObject<'a>>) -> Self {
        Self {
            import_scopes: self.import_scopes.clone(),
            type_scopes,
            value_scopes: self.value_scopes.clone(),
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
    Func,
    Local,
}

pub(crate) struct GlobalObject<'a> {
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

            let mut type_params = Vec::default();
            let mut param_pos = HashMap::<Symbol, Pos>::default();
            for (i, type_param) in struct_node.type_params.iter().enumerate() {
                let name = ctx.define_symbol(type_param.name.value.as_str());
                type_params.push(TypeArg::new(i, name));
                if let Some(declared_at) = param_pos.get(&name) {
                    let declared_at = ctx.files.location(*declared_at);
                    ctx.errors
                        .redeclared_symbol(type_param.name.pos, declared_at, &name);
                } else {
                    param_pos.insert(name, type_param.name.pos);
                }
            }

            let ty = ctx.define_type(Type::Struct(StructType {
                def_id,
                type_params,
                body: OnceCell::default(),
                sized: OnceCell::default(),
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

            let mut type_param_table = IndexMap::<Symbol, TypeObject>::default();
            for type_param in &struct_type.type_params {
                if !type_param_table.contains_key(&type_param.name) {
                    let ty = ctx.define_type(Type::TypeArg(*type_param));
                    type_param_table.insert(type_param.name.clone(), ty.into());
                }
            }

            let mut scope = scopes.clone();
            if !type_param_table.is_empty() {
                let new_type_scope = scope.type_scopes.new_child(type_param_table);
                scope = scope.with_type_scope(new_type_scope);
            }

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
            let struct_body = StructBody { fields };

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
