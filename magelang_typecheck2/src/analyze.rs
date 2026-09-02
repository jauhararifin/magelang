//! The driver: runs the phases in order (see `lib.rs`) and assembles the `Module`.

use crate::cycle::check_infinite_structs;
use crate::def::{self, Definitions};
use crate::errors::SemanticError;
use crate::expr::{get_expr_from_node, Expr, ExprKind};
use crate::func::check_body;
use crate::global_init::compute_init_order;
use crate::instance::{func_instance, identity_args, struct_instance, FuncInstance, Instances};
use crate::interner::Interner;
use crate::loader::{check_circular_imports, load_packages};
use crate::path::get_stdlib_path;
use crate::resolve::resolve_type;
use crate::scope::Scopes;
use crate::statement::Statement;
use crate::ty::{BitSize, FloatType, Type, TypeArgs, TypeRepr};
use crate::{DefId, Func, Global, Module, Package, Symbol, SymbolInterner};
use bumpalo::Bump;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, FileManager, Pos};
use std::cell::Cell;

pub fn analyze<'a>(
    arena: &'a Bump,
    file_manager: &mut FileManager,
    error_manager: &impl ErrorReporter,
    main_package: &str,
) -> Module<'a> {
    // Phase 1: load.
    let symbols = SymbolInterner::new(arena);
    let stdlib_path = get_stdlib_path();
    let main_package = symbols.define(main_package);
    let package_asts = load_packages(
        file_manager,
        error_manager,
        &symbols,
        &stdlib_path,
        main_package,
    );
    check_circular_imports(error_manager, &symbols, &package_asts);

    let mut ctx = Context {
        arena,
        files: file_manager,
        errors: Errors::new(error_manager),
        interners: Interners {
            symbols,
            types: Interner::new(arena),
            typeargs: Interner::new(arena),
        },
        defs: Definitions::default(),
        instances: Instances::default(),
    };

    // Phase 2: declare.
    ctx.defs = def::declare(&ctx, package_asts);
    // Phase 3: signatures.
    resolve_signatures(&ctx);
    // Phase 4: definition bodies.
    check_definitions(&ctx);
    // Phase 5: instantiate.
    instantiate_pending(&ctx);
    // Phase 6: validate.
    check_infinite_structs(&ctx);
    let global_init_order = compute_init_order(&ctx);

    // Phase 7: build.
    let is_valid = !ctx.errors.has_errors();
    build_module(ctx, is_valid, global_init_order)
}

/// Forwards to the caller's error reporter while counting reports, so a phase can tell
/// whether the piece of code it just checked produced errors.
pub(crate) struct Errors<'syn, E> {
    inner: &'syn E,
    count: Cell<usize>,
}

impl<'syn, E> Errors<'syn, E> {
    fn new(inner: &'syn E) -> Self {
        Self {
            inner,
            count: Cell::new(0),
        }
    }

    pub(crate) fn count(&self) -> usize {
        self.count.get()
    }
}

impl<'syn, E: ErrorReporter> ErrorReporter for Errors<'syn, E> {
    fn report(&self, pos: Pos, message: String) {
        self.count.set(self.count.get() + 1);
        self.inner.report(pos, message);
    }

    fn has_errors(&self) -> bool {
        self.inner.has_errors()
    }
}

/// Everything the phases share. `'a` is the arena lifetime of all produced data; `'syn`
/// is the (shorter) lifetime of the file and error managers, kept separate so that the
/// produced `Module` doesn't borrow them.
pub(crate) struct Context<'a, 'syn, E> {
    pub(crate) arena: &'a Bump,
    pub(crate) files: &'syn FileManager,
    pub(crate) errors: Errors<'syn, E>,
    pub(crate) interners: Interners<'a>,
    pub(crate) defs: Definitions<'a>,
    pub(crate) instances: Instances<'a>,
}

pub(crate) struct Interners<'a> {
    symbols: SymbolInterner<'a>,
    types: Interner<'a, Type<'a>>,
    typeargs: Interner<'a, TypeArgs<'a>>,
}

impl<'a, 'syn, E> Context<'a, 'syn, E> {
    pub(crate) fn define_symbol(&self, symbol: &str) -> Symbol<'a> {
        self.interners.symbols.define(symbol)
    }

    pub(crate) fn define_type(&self, ty: Type<'a>) -> &'a Type<'a> {
        self.interners.types.define(ty)
    }

    pub(crate) fn define_typeargs(&self, type_args: &TypeArgs<'a>) -> &'a TypeArgs<'a> {
        self.interners.typeargs.define(type_args)
    }

    pub(crate) fn anon(&self, repr: TypeRepr<'a>) -> &'a Type<'a> {
        self.define_type(Type::anonymous(repr))
    }

    pub(crate) fn unknown_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Unknown)
    }

    pub(crate) fn void_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Void)
    }

    pub(crate) fn bool_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Bool)
    }

    pub(crate) fn opaque_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Opaque)
    }

    pub(crate) fn isize_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Int(true, BitSize::ISize))
    }

    pub(crate) fn u8_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Int(false, BitSize::I8))
    }

    pub(crate) fn f64_type(&self) -> &'a Type<'a> {
        self.anon(TypeRepr::Float(FloatType::F64))
    }

    pub(crate) fn package_scope(&self, package: Symbol<'a>) -> &Scopes<'a> {
        self.defs
            .package_scope(package)
            .expect("every loaded package has a scope")
    }
}

/// Phase 3: the declared type of every global, the signature of every function under
/// its own type parameters, and the identity instance of every struct (which resolves
/// the field types once and reports their errors).
fn resolve_signatures<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) {
    for def in ctx.defs.globals.values() {
        let scope = ctx.package_scope(def.def_id.package);
        let ty = resolve_type(ctx, scope, &def.node.ty);
        def.ty.set(ty).expect("global types are resolved once");
    }

    for def in ctx.defs.structs.values() {
        let type_args = identity_args(ctx, def.type_params);
        let ty = struct_instance(ctx, def, type_args, def.pos);
        def.identity
            .set(ty)
            .expect("struct identities are resolved once");
    }

    for def in ctx.defs.funcs.values() {
        let type_args = identity_args(ctx, def.type_params);
        let inst = func_instance(ctx, def, type_args, def.pos)
            .expect("identity type arguments are never too deep");
        def.sig
            .set(inst.ty)
            .expect("function signatures are resolved once");
    }
}

/// Phase 4: global initializers, and the bodies of generic functions checked against
/// their own type parameters. Non-generic functions were queued by phase 3 and are
/// checked in phase 5 like any other concrete instance.
fn check_definitions<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) {
    for def in ctx.defs.globals.values() {
        let scope = ctx.package_scope(def.def_id.package);
        let ty = *def.ty.get().expect("global types are resolved in phase 3");

        let value = match &def.node.value {
            Some(expr) => get_expr_from_node(ctx, scope, Some(ty), expr),
            None => Expr {
                ty,
                kind: ExprKind::Zero,
                pos: def.node.pos,
                assignable: false,
            },
        };

        if !ty.is_assignable_with(value.ty) {
            let pos = def
                .node
                .value
                .as_ref()
                .map(|expr| expr.pos())
                .unwrap_or(def.node.pos);
            ctx.errors.type_mismatch(pos, ty, value.ty);
        }

        def.value.set(value).expect("global values are checked once");
    }

    for def in ctx.defs.funcs.values() {
        if def.type_params.is_empty() {
            continue;
        }
        let type_args = identity_args(ctx, def.type_params);
        let inst = ctx
            .instances
            .get_func(def.def_id, type_args)
            .expect("identity instances are created in phase 3");

        let errors_before = ctx.errors.count();
        ctx.instances.set_definition_check(true);
        let _ = check_body(ctx, def, inst);
        ctx.instances.set_definition_check(false);
        def.body_ok.set(ctx.errors.count() == errors_before);
    }
}

/// Phase 5: check the body of every concrete function instance. Checking a body may
/// request further instances, which are appended to the queue.
fn instantiate_pending<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) {
    while let Some(inst) = ctx.instances.next_pending() {
        let def = ctx.defs.func_def(inst.def_id);
        let body = if def.type_params.is_empty() || def.body_ok.get() {
            check_body(ctx, def, inst)
        } else {
            // The definition itself failed to check; instantiating it would only repeat
            // the errors with the type arguments filled in.
            Statement::Native
        };
        let body: &'a Statement<'a> = ctx.arena.alloc(body);
        inst.body
            .set(body)
            .expect("an instance body is checked once");
    }
}

/// Phase 7. Packages appear in load order; globals and functions in declaration order,
/// with the instances of a generic function in creation order.
fn build_module<'a, E>(
    ctx: Context<'a, '_, E>,
    is_valid: bool,
    global_init_order: Vec<DefId<'a>>,
) -> Module<'a> {
    let Context {
        defs, instances, ..
    } = ctx;

    let mut instances_by_def = IndexMap::<DefId<'a>, Vec<&'a FuncInstance<'a>>>::default();
    for inst in instances.func_instances() {
        instances_by_def
            .entry(inst.def_id)
            .or_default()
            .push(inst);
    }

    let mut globals_by_package = IndexMap::<Symbol<'a>, Vec<Global<'a>>>::default();
    for (def_id, mut def) in defs.globals {
        let value = def.value.take().expect("global values are checked in phase 4");
        globals_by_package
            .entry(def_id.package)
            .or_default()
            .push(Global {
                name: def_id,
                ty: def.ty.get().expect("global types are resolved in phase 3"),
                value,
                annotations: def.annotations.clone(),
            });
    }

    let mut functions_by_package = IndexMap::<Symbol<'a>, Vec<Func<'a>>>::default();
    for (def_id, def) in &defs.funcs {
        let Some(insts) = instances_by_def.get(def_id) else {
            continue;
        };
        for inst in insts {
            // Instances with `Param` type arguments have no body and are not emitted.
            let Some(statement) = inst.body.get() else {
                continue;
            };
            functions_by_package
                .entry(def_id.package)
                .or_default()
                .push(Func {
                    name: *def_id,
                    pos: def.pos,
                    typeargs: (!def.type_params.is_empty()).then_some(inst.type_args),
                    ty: inst.ty,
                    statement,
                    annotations: def.annotations.clone(),
                });
        }
    }

    let packages = defs
        .packages
        .keys()
        .map(|name| Package {
            name,
            globals: globals_by_package.swap_remove(name).unwrap_or_default(),
            functions: functions_by_package.swap_remove(name).unwrap_or_default(),
        })
        .collect();

    Module {
        is_valid,
        packages,
        global_init_order,
    }
}
