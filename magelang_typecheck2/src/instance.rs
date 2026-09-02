//! Instances of generic definitions. This is the whole generics machinery.
//!
//! An instance is a definition applied to interned type arguments. Its content is
//! obtained by resolving the definition's syntax again with the type parameters bound to
//! the arguments (`resolve.rs`), never by substituting into previously checked output.
//!
//! Invariants:
//! 1. `(def_id, type_args)` maps to at most one instance. Type arguments are interned,
//!    so identity is pointer identity.
//! 2. A struct instance is cached *before* its body is computed. The only way to observe
//!    an instance without a body is a by-value reference to itself from inside its own
//!    body — an infinite-size type, which `cycle.rs` diagnoses. Expression checking never
//!    runs during body computation, so it always sees complete bodies.
//! 3. Instances whose type arguments mention a `Param` are artifacts of checking a
//!    generic definition against its own parameters. They are never queued for body
//!    checking and never emitted. Requests made *while* a definition is being checked are
//!    not queued either; a concrete body that needs the instance queues it later.
//! 4. Instantiation depth is capped so that polymorphic recursion such as
//!    `fn f<T>() { f::<Wrap<T>>(); }` ends in a diagnostic instead of a loop.

use crate::analyze::Context;
use crate::def::{FuncDef, StructDef};
use crate::errors::SemanticError;
use crate::resolve::{resolve_signature, resolve_type};
use crate::scope::Scopes;
use crate::statement::Statement;
use crate::ty::{StructBody, StructType, Type, TypeArgs, TypeKind, TypeParam, TypeRepr};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, Pos, StructNode};
use std::cell::{Cell, OnceCell, RefCell};
use std::collections::{HashMap, VecDeque};

const MAX_INSTANTIATION_DEPTH: usize = 64;

pub(crate) struct FuncInstance<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) type_args: &'a TypeArgs<'a>,
    /// The function type with the type parameters replaced by `type_args`.
    pub(crate) ty: &'a Type<'a>,
    /// Phase 5. Never set for instances with `Param` type arguments.
    pub(crate) body: OnceCell<&'a Statement<'a>>,
    queued: Cell<bool>,
}

#[derive(Default)]
pub(crate) struct Instances<'a> {
    structs: RefCell<IndexMap<(DefId<'a>, &'a TypeArgs<'a>), &'a Type<'a>>>,
    funcs: RefCell<IndexMap<(DefId<'a>, &'a TypeArgs<'a>), &'a FuncInstance<'a>>>,
    pending: RefCell<VecDeque<&'a FuncInstance<'a>>>,
    in_definition_check: Cell<bool>,
}

impl<'a> Instances<'a> {
    pub(crate) fn set_definition_check(&self, on: bool) {
        self.in_definition_check.set(on);
    }

    pub(crate) fn get_func(
        &self,
        def_id: DefId<'a>,
        type_args: &'a TypeArgs<'a>,
    ) -> Option<&'a FuncInstance<'a>> {
        self.funcs.borrow().get(&(def_id, type_args)).copied()
    }

    pub(crate) fn next_pending(&self) -> Option<&'a FuncInstance<'a>> {
        self.pending.borrow_mut().pop_front()
    }

    /// Every struct instance, in creation order.
    pub(crate) fn struct_instances(&self) -> Vec<&'a Type<'a>> {
        self.structs.borrow().values().copied().collect()
    }

    /// Every function instance, in creation order.
    pub(crate) fn func_instances(&self) -> Vec<&'a FuncInstance<'a>> {
        self.funcs.borrow().values().copied().collect()
    }
}

/// The type arguments that stand for a definition's own parameters.
pub(crate) fn identity_args<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    type_params: &[Symbol<'a>],
) -> &'a TypeArgs<'a> {
    let params = type_params
        .iter()
        .enumerate()
        .map(|(index, name)| {
            ctx.define_type(Type::anonymous(TypeRepr::Param(TypeParam {
                index,
                name,
            })))
        })
        .collect::<Vec<_>>();
    ctx.define_typeargs(&params)
}

fn is_too_deep(type_args: &TypeArgs<'_>) -> bool {
    type_args
        .iter()
        .any(|ty| ty.depth() >= MAX_INSTANTIATION_DEPTH)
}

pub(crate) fn struct_instance<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    def: &StructDef<'a>,
    type_args: &'a TypeArgs<'a>,
    pos: Pos,
) -> &'a Type<'a> {
    let key = (def.def_id, type_args);
    if let Some(ty) = ctx.instances.structs.borrow().get(&key) {
        return ty;
    }
    if is_too_deep(type_args) {
        ctx.errors
            .instantiation_too_deep(pos, MAX_INSTANTIATION_DEPTH);
        return ctx.unknown_type();
    }

    let ty = ctx.define_type(Type {
        kind: TypeKind::Named {
            def_id: def.def_id,
            type_args,
        },
        repr: TypeRepr::Struct(StructType {
            body: OnceCell::new(),
        }),
    });
    // Cache before computing the body: the fields may refer back to this instance.
    ctx.instances.structs.borrow_mut().insert(key, ty);

    let scope = ctx
        .package_scope(def.def_id.package)
        .bind_type_params(def.type_params, type_args);
    let body = build_struct_body(ctx, &scope, &def.node);
    ty.as_struct()
        .expect("struct instances have a struct repr")
        .body
        .set(body)
        .expect("the body of a new instance is set exactly once");
    ty
}

fn build_struct_body<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &StructNode,
) -> StructBody<'a> {
    let mut declared = HashMap::<Symbol<'a>, Pos>::default();
    let mut fields = IndexMap::<Symbol<'a>, &'a Type<'a>>::default();
    for field in &node.fields {
        let name = ctx.define_symbol(&field.name.value);
        if let Some(declared_at) = declared.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(field.name.pos, declared_at, &field.name.value);
            continue;
        }
        declared.insert(name, field.name.pos);
        fields.insert(name, resolve_type(ctx, scope, &field.ty));
    }
    StructBody { fields }
}

/// Returns `None` only when the instantiation is too deep (already reported).
pub(crate) fn func_instance<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    def: &FuncDef<'a>,
    type_args: &'a TypeArgs<'a>,
    pos: Pos,
) -> Option<&'a FuncInstance<'a>> {
    let key = (def.def_id, type_args);
    let existing = ctx.instances.funcs.borrow().get(&key).copied();
    let inst = match existing {
        Some(inst) => inst,
        None => {
            if is_too_deep(type_args) {
                ctx.errors
                    .instantiation_too_deep(pos, MAX_INSTANTIATION_DEPTH);
                return None;
            }
            let scope = ctx
                .package_scope(def.def_id.package)
                .bind_type_params(def.type_params, type_args);
            let func_type = resolve_signature(ctx, &scope, &def.node.signature);
            let ty = ctx.define_type(Type::anonymous(TypeRepr::Func(func_type)));
            let inst: &'a FuncInstance<'a> = ctx.arena.alloc(FuncInstance {
                def_id: def.def_id,
                type_args,
                ty,
                body: OnceCell::new(),
                queued: Cell::new(false),
            });
            ctx.instances.funcs.borrow_mut().insert(key, inst);
            inst
        }
    };

    let is_concrete = !type_args.iter().any(|ty| ty.has_param());
    if is_concrete && !inst.queued.get() && !ctx.instances.in_definition_check.get() {
        inst.queued.set(true);
        ctx.instances.pending.borrow_mut().push_back(inst);
    }

    Some(inst)
}
