//! Turning syntax into types: type expressions, paths and function signatures, always
//! relative to a scope. Binding a generic definition's parameters in the scope is all it
//! takes to resolve the definition's syntax for one particular instance.

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::instance::struct_instance;
use crate::scope::{Scope, Scopes, TypeEntry, ValueEntry};
use crate::ty::{FuncType, Type, TypeArgs, TypeRepr};
use crate::Symbol;
use bumpalo::collections::Vec as BumpVec;
use magelang_syntax::{ErrorReporter, PathName, PathNode, Pos, SignatureNode, TypeExprNode};
use std::collections::HashMap;

pub(crate) fn resolve_type<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &TypeExprNode,
) -> &'a Type<'a> {
    match node {
        TypeExprNode::Invalid(..) => ctx.unknown_type(),
        TypeExprNode::Path(node) => resolve_type_path(ctx, scope, node),
        TypeExprNode::Ptr(node) => {
            let element = resolve_type(ctx, scope, &node.ty);
            ctx.define_type(Type::anonymous(TypeRepr::Ptr(element)))
        }
        TypeExprNode::ArrayPtr(node) => {
            let element = resolve_type(ctx, scope, &node.ty);
            ctx.define_type(Type::anonymous(TypeRepr::ArrayPtr(element)))
        }
        TypeExprNode::Func(node) => {
            let mut params = BumpVec::with_capacity_in(node.params.len(), ctx.arena);
            for param in &node.params {
                params.push(resolve_type(ctx, scope, &param.ty));
            }
            let return_type = match &node.return_type {
                Some(ty) => resolve_type(ctx, scope, ty),
                None => ctx.void_type(),
            };
            ctx.define_type(Type::anonymous(TypeRepr::Func(FuncType {
                params: params.into_bump_slice(),
                return_type,
            })))
        }
        TypeExprNode::Grouped(node) => resolve_type(ctx, scope, node),
    }
}

fn resolve_type_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    node: &PathNode,
) -> &'a Type<'a> {
    let Some(entry) = lookup_type(ctx, scope, &node.path) else {
        return ctx.unknown_type();
    };

    match entry {
        TypeEntry::Type(ty) => {
            if !node.args.is_empty() {
                ctx.errors.non_generic_value(node.pos());
            }
            ty
        }
        TypeEntry::Struct(def_id) => {
            let def = ctx.defs.struct_def(def_id);
            if def.type_params.is_empty() && !node.args.is_empty() {
                ctx.errors.non_generic_value(node.pos());
            }
            let type_args =
                resolve_type_args(ctx, scope, def.type_params.len(), &node.args, node.pos());
            struct_instance(ctx, def, type_args, node.pos())
        }
    }
}

/// Resolves the type arguments written at a use of a generic definition. A wrong count is
/// reported; missing arguments become `Unknown` and extra ones are dropped, so the
/// caller always gets exactly `expected` arguments.
pub(crate) fn resolve_type_args<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    expected: usize,
    nodes: &[TypeExprNode],
    pos: Pos,
) -> &'a TypeArgs<'a> {
    let mut type_args = nodes
        .iter()
        .map(|node| resolve_type(ctx, scope, node))
        .collect::<Vec<_>>();

    if type_args.len() != expected {
        ctx.errors
            .type_arguments_count_mismatch(pos, expected, type_args.len());
    }
    type_args.resize_with(expected, || ctx.unknown_type());

    ctx.define_typeargs(&type_args)
}

/// Resolves a function signature. Duplicate parameter names are reported here; they are
/// still counted so that parameter positions stay aligned with the function type.
pub(crate) fn resolve_signature<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    signature: &SignatureNode,
) -> FuncType<'a> {
    let mut declared = HashMap::<Symbol<'a>, Pos>::default();
    let mut params = BumpVec::with_capacity_in(signature.parameters.len(), ctx.arena);
    for param in &signature.parameters {
        let name = ctx.define_symbol(&param.name.value);
        if let Some(declared_at) = declared.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(param.name.pos, declared_at, &param.name.value);
        } else {
            declared.insert(name, param.name.pos);
        }
        params.push(resolve_type(ctx, scope, &param.ty));
    }

    let return_type = match &signature.return_type {
        Some(ty) => resolve_type(ctx, scope, ty),
        None => ctx.void_type(),
    };

    FuncType {
        params: params.into_bump_slice(),
        return_type,
    }
}

pub(crate) fn lookup_type<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    path: &PathName,
) -> Option<TypeEntry<'a>> {
    lookup(ctx, scope, path, |scopes| &scopes.types)
}

pub(crate) fn lookup_value<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    path: &PathName,
) -> Option<ValueEntry<'a>> {
    lookup(ctx, scope, path, |scopes| &scopes.values)
}

/// Looks a path up in one namespace, reporting undeclared names. A package-qualified path
/// goes through the import table of the current scope into the top-level scope of the
/// imported package.
fn lookup<'a, T: Copy, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    path: &PathName,
    namespace: impl for<'s> Fn(&'s Scopes<'a>) -> &'s Scope<'a, T>,
) -> Option<T> {
    match path {
        PathName::Local(name) => {
            let symbol = ctx.define_symbol(&name.value);
            let entry = namespace(scope).lookup(symbol).copied();
            if entry.is_none() {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
            }
            entry
        }
        PathName::Package { package, name } => {
            let package_symbol = ctx.define_symbol(&package.value);
            let Some(import) = scope.imports.lookup(package_symbol) else {
                ctx.errors.undeclared_symbol(package.pos, &package.value);
                return None;
            };
            let Some(package_scope) = ctx.defs.package_scope(import.package) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };
            let symbol = ctx.define_symbol(&name.value);
            let entry = namespace(package_scope).lookup(symbol).copied();
            if entry.is_none() {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
            }
            entry
        }
        PathName::Invalid(..) => None,
    }
}
