//! Phase 6: structs that contain themselves by value have infinite size.
//!
//! The check runs over struct *instances*, whose bodies are fully substituted, so a cycle
//! that only exists through a type argument (`struct A<T> { x: T }  struct B { a: A<B> }`)
//! is found as well. Identity instances are visited first, in declaration order, so a
//! cycle among definitions is reported in terms of the definitions. Once a definition is
//! known to be cyclic (the one a cycle was entered through), its other instances are
//! skipped: they would only repeat the error.

use crate::analyze::Context;
use crate::errors::SemanticError;
use crate::ty::Type;
use crate::DefId;
use indexmap::IndexSet;
use magelang_syntax::ErrorReporter;
use std::collections::HashSet;

pub(crate) fn check_infinite_structs<'a, E: ErrorReporter>(ctx: &Context<'a, '_, E>) {
    let mut checker = CycleChecker {
        ctx,
        visited: IndexSet::default(),
        in_chain: IndexSet::default(),
        cyclic_defs: HashSet::default(),
    };

    for def in ctx.defs.structs.values() {
        if let Some(ty) = def.identity.get() {
            checker.visit(ty);
        }
    }
    for ty in ctx.instances.struct_instances() {
        checker.visit(ty);
    }
}

struct CycleChecker<'c, 'a, 'syn, E> {
    ctx: &'c Context<'a, 'syn, E>,
    visited: IndexSet<&'a Type<'a>>,
    in_chain: IndexSet<&'a Type<'a>>,
    cyclic_defs: HashSet<DefId<'a>>,
}

impl<'c, 'a, 'syn, E: ErrorReporter> CycleChecker<'c, 'a, 'syn, E> {
    fn visit(&mut self, ty: &'a Type<'a>) {
        let Some(def_id) = ty.def_id() else {
            return;
        };
        if self.visited.contains(&ty) || self.cyclic_defs.contains(&def_id) {
            return;
        }
        self.visited.insert(ty);
        self.in_chain.insert(ty);

        // Only fields held by value can make the struct infinite; pointers break the chain.
        let fields = ty
            .as_struct()
            .and_then(|struct_type| struct_type.body.get())
            .map(|body| {
                body.fields
                    .values()
                    .copied()
                    .filter(|field_ty| field_ty.def_id().is_some())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        for field_ty in fields {
            if self.in_chain.contains(&field_ty) {
                self.report(field_ty);
            } else if !self.visited.contains(&field_ty) {
                self.visit(field_ty);
            }
        }

        self.in_chain.shift_remove(&ty);
    }

    fn report(&mut self, start: &'a Type<'a>) {
        let chain = self
            .in_chain
            .iter()
            .copied()
            .skip_while(|ty| !std::ptr::eq(*ty, start))
            .collect::<Vec<_>>();
        let names = chain
            .iter()
            .map(|ty| self.display_name(ty))
            .collect::<Vec<_>>();

        let start_def = start.def_id().expect("struct instances are named");
        let pos = self.ctx.defs.struct_def(start_def).pos;
        self.ctx.errors.circular_type(pos, &names);

        // Only the definition the cycle was entered through is marked: its other instances
        // would repeat this error, while the remaining definitions may still take part in
        // different cycles (`struct C { a: A<C> }` next to `struct B { a: A<B> }`).
        self.cyclic_defs.insert(start_def);
    }

    /// An identity instance is displayed as its definition (`main::A`), every other
    /// instance with its type arguments (`main::A::<main::B>`).
    fn display_name(&self, ty: &'a Type<'a>) -> String {
        let def = self
            .ctx
            .defs
            .struct_def(ty.def_id().expect("struct instances are named"));
        if def
            .identity
            .get()
            .is_some_and(|identity| std::ptr::eq(*identity, ty))
        {
            def.def_id.to_string()
        } else {
            ty.to_string()
        }
    }
}
