//! Lexical scopes. There are three independent namespaces — imports, types and values —
//! because a struct, a function and an import may all share one name.

use crate::ty::{Type, TypeArgs};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use std::iter::zip;
use std::rc::Rc;

/// A persistent, immutable symbol table with a parent chain. Cloning is cheap.
pub(crate) struct Scope<'a, T> {
    internal: Rc<ScopeInternal<'a, T>>,
}

struct ScopeInternal<'a, T> {
    table: IndexMap<Symbol<'a>, T>,
    parent: Option<Rc<ScopeInternal<'a, T>>>,
}

impl<'a, T> Default for Scope<'a, T> {
    fn default() -> Self {
        Self::new(IndexMap::default())
    }
}

impl<'a, T> Clone for Scope<'a, T> {
    fn clone(&self) -> Self {
        Self {
            internal: self.internal.clone(),
        }
    }
}

impl<'a, T> Scope<'a, T> {
    pub(crate) fn new(table: IndexMap<Symbol<'a>, T>) -> Self {
        Self {
            internal: Rc::new(ScopeInternal {
                table,
                parent: None,
            }),
        }
    }

    pub(crate) fn new_child(&self, table: IndexMap<Symbol<'a>, T>) -> Self {
        Self {
            internal: Rc::new(ScopeInternal {
                table,
                parent: Some(self.internal.clone()),
            }),
        }
    }

    pub(crate) fn lookup(&self, name: Symbol<'a>) -> Option<&T> {
        let mut internal = Some(&self.internal);
        while let Some(s) = internal {
            if let Some(object) = s.table.get(&name) {
                return Some(object);
            }
            internal = s.parent.as_ref();
        }
        None
    }
}

#[derive(Clone, Copy)]
pub(crate) struct ImportEntry<'a> {
    pub(crate) package: Symbol<'a>,
}

#[derive(Clone, Copy)]
pub(crate) enum TypeEntry<'a> {
    /// A builtin type or a bound type parameter.
    Type(&'a Type<'a>),
    /// A struct definition; it becomes a type once applied to its type arguments.
    Struct(DefId<'a>),
}

#[derive(Clone, Copy)]
pub(crate) enum ValueEntry<'a> {
    Func(DefId<'a>),
    Global(DefId<'a>),
    Local { id: usize, ty: &'a Type<'a> },
}

#[derive(Default, Clone)]
pub(crate) struct Scopes<'a> {
    pub(crate) imports: Scope<'a, ImportEntry<'a>>,
    pub(crate) types: Scope<'a, TypeEntry<'a>>,
    pub(crate) values: Scope<'a, ValueEntry<'a>>,
}

impl<'a> Scopes<'a> {
    /// Binds the type parameter names to `type_args`, positionally. When a name is
    /// declared twice (already reported), the first binding wins.
    pub(crate) fn bind_type_params(
        &self,
        names: &[Symbol<'a>],
        type_args: &'a TypeArgs<'a>,
    ) -> Self {
        if names.is_empty() {
            return self.clone();
        }
        let mut table = IndexMap::<Symbol<'a>, TypeEntry<'a>>::default();
        for (name, ty) in zip(names, type_args) {
            table.entry(*name).or_insert(TypeEntry::Type(ty));
        }
        Self {
            imports: self.imports.clone(),
            types: self.types.new_child(table),
            values: self.values.clone(),
        }
    }

    pub(crate) fn with_locals(&self, table: IndexMap<Symbol<'a>, ValueEntry<'a>>) -> Self {
        Self {
            imports: self.imports.clone(),
            types: self.types.clone(),
            values: self.values.new_child(table),
        }
    }
}
