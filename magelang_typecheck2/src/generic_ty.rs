use crate::analyze::{Context, Scopes, TypeObject, TypeObjectKind};
use crate::errors::SemanticError;
use crate::interner::Interner;
use crate::ty::{get_type_object_from_path, FuncType, StructType, Type, TypeKind, TypeRepr};
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, PathNode, Pos, TypeExprNode, TypeParameterNode};
use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::hash::Hash;

pub(crate) type GenericTypeInterner<'a> = Interner<'a, GenericType<'a>>;

// TODO: consider creating a new-type for type-args to implement hash, eq, and partial-eq
// to improve performance.
pub type TypeArgs<'a> = [&'a Type<'a>];
pub(crate) type TypeArgsInterner<'a> = Interner<'a, TypeArgs<'a>>;

pub type GenericTypeArgs<'a> = [&'a GenericType<'a>];
pub(crate) type GenericTypeArgsInterner<'a> = Interner<'a, GenericTypeArgs<'a>>;

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct GenericType<'a> {
    pub(crate) kind: GenericTypeKind<'a>,
    pub(crate) params: &'a [TypeArg<'a>],
    pub(crate) repr: GenericTypeRepr<'a>,

    mono_cache: RefCell<HashMap<&'a TypeArgs<'a>, &'a Type<'a>>>,
    generic_mono_cache: RefCell<HashMap<&'a GenericTypeArgs<'a>, &'a GenericType<'a>>>,
}

impl<'a> Hash for GenericType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

impl<'a> GenericType<'a> {
    pub(crate) fn new(
        kind: GenericTypeKind<'a>,
        params: &'a [TypeArg<'a>],
        repr: GenericTypeRepr<'a>,
    ) -> Self {
        Self {
            kind,
            params,
            repr,
            mono_cache: RefCell::default(),
            generic_mono_cache: RefCell::default(),
        }
    }

    pub(crate) fn init_body<E: ErrorReporter>(&'a self, ctx: &Context<'a, E>) {
        let GenericTypeRepr::Struct(struct_type) = &self.repr else {
            return;
        };

        let GenericTypeKind::User(def_id) = self.kind else {
            return;
        };

        let package = def_id.package;
        let package_scope = ctx
            .package_scopes
            .get(package)
            .expect("missing package scope");

        let scope = get_typeparam_scope(ctx, package_scope, self.params);

        let struct_node = ctx
            .package_scopes
            .get(def_id.package)
            .expect("missing package scope")
            .type_scopes
            .lookup(def_id.name)
            .expect("missing object")
            .node
            .as_ref()
            .expect("missing object node");

        let mut field_pos = HashMap::<Symbol, Pos>::default();
        let mut fields = IndexMap::<Symbol, &'a GenericType<'a>>::default();
        for field_node in &struct_node.fields {
            let field_name = ctx.symbols.define(field_node.name.value.as_str());
            let pos = field_node.name.pos;
            if let Some(defined_at) = field_pos.get(&field_name) {
                ctx.errors.redeclared_symbol(
                    pos,
                    ctx.files.location(*defined_at),
                    &field_node.name.value,
                );
            } else {
                field_pos.insert(field_name, pos);
                let generic_ty = get_generic_type_from_node(ctx, &scope, &field_node.ty);
                fields.insert(field_name, generic_ty);
            }
        }

        struct_type
            .body
            .set(GenericStructBody { fields })
            .expect("cannot set struct body");
    }

    pub fn as_struct(&self) -> Option<&GenericStructType<'a>> {
        self.repr.as_struct()
    }

    pub(crate) fn monomorphize_shallow<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        {
            let mut mono_cache = self.mono_cache.borrow_mut();
            if let Some(ty) = mono_cache.get(type_args) {
                return ty;
            }

            if let GenericTypeRepr::Struct(..) = &self.repr {
                let GenericTypeKind::User(def_id) = self.kind else {
                    unreachable!("generic struct must be defined by the user");
                };

                let ty = ctx.types.define(Type {
                    kind: TypeKind::Inst(def_id, type_args),
                    repr: TypeRepr::Struct(StructType {
                        body: OnceCell::default(),
                    }),
                });

                mono_cache.insert(type_args, ty);
                return ty;
            }
        }

        let kind = match self.kind {
            GenericTypeKind::User(def_id) => TypeKind::Inst(def_id, type_args),
            GenericTypeKind::Inst(def_id, generic_typeargs) => {
                let typeargs = BumpVec::from_iter_in(
                    generic_typeargs
                        .iter()
                        .map(|generic_ty| generic_ty.monomorphize_shallow(ctx, type_args)),
                    ctx.arena,
                );
                TypeKind::Inst(def_id, typeargs.into_bump_slice())
            }
            GenericTypeKind::Anonymous => TypeKind::Anonymous,
        };

        let repr = match &self.repr {
            GenericTypeRepr::Unknown => TypeRepr::Unknown,
            GenericTypeRepr::Struct(..) => {
                unreachable!("generic struct already handled specially")
            }
            GenericTypeRepr::Func(func_type) => {
                TypeRepr::Func(func_type.monomorphize(ctx, type_args))
            }
            GenericTypeRepr::Ty(ty) => {
                return ty;
            }
            GenericTypeRepr::Ptr(el) => TypeRepr::Ptr(el.monomorphize_shallow(ctx, type_args)),
            GenericTypeRepr::ArrayPtr(el) => {
                TypeRepr::ArrayPtr(el.monomorphize_shallow(ctx, type_args))
            }
            GenericTypeRepr::TypeArg(arg) => {
                return type_args
                    .get(arg.index)
                    .expect("missing type arg at the index")
            }
        };

        ctx.types.define(Type { kind, repr })
    }

    pub(crate) fn monomorphize_generic_shallow<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a GenericTypeArgs<'a>,
    ) -> &'a GenericType<'a> {
        {
            let mut generic_mono_cache = self.generic_mono_cache.borrow_mut();
            if let Some(ty) = generic_mono_cache.get(type_args) {
                return ty;
            }

            if let GenericTypeRepr::Struct(..) = &self.repr {
                let GenericTypeKind::User(def_id) = self.kind else {
                    unreachable!("generic struct must be defined by the user");
                };

                let generic_ty = ctx.generic_types.define(GenericType::new(
                    GenericTypeKind::Inst(def_id, type_args),
                    &[],
                    GenericTypeRepr::Struct(GenericStructType {
                        body: OnceCell::default(),
                    }),
                ));

                generic_mono_cache.insert(type_args, generic_ty);
                return generic_ty;
            }
        }

        let kind = match self.kind {
            GenericTypeKind::User(def_id) => GenericTypeKind::Inst(def_id, type_args),
            GenericTypeKind::Inst(def_id, generic_typeargs) => {
                let typeargs = BumpVec::from_iter_in(
                    generic_typeargs
                        .iter()
                        .map(|generic_ty| generic_ty.monomorphize_generic_shallow(ctx, type_args)),
                    ctx.arena,
                );
                GenericTypeKind::Inst(def_id, typeargs.into_bump_slice())
            }
            GenericTypeKind::Anonymous => GenericTypeKind::Anonymous,
        };

        let repr = match &self.repr {
            GenericTypeRepr::Unknown => GenericTypeRepr::Unknown,
            GenericTypeRepr::Struct(..) => {
                unreachable!("generic struct already handled specially")
            }
            GenericTypeRepr::Func(func_type) => {
                GenericTypeRepr::Func(func_type.monomorphize_generic(ctx, type_args))
            }
            GenericTypeRepr::Ty(ty) => GenericTypeRepr::Ty(ty),
            GenericTypeRepr::Ptr(el) => {
                GenericTypeRepr::Ptr(el.monomorphize_generic_shallow(ctx, type_args))
            }
            GenericTypeRepr::ArrayPtr(el) => {
                GenericTypeRepr::ArrayPtr(el.monomorphize_generic_shallow(ctx, type_args))
            }
            GenericTypeRepr::TypeArg(arg) => {
                return type_args
                    .get(arg.index)
                    .expect("missing type arg at the index")
            }
        };

        ctx.generic_types.define(GenericType::new(kind, &[], repr))
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum GenericTypeKind<'a> {
    User(DefId<'a>),
    Inst(DefId<'a>, &'a GenericTypeArgs<'a>),
    Anonymous,
}

impl<'a> GenericTypeKind<'a> {
    pub(crate) fn get_def_id(&self) -> Option<DefId<'a>> {
        match self {
            GenericTypeKind::User(def_id) => Some(*def_id),
            GenericTypeKind::Inst(def_id, _) => Some(*def_id),
            GenericTypeKind::Anonymous => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum GenericTypeRepr<'a> {
    Unknown,
    Struct(GenericStructType<'a>),
    Func(GenericFuncType<'a>),
    Ty(&'a Type<'a>),
    Ptr(&'a GenericType<'a>),
    ArrayPtr(&'a GenericType<'a>),
    TypeArg(TypeArg<'a>),
}

impl<'a> GenericTypeRepr<'a> {
    pub fn as_struct(&self) -> Option<&GenericStructType<'a>> {
        if let Self::Struct(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct GenericStructType<'a> {
    pub body: OnceCell<GenericStructBody<'a>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct GenericStructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a GenericType<'a>>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct GenericFuncType<'a> {
    pub params: &'a [&'a GenericType<'a>],
    pub return_type: &'a GenericType<'a>,
}

impl<'a> GenericFuncType<'a> {
    pub(crate) fn monomorphize<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> FuncType<'a> {
        let mut params = BumpVec::with_capacity_in(self.params.len(), ctx.arena);
        for ty in self.params {
            params.push(ty.monomorphize_shallow(ctx, type_args));
        }
        let return_type = self.return_type.monomorphize_shallow(ctx, type_args);
        FuncType {
            params: params.into_bump_slice(),
            return_type,
        }
    }

    pub(crate) fn monomorphize_generic<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a GenericTypeArgs<'a>,
    ) -> GenericFuncType<'a> {
        let mut params = BumpVec::with_capacity_in(self.params.len(), ctx.arena);
        for ty in self.params {
            params.push(ty.monomorphize_generic_shallow(ctx, type_args));
        }
        let return_type = self
            .return_type
            .monomorphize_generic_shallow(ctx, type_args);
        GenericFuncType {
            params: params.into_bump_slice(),
            return_type,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeArg<'a> {
    pub(crate) index: usize,
    pub(crate) name: Symbol<'a>,
}

impl<'a> TypeArg<'a> {
    pub(crate) fn new(index: usize, name: Symbol<'a>) -> Self {
        Self { index, name }
    }
}

pub(crate) fn get_typeparams<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    nodes: &[TypeParameterNode],
) -> &'a [TypeArg<'a>] {
    let mut type_params = BumpVec::with_capacity_in(nodes.len(), ctx.arena);
    let mut param_pos = HashMap::<Symbol, Pos>::default();
    for (i, type_param) in nodes.iter().enumerate() {
        let name = ctx.symbols.define(type_param.name.value.as_str());
        type_params.push(TypeArg::new(i, name));
        if let Some(declared_at) = param_pos.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(type_param.name.pos, declared_at, name);
        } else {
            param_pos.insert(name, type_param.name.pos);
        }
    }
    type_params.into_bump_slice()
}

fn get_typeparam_scope<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    type_params: &[TypeArg<'a>],
) -> Scopes<'a> {
    let mut type_param_table = IndexMap::<Symbol, TypeObject>::default();
    for type_param in type_params {
        if !type_param_table.contains_key(&type_param.name) {
            let generic_ty = ctx.generic_types.define(GenericType::new(
                GenericTypeKind::Anonymous,
                &[],
                GenericTypeRepr::TypeArg(*type_param),
            ));

            type_param_table.insert(
                type_param.name,
                TypeObject {
                    kind: TypeObjectKind::Generic(generic_ty),
                    node: None,
                },
            );
        }
    }

    let mut scope = scope.clone();
    if !type_param_table.is_empty() {
        let new_type_scope = scope.type_scopes.new_child(type_param_table);
        scope = scope.with_type_scope(new_type_scope);
    }

    scope
}

fn get_generic_type_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &TypeExprNode,
) -> &'a GenericType<'a> {
    match node {
        TypeExprNode::Invalid(..) => ctx.generic_types.define(GenericType::new(
            GenericTypeKind::Anonymous,
            &[],
            GenericTypeRepr::Unknown,
        )),
        TypeExprNode::Path(node) => get_generic_type_from_path(ctx, scope, node),
        TypeExprNode::Ptr(node) => {
            let element_ty = get_generic_type_from_node(ctx, scope, &node.ty);
            ctx.generic_types.define(GenericType::new(
                GenericTypeKind::Anonymous,
                &[],
                GenericTypeRepr::Ptr(element_ty),
            ))
        }
        TypeExprNode::ArrayPtr(node) => {
            let element_ty = get_generic_type_from_node(ctx, scope, &node.ty);
            ctx.generic_types.define(GenericType::new(
                GenericTypeKind::Anonymous,
                &[],
                GenericTypeRepr::ArrayPtr(element_ty),
            ))
        }
        TypeExprNode::Func(node) => {
            let mut params = BumpVec::with_capacity_in(node.params.len(), ctx.arena);
            for param_node in &node.params {
                params.push(get_generic_type_from_node(ctx, scope, param_node));
            }

            let return_type = if let Some(expr) = &node.return_type {
                get_generic_type_from_node(ctx, scope, expr)
            } else {
                ctx.generic_types.define(GenericType::new(
                    GenericTypeKind::Anonymous,
                    &[],
                    GenericTypeRepr::Ty(ctx.types.define(Type {
                        kind: TypeKind::Anonymous,
                        repr: TypeRepr::Void,
                    })),
                ))
            };

            ctx.generic_types.define(GenericType::new(
                GenericTypeKind::Anonymous,
                &[],
                GenericTypeRepr::Func(GenericFuncType {
                    params: params.into_bump_slice(),
                    return_type,
                }),
            ))
        }
        TypeExprNode::Grouped(node) => get_generic_type_from_node(ctx, scope, node),
    }
}

fn get_generic_type_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &PathNode,
) -> &'a GenericType<'a> {
    let mut type_args = node
        .args
        .iter()
        .map(|node| get_generic_type_from_node(ctx, scope, node))
        .collect::<Vec<_>>();

    let Some(object) = get_type_object_from_path(ctx, scope, &node.names) else {
        return ctx.generic_types.define(GenericType::new(
            GenericTypeKind::Anonymous,
            &[],
            GenericTypeRepr::Unknown,
        ));
    };

    match &object.kind {
        TypeObjectKind::Regular(ty) => {
            if !node.args.is_empty() {
                ctx.errors.non_generic_value(node.pos());
            }

            ctx.generic_types.define(GenericType::new(
                GenericTypeKind::Anonymous,
                &[],
                GenericTypeRepr::Ty(ty),
            ))
        }
        TypeObjectKind::Generic(generic_ty) => {
            let required_type_param = generic_ty.params.len();
            if type_args.len() != required_type_param {
                ctx.errors.type_arguments_count_mismatch(
                    node.pos(),
                    required_type_param,
                    type_args.len(),
                );
            }

            while type_args.len() < required_type_param {
                let unknown_type = ctx.generic_types.define(GenericType::new(
                    GenericTypeKind::Anonymous,
                    &[],
                    GenericTypeRepr::Unknown,
                ));
                type_args.push(unknown_type);
            }
            let generic_type_args = ctx.generic_typeargs.define(&type_args);

            generic_ty.monomorphize_generic_shallow(ctx, generic_type_args)
        }
    }
}
