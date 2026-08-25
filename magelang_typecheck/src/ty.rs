use crate::analyze::{Context, Scopes, TypeObject};
use crate::errors::SemanticError;
use crate::interner::Interner;
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{
    ErrorReporter, PathName, PathNode, Pos, SignatureNode, TypeExprNode, TypeParameterNode,
    WhereConstraintNode,
};
use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Display};
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;

// TODO: consider creating a new-type for type-args to implement hash, eq, and partial-eq
// to improve performance.
pub type TypeArgs<'a> = [&'a Type<'a>];
pub(crate) type TypeArgsInterner<'a> = Interner<'a, TypeArgs<'a>>;

pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub repr: TypeRepr<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Constraint<'a> {
    Numeric { pos: Pos },
    Integer { pos: Pos },
    Derefable { pos: Pos },
    Comparable { pos: Pos, other: &'a Type<'a> },
    Ordered { pos: Pos, other: &'a Type<'a> },
    CastableTo { pos: Pos, target: &'a Type<'a> },
}

impl<'a> Constraint<'a> {
    pub(crate) fn pos(&self) -> Pos {
        match *self {
            Self::Numeric { pos }
            | Self::Integer { pos }
            | Self::Derefable { pos }
            | Self::Comparable { pos, .. }
            | Self::Ordered { pos, .. }
            | Self::CastableTo { pos, .. } => pos,
        }
    }

    pub(crate) fn substitute<E: ErrorReporter>(
        self,
        ctx: &Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> Self {
        match self {
            Self::Numeric { pos } => Self::Numeric { pos },
            Self::Integer { pos } => Self::Integer { pos },
            Self::Derefable { pos } => Self::Derefable { pos },
            Self::Comparable { pos, other } => Self::Comparable {
                pos,
                other: other.substitute(ctx, type_args),
            },
            Self::Ordered { pos, other } => Self::Ordered {
                pos,
                other: other.substitute(ctx, type_args),
            },
            Self::CastableTo { pos, target } => Self::CastableTo {
                pos,
                target: target.substitute(ctx, type_args),
            },
        }
    }

    pub(crate) fn is_satisfied_by(&self, ty: &'a Type<'a>) -> bool {
        if self.is_satisfied_concrete_by(ty) {
            return true;
        }
        if !self.contains_type_arg(ty) {
            return false;
        }

        if type_arg_constraints(ty).is_some_and(|constraints| {
            constraints
                .iter()
                .any(|constraint| constraint.satisfies_required(*self))
        }) {
            return true;
        }

        match *self {
            Self::Comparable { pos, other } => {
                type_arg_constraints(other).is_some_and(|constraints| {
                    constraints.iter().any(|constraint| {
                        constraint.satisfies_required(Self::Comparable { pos, other: ty })
                    })
                })
            }
            Self::Ordered { pos, other } => {
                type_arg_constraints(other).is_some_and(|constraints| {
                    constraints.iter().any(|constraint| {
                        constraint.satisfies_required(Self::Ordered { pos, other: ty })
                    })
                })
            }
            _ => false,
        }
    }

    pub(crate) fn reason(&self, ty: &'a Type<'a>) -> String {
        match *self {
            Self::Numeric { .. } => format!("{ty} does not satisfy @numeric"),
            Self::Integer { .. } => format!("{ty} does not satisfy @integer"),
            Self::Derefable { .. } => format!("{ty} does not satisfy @derefable"),
            Self::Comparable { other, .. } => {
                format!("{ty} does not satisfy @comparable<{other}>")
            }
            Self::Ordered { other, .. } => format!("{ty} does not satisfy @ordered<{other}>"),
            Self::CastableTo { target, .. } => {
                format!("{ty} does not satisfy @castable<{target}>")
            }
        }
    }

    fn contains_type_arg(&self, ty: &'a Type<'a>) -> bool {
        ty.contains_type_arg()
            || match *self {
                Self::Numeric { .. } | Self::Integer { .. } | Self::Derefable { .. } => false,
                Self::Comparable { other, .. } | Self::Ordered { other, .. } => {
                    other.contains_type_arg()
                }
                Self::CastableTo { target, .. } => target.contains_type_arg(),
            }
    }

    fn is_satisfied_concrete_by(&self, ty: &'a Type<'a>) -> bool {
        match *self {
            Self::Numeric { .. } => ty.is_arithmetic(),
            Self::Integer { .. } => ty.is_int(),
            Self::Derefable { .. } => ty.is_derefable(),
            Self::Comparable { other, .. } => ty == other && ty.is_equality_comparable(),
            Self::Ordered { other, .. } => ty == other && ty.is_arithmetic(),
            Self::CastableTo { target, .. } => ty.is_castable_to(target),
        }
    }

    fn satisfies_required(&self, required: Self) -> bool {
        match (*self, required) {
            (Self::Numeric { .. } | Self::Integer { .. }, Self::Numeric { .. }) => true,
            (Self::Integer { .. }, Self::Integer { .. }) => true,
            (Self::Derefable { .. }, Self::Derefable { .. }) => true,
            (
                Self::Comparable { other, .. } | Self::Ordered { other, .. },
                Self::Comparable {
                    other: required, ..
                },
            ) => other == required,
            (
                Self::Ordered { other, .. },
                Self::Ordered {
                    other: required, ..
                },
            ) => other == required,
            (
                Self::CastableTo { target, .. },
                Self::CastableTo {
                    target: required, ..
                },
            ) => target == required,
            _ => false,
        }
    }
}

fn type_arg_constraints<'a>(ty: &'a Type<'a>) -> Option<&'a [Constraint<'a>]> {
    let TypeRepr::TypeArg(type_arg) = ty.repr else {
        return None;
    };
    type_arg.constraints.get().copied()
}

pub(crate) fn check_generic_constraints<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    func_name: Symbol<'a>,
    type_params: &[TypeArg<'a>],
    typeargs: &'a TypeArgs<'a>,
    instantiation_pos: Pos,
) {
    let formatted_typeargs = format_typeargs(typeargs);

    for type_param in type_params {
        let Some(constraints) = type_param.constraints.get().copied() else {
            continue;
        };
        for constraint in constraints {
            let ty = typeargs[type_param.index];
            let constraint = constraint.substitute(ctx, typeargs);
            if constraint.is_satisfied_by(ty) {
                continue;
            }

            let constraint_location = ctx.files.location(constraint.pos());
            ctx.errors.report(
                instantiation_pos,
                format!(
                    "cannot instantiate {func_name} with {formatted_typeargs} because {} ({constraint_location})",
                    constraint.reason(ty)
                ),
            );
        }
    }
}

fn format_typeargs(typeargs: &TypeArgs<'_>) -> String {
    let mut result = String::from("<");
    if let Some(ty) = typeargs.first() {
        result.push_str(&ty.to_string());
    }
    for ty in typeargs.iter().skip(1) {
        result.push(',');
        result.push_str(&ty.to_string());
    }
    result.push('>');
    result
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TypeKind::User(a), TypeKind::User(b)) => a.eq(b),
            (TypeKind::Inst(a), TypeKind::Inst(b)) => a.eq(b),
            (TypeKind::GenericStruct(a), TypeKind::GenericStruct(b)) => a.eq(b),
            (TypeKind::GenericFunc(a), TypeKind::GenericFunc(b)) => a.eq(b),
            (TypeKind::Anonymous, TypeKind::Anonymous) => self.repr.eq(&other.repr),
            _ => false,
        }
    }
}

impl<'a> Eq for Type<'a> {}

impl<'a> Hash for Type<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::User(ty) => Display::fmt(&ty.def_id, f),
            TypeKind::Inst(ty) => Display::fmt(&ty, f),
            TypeKind::GenericStruct(ty) => Display::fmt(&ty, f),
            TypeKind::GenericFunc(ty) => Display::fmt(&ty, f),
            TypeKind::Anonymous => Display::fmt(&self.repr, f),
        }
    }
}

impl<'a> Debug for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)?;
        write!(f, " of ")?;
        Debug::fmt(&self.repr, f)
    }
}

impl<'a> Type<'a> {
    pub(crate) fn init_body<E: ErrorReporter>(&'a self, ctx: &Context<'a, '_, E>) {
        let TypeRepr::Struct(struct_type) = &self.repr else {
            return;
        };

        let Some(def_id) = self.kind.get_def_id() else {
            return;
        };

        let package = def_id.package;
        let package_scope = ctx.scopes.get(package).expect("missing package scope");

        let scope = if let TypeKind::GenericStruct(kind) = &self.kind {
            Some(get_typeparam_scope(ctx, package_scope, kind.type_params))
        } else {
            None
        };
        let scope = scope.as_ref().unwrap_or(package_scope);

        let struct_node = ctx
            .scopes
            .get(def_id.package)
            .expect("missing package scope")
            .type_scopes
            .lookup(def_id.name)
            .expect("missing object")
            .node
            .as_ref()
            .expect("missing object node");

        let mut field_pos = HashMap::<Symbol, Pos>::default();
        let mut fields = IndexMap::<Symbol, &'a Type<'a>>::default();
        for field_node in &struct_node.fields {
            let field_name = ctx.define_symbol(field_node.name.value.as_str());
            let pos = field_node.name.pos;
            if let Some(defined_at) = field_pos.get(&field_name) {
                ctx.errors.redeclared_symbol(
                    pos,
                    ctx.files.location(*defined_at),
                    &field_node.name.value,
                );
            } else {
                field_pos.insert(field_name, pos);
                let ty = get_type_from_node(ctx, scope, &field_node.ty);
                fields.insert(field_name, ty);
            }
        }

        let body = StructBody { fields };

        struct_type.body.set(body).expect("cannot set struct body");

        if let TypeKind::GenericStruct(kind) = &self.kind {
            // TODO: I think we don't really need to do this. we can just do dfs normally
            let instanced_types: Vec<&TypeArgs> = kind
                .mono_cache
                .borrow()
                .values()
                .filter_map(|ty| {
                    if let TypeKind::Inst(inst) = ty.kind {
                        Some(inst.type_args)
                    } else {
                        None
                    }
                })
                .collect();

            for type_args in instanced_types {
                self.specialize(ctx, type_args);
            }
        };
    }

    pub(crate) fn specialize<E: ErrorReporter>(
        &'a self,
        ctx: &Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        match &self.kind {
            TypeKind::GenericStruct(generic_type) => {
                // TODO: maybe it's better to put the cache in the ctx instead of type itself.
                // It's better because the cache won't leaked to the codegen phase. But, I'm
                // not entirely sure yet.
                // Or maybe, we can omit the cache entirely and just rely on the type integer.
                // The type interner use the type kind as the identifier. Which means, instantiated
                // type with the same name will always refer to the same object. We can utilize
                // this behavior to get the caching benefit.
                let mut cache = generic_type.mono_cache.borrow_mut();
                if let Some(ty) = cache.get(type_args) {
                    return ty;
                }
                let ty = ctx.define_type(Type {
                    kind: TypeKind::Inst(InstType {
                        def_id: generic_type.def_id,
                        type_args,
                    }),
                    repr: TypeRepr::Struct(StructType {
                        body: OnceCell::default(),
                    }),
                });
                cache.insert(type_args, ty);
                drop(cache);

                // it is important to call initialize the body after the cache is inserted and
                // dropped to avoid infinite loop due to circular traversal in the type graph.
                if let Some(body) = self
                    .repr
                    .as_struct()
                    .expect("generic structs have struct repr")
                    .body
                    .get()
                {
                    let TypeRepr::Struct(ref instanced_repr) = ty.repr else {
                        unreachable!()
                    };
                    instanced_repr.body.get_or_init(|| {
                        let fields = body
                            .fields
                            .iter()
                            .map(|(name, ty)| (*name, ty.substitute(ctx, type_args)))
                            .collect::<IndexMap<_, _>>();
                        StructBody { fields }
                    });
                }

                ty
            }
            TypeKind::GenericFunc(generic_type) => {
                let TypeRepr::Func(ref func_type) = self.repr else {
                    unreachable!()
                };
                let mut cache = generic_type.mono_cache.borrow_mut();
                // it is ok to not prefilled the cache like the one we have for generic struct
                // because generic function type is never circular
                cache.entry(type_args).or_insert_with(|| {
                    ctx.define_type(Type {
                        kind: TypeKind::Inst(InstType {
                            def_id: generic_type.def_id,
                            type_args,
                        }),
                        repr: TypeRepr::Func(func_type.substitute(ctx, type_args)),
                    })
                })
            }
            _ => unreachable!("can't specialize non generic type"),
        }
    }

    pub(crate) fn substitute<E: ErrorReporter>(
        &'a self,
        ctx: &Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        assert!(
            !matches!(
                self.kind,
                TypeKind::GenericFunc(..) | TypeKind::GenericStruct(..)
            ),
            "a struct field can't contain generic type. generic type is only defined in top level"
        );

        match &self.repr {
            TypeRepr::Unknown => self,
            TypeRepr::Struct(struct_type) => match self.kind {
                TypeKind::GenericStruct(..) | TypeKind::GenericFunc(..) => unreachable!(),
                TypeKind::Inst(inst_type) => {
                    let object = ctx
                        .scopes
                        .get(&inst_type.def_id.package)
                        .expect("package scope is populated")
                        .type_scopes
                        .lookup(inst_type.def_id.name)
                        .expect("generic type is defined");
                    let mut substituted_typeargs =
                        BumpVec::with_capacity_in(inst_type.type_args.len(), ctx.arena);
                    for type_arg in inst_type.type_args {
                        substituted_typeargs.push(type_arg.substitute(ctx, type_args));
                    }
                    let substituted_typeargs =
                        ctx.define_typeargs(substituted_typeargs.into_bump_slice());
                    object.ty.specialize(ctx, substituted_typeargs)
                }
                TypeKind::User(..) => self,
                TypeKind::Anonymous => {
                    let fields = struct_type
                        .body
                        .get()
                        .expect("all anonymous struct body should've been resolved")
                        .fields
                        .iter()
                        .map(|(name, ty)| (*name, ty.substitute(ctx, type_args)))
                        .collect::<IndexMap<_, _>>();
                    ctx.define_type(Type {
                        kind: TypeKind::Anonymous,
                        repr: TypeRepr::Struct(StructType {
                            body: OnceCell::from(StructBody { fields }),
                        }),
                    })
                }
            },
            TypeRepr::Func(func_type) => match self.kind {
                TypeKind::GenericFunc(..) | TypeKind::GenericStruct(..) => {
                    unreachable!("a function in a struct field must not be a generic type")
                }
                TypeKind::User(..) => self,
                TypeKind::Inst(inst_type) => {
                    let mut substituted_typeargs =
                        BumpVec::with_capacity_in(inst_type.type_args.len(), ctx.arena);
                    for type_arg in inst_type.type_args {
                        substituted_typeargs.push(type_arg.substitute(ctx, type_args));
                    }
                    let substituted_typeargs =
                        ctx.define_typeargs(substituted_typeargs.into_bump_slice());

                    ctx.define_type(Type {
                        kind: TypeKind::Inst(InstType {
                            def_id: inst_type.def_id,
                            type_args: substituted_typeargs,
                        }),
                        repr: TypeRepr::Func(func_type.substitute(ctx, type_args)),
                    })
                }
                TypeKind::Anonymous => ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Func(func_type.substitute(ctx, type_args)),
                }),
            },
            TypeRepr::Void => self,
            TypeRepr::Opaque => self,
            TypeRepr::Bool => self,
            TypeRepr::UntypedInt => self,
            TypeRepr::Int(..) => self,
            TypeRepr::UntypedFloat => self,
            TypeRepr::Float(..) => self,
            TypeRepr::Ptr(el) => ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Ptr(el.substitute(ctx, type_args)),
            }),
            TypeRepr::ArrayPtr(el) => ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::ArrayPtr(el.substitute(ctx, type_args)),
            }),
            TypeRepr::TypeArg(arg) => arg.substitute(type_args),
        }
    }

    pub fn as_func(&self) -> Option<&FuncType<'a>> {
        self.repr.as_func()
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        self.repr.as_struct()
    }

    pub fn is_usize(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Int(false, BitSize::ISize))
    }

    pub fn is_void(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_void()
    }

    pub fn is_integral(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_integral()
    }

    pub fn is_float(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_float()
    }

    pub fn is_int(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_int()
    }

    pub fn is_f32(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Float(FloatType::F32))
    }

    pub fn is_f64(&self) -> bool {
        self.is_unknown() || matches!(self.repr, TypeRepr::Float(FloatType::F64))
    }

    pub fn is_byte_array(&self) -> bool {
        self.is_unknown()
            || if let TypeRepr::ArrayPtr(element_ty) = self.repr {
                matches!(element_ty.repr, TypeRepr::Int(false, BitSize::I8))
            } else {
                false
            }
    }

    pub fn is_bool(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_bool()
    }

    pub fn is_opaque(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_opaque()
    }

    pub fn is_strictly_opaque(&self) -> bool {
        self.repr.is_opaque()
    }

    pub fn is_unknown(&self) -> bool {
        self.repr.is_unknown()
    }

    pub(crate) fn is_arithmetic(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_arithmetic()
    }

    pub(crate) fn is_equality_comparable(&self) -> bool {
        self.repr.is_unknown() || self.repr.is_equality_comparable()
    }

    pub(crate) fn is_derefable(&self) -> bool {
        let mut visited = HashSet::<*const Type<'a>>::default();
        self.repr.is_unknown() || self.is_derefable_inner(&mut visited)
    }

    pub(crate) fn is_castable_to(&self, target: &Self) -> bool {
        self.is_unknown()
            || target.is_unknown()
            || (self.is_integral() && target.is_integral())
            || (self.is_float() && target.is_float())
            || (self.is_integral() && target.is_float())
            || (self.is_float() && target.is_integral())
    }

    fn is_derefable_inner(&self, visited: &mut HashSet<*const Type<'a>>) -> bool {
        let ptr = self as *const Type<'a>;
        if !visited.insert(ptr) {
            return true;
        }

        let result = match &self.repr {
            TypeRepr::Unknown => true,
            TypeRepr::Struct(struct_type) => struct_type
                .body
                .get()
                .map(|body| {
                    body.fields
                        .values()
                        .all(|ty| ty.is_derefable_inner(visited))
                })
                .unwrap_or(false),
            TypeRepr::Func(..)
            | TypeRepr::Void
            | TypeRepr::Bool
            | TypeRepr::Int(..)
            | TypeRepr::Float(..)
            | TypeRepr::Ptr(..)
            | TypeRepr::ArrayPtr(..) => true,
            TypeRepr::Opaque
            | TypeRepr::UntypedInt
            | TypeRepr::UntypedFloat
            | TypeRepr::TypeArg(..) => false,
        };

        visited.remove(&ptr);
        result
    }

    pub(crate) fn contains_type_arg(&self) -> bool {
        let mut visited = HashSet::<*const Type<'a>>::default();
        self.contains_type_arg_inner(&mut visited)
    }

    fn contains_type_arg_inner(&self, visited: &mut HashSet<*const Type<'a>>) -> bool {
        let ptr = self as *const Type<'a>;
        if !visited.insert(ptr) {
            return false;
        }

        let result = match &self.repr {
            TypeRepr::Unknown
            | TypeRepr::Void
            | TypeRepr::Opaque
            | TypeRepr::Bool
            | TypeRepr::UntypedInt
            | TypeRepr::Int(..)
            | TypeRepr::UntypedFloat
            | TypeRepr::Float(..) => false,
            TypeRepr::Struct(struct_type) => struct_type
                .body
                .get()
                .map(|body| {
                    body.fields
                        .values()
                        .any(|ty| ty.contains_type_arg_inner(visited))
                })
                .unwrap_or(false),
            TypeRepr::Func(func_type) => {
                func_type
                    .params
                    .iter()
                    .any(|ty| ty.contains_type_arg_inner(visited))
                    || func_type.return_type.contains_type_arg_inner(visited)
            }
            TypeRepr::Ptr(element_ty) | TypeRepr::ArrayPtr(element_ty) => {
                element_ty.contains_type_arg_inner(visited)
            }
            TypeRepr::TypeArg(..) => true,
        };

        visited.remove(&ptr);
        result
    }

    pub(crate) fn is_assignable_with(&self, other: &Self) -> bool {
        if self.is_unknown() || other.is_unknown() {
            return true;
        }
        if matches!(self.kind, TypeKind::Anonymous) {
            self.repr.is_assignable_with(&other.repr)
        } else {
            self.kind.eq(&other.kind) || self.repr.is_assignable_with(&other.repr)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind<'a> {
    User(UserType<'a>),
    Inst(InstType<'a>),
    GenericStruct(GenericType<'a>),
    GenericFunc(GenericType<'a>),
    Anonymous,
}

impl<'a> TypeKind<'a> {
    pub(crate) fn get_def_id(&self) -> Option<DefId<'a>> {
        match self {
            TypeKind::User(ty) => Some(ty.def_id),
            TypeKind::Inst(ty) => Some(ty.def_id),
            TypeKind::GenericStruct(ty) => Some(ty.def_id),
            TypeKind::GenericFunc(ty) => Some(ty.def_id),
            TypeKind::Anonymous => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UserType<'a> {
    pub def_id: DefId<'a>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstType<'a> {
    pub def_id: DefId<'a>,
    pub type_args: &'a TypeArgs<'a>,
}

impl<'a> Display for InstType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.def_id, f)?;
        write!(f, "::<")?;
        if let Some(ty) = self.type_args.first() {
            Display::fmt(ty, f)?;
        }
        for ty in self.type_args.iter().skip(1) {
            write!(f, ",")?;
            Display::fmt(&ty, f)?;
        }
        write!(f, ">")
    }
}

pub struct GenericType<'a> {
    pub def_id: DefId<'a>,
    pub type_params: &'a [TypeArg<'a>],
    pub(crate) mono_cache: RefCell<HashMap<&'a TypeArgs<'a>, &'a Type<'a>>>,
}

impl<'a> Display for GenericType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.def_id, f)?;
        write!(f, "::<")?;
        if let Some(ty) = self.type_params.first() {
            Display::fmt(ty.name, f)?;
        }
        for ty in self.type_params.iter().skip(1) {
            write!(f, ",")?;
            Display::fmt(ty.name, f)?;
        }
        write!(f, ">")
    }
}

impl<'a> Debug for GenericType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a> PartialEq for GenericType<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.def_id.eq(&other.def_id) && self.type_params.eq(other.type_params)
    }
}

impl<'a> Eq for GenericType<'a> {}

impl<'a> Hash for GenericType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
        self.type_params.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeRepr<'a> {
    Unknown,
    Struct(StructType<'a>),
    Func(FuncType<'a>),
    Void,
    Opaque,
    Bool,
    UntypedInt,
    Int(IntSign, BitSize),
    UntypedFloat,
    Float(FloatType),
    Ptr(&'a Type<'a>),
    ArrayPtr(&'a Type<'a>),
    TypeArg(TypeArg<'a>),
}

impl<'a> TypeRepr<'a> {
    pub fn as_func(&self) -> Option<&FuncType<'a>> {
        if let Self::Func(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        if let Self::Struct(t) = self {
            Some(t)
        } else {
            None
        }
    }

    pub(crate) fn is_opaque(&self) -> bool {
        matches!(self, Self::Opaque)
    }

    pub(crate) fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub(crate) fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Int(..) | Self::Float(..))
    }

    pub(crate) fn is_equality_comparable(&self) -> bool {
        match self {
            Self::Unknown => true,
            Self::Struct(struct_ty) => struct_ty
                .body
                .get()
                .map(|body| body.fields.values().all(|ty| ty.is_equality_comparable()))
                .unwrap_or(false),
            Self::Func(..)
            | Self::Void
            | Self::Bool
            | Self::Int(..)
            | Self::Float(..)
            | Self::Ptr(..)
            | Self::ArrayPtr(..) => true,
            Self::Opaque | Self::UntypedInt | Self::UntypedFloat | Self::TypeArg(..) => false,
        }
    }

    pub(crate) fn is_integral(&self) -> bool {
        matches!(self, Self::Int(..) | Self::Ptr(..) | Self::ArrayPtr(..))
    }

    pub(crate) fn is_float(&self) -> bool {
        matches!(self, Self::Float(..))
    }

    pub(crate) fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    pub(crate) fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub(crate) fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub(crate) fn is_assignable_with(&self, other: &Self) -> bool {
        self.eq(other)
    }
}

impl<'a> Display for TypeRepr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeRepr::Unknown => write!(f, "{{unknown}}"),
            TypeRepr::Struct(ty) => {
                if let Some(body) = ty.body.get() {
                    write!(f, "struct{{")?;
                    for (name, ty) in body.fields.iter() {
                        write!(f, "{name}: {ty},")?;
                    }
                    write!(f, "}}")
                } else {
                    write!(f, "struct{{missing body}}")
                }
            }
            TypeRepr::Func(func_type) => write!(f, "{}", func_type),
            TypeRepr::Void => write!(f, "void"),
            TypeRepr::Opaque => write!(f, "opaque"),
            TypeRepr::Bool => write!(f, "bool"),
            TypeRepr::UntypedInt => write!(f, "untyped int"),
            TypeRepr::Int(sign, size) => {
                write!(
                    f,
                    "{}{}",
                    if *sign { "i" } else { "u" },
                    match size {
                        BitSize::I8 => "8",
                        BitSize::I16 => "16",
                        BitSize::I32 => "32",
                        BitSize::I64 => "64",
                        BitSize::ISize => "size",
                    }
                )
            }
            TypeRepr::UntypedFloat => write!(f, "untyped float"),
            TypeRepr::Float(float_ty) => match float_ty {
                FloatType::F32 => write!(f, "f32"),
                FloatType::F64 => write!(f, "f64"),
            },
            TypeRepr::Ptr(ty) => {
                write!(f, "*{}", ty)
            }
            TypeRepr::ArrayPtr(ty) => {
                write!(f, "[*]{}", ty)
            }
            TypeRepr::TypeArg(arg) => {
                write!(f, "{}", arg.name)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructType<'a> {
    pub body: OnceCell<StructBody<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a Type<'a>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType<'a> {
    // TODO: using the arena to allocate vec, or use slice
    // alltogeher
    pub params: &'a [&'a Type<'a>],
    pub return_type: &'a Type<'a>,
}

impl<'a> FuncType<'a> {
    pub(crate) fn substitute<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, '_, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> FuncType<'a> {
        let mut params = BumpVec::with_capacity_in(self.params.len(), ctx.arena);
        for ty in self.params {
            params.push(ty.substitute(ctx, type_args));
        }
        let return_type = self.return_type.substitute(ctx, type_args);
        FuncType {
            params: params.into_bump_slice(),
            return_type,
        }
    }
}

impl<'a> Display for FuncType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        if let Some(ty) = self.params.first() {
            Display::fmt(ty, f)?;
        }
        for ty in self.params.iter().skip(1) {
            write!(f, ",")?;
            Display::fmt(&ty, f)?;
        }
        write!(f, "):")?;
        Display::fmt(&self.return_type, f)
    }
}

pub type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy)]
pub struct TypeArg<'a> {
    pub(crate) index: usize,
    pub(crate) name: Symbol<'a>,
    pub(crate) constraints: &'a OnceCell<&'a [Constraint<'a>]>,
}

impl<'a> PartialEq for TypeArg<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
            && self.name == other.name
            && std::ptr::eq(self.constraints, other.constraints)
    }
}

impl<'a> Eq for TypeArg<'a> {}

impl<'a> Hash for TypeArg<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.name.hash(state);
        (self.constraints as *const OnceCell<&[Constraint]>).hash(state);
    }
}

impl<'a> TypeArg<'a> {
    pub(crate) fn new(
        index: usize,
        name: Symbol<'a>,
        constraints: &'a OnceCell<&'a [Constraint<'a>]>,
    ) -> Self {
        Self {
            index,
            name,
            constraints,
        }
    }

    pub(crate) fn substitute(&self, type_args: &'a TypeArgs<'a>) -> &'a Type<'a> {
        type_args
            .get(self.index)
            .expect("missing type arg at the index")
    }
}

pub(crate) fn get_type_from_node<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, '_, E>,
    scope: &'b Scopes<'a>,
    node: &TypeExprNode,
) -> &'a Type<'a> {
    match node {
        TypeExprNode::Invalid(..) => ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        }),
        TypeExprNode::Path(node) => get_type_from_path(ctx, scope, node),
        TypeExprNode::Ptr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Ptr(element_ty),
            })
        }
        TypeExprNode::ArrayPtr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::ArrayPtr(element_ty),
            })
        }
        TypeExprNode::Func(node) => {
            let mut params = BumpVec::with_capacity_in(node.params.len(), ctx.arena);
            for param_node in &node.params {
                params.push(get_type_from_node(ctx, scope, &param_node.ty));
            }

            let return_type = if let Some(expr) = &node.return_type {
                get_type_from_node(ctx, scope, expr)
            } else {
                ctx.define_type(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Void,
                })
            };

            ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Func(FuncType {
                    params: params.into_bump_slice(),
                    return_type,
                }),
            })
        }
        TypeExprNode::Grouped(node) => get_type_from_node(ctx, scope, node),
    }
}

fn get_type_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, '_, E>,
    scope: &'b Scopes<'a>,
    node: &PathNode,
) -> &'a Type<'a> {
    let Some(object) = get_type_object_from_path(ctx, scope, &node.path) else {
        return ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    };

    let TypeKind::GenericStruct(generic_type) = &object.kind else {
        if !node.args.is_empty() {
            ctx.errors.non_generic_value(node.pos());
        }
        return object.ty;
    };

    let required_type_param = generic_type.type_params.len();
    let mut type_args = node
        .args
        .iter()
        .map(|node| get_type_from_node(ctx, scope, node))
        .collect::<Vec<_>>();

    if type_args.len() != required_type_param {
        ctx.errors
            .type_arguments_count_mismatch(node.pos(), required_type_param, type_args.len());
    }

    while type_args.len() < required_type_param {
        let unknown_type = ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
        type_args.push(unknown_type);
    }
    let type_args = ctx.define_typeargs(&type_args);

    object.specialize(ctx, type_args)
}

fn get_type_object_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, '_, E>,
    scope: &'b Scopes<'a>,
    path: &PathName,
) -> Option<&'b TypeObject<'a>> {
    match path {
        PathName::Local(name) => {
            let name_symbol = ctx.define_symbol(name.value.as_str());
            let Some(object) = scope.type_scopes.lookup(name_symbol) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };
            Some(object)
        }
        PathName::Package { package, name } => {
            let package_symbol = ctx.define_symbol(package.value.as_str());
            let Some(import_object) = scope.import_scopes.lookup(package_symbol) else {
                ctx.errors.undeclared_symbol(package.pos, &package.value);
                return None;
            };

            let Some(scope) = ctx.scopes.get(&import_object.package) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };

            let name_symbol = ctx.define_symbol(name.value.as_str());
            let Some(object) = scope.type_scopes.lookup(name_symbol) else {
                ctx.errors.undeclared_symbol(name.pos, &name.value);
                return None;
            };

            Some(object)
        }
        PathName::Invalid(..) => None,
    }
}

pub(crate) fn get_func_type_from_signature<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    type_params: &[TypeArg<'a>],
    signature: &SignatureNode,
) -> FuncType<'a> {
    let scope = get_typeparam_scope(ctx, scope, type_params);

    let mut param_pos = HashMap::<Symbol, Pos>::default();
    let mut params = BumpVec::with_capacity_in(signature.parameters.len(), ctx.arena);
    for param_node in &signature.parameters {
        let name = ctx.define_symbol(&param_node.name.value);
        let pos = param_node.name.pos;
        if let Some(defined_at) = param_pos.get(&name) {
            ctx.errors.redeclared_symbol(
                pos,
                ctx.files.location(*defined_at),
                &param_node.name.value,
            );
        } else {
            param_pos.insert(name, pos);
        }

        let ty = get_type_from_node(ctx, &scope, &param_node.ty);
        params.push(ty);
    }

    let return_type = if let Some(expr) = &signature.return_type {
        get_type_from_node(ctx, &scope, expr)
    } else {
        ctx.define_type(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Void,
        })
    };

    FuncType {
        params: params.into_bump_slice(),
        return_type,
    }
}

pub(crate) fn get_typeparams<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    nodes: &[TypeParameterNode],
    constraint_nodes: &[WhereConstraintNode],
) -> &'a [TypeArg<'a>] {
    let mut type_params = BumpVec::with_capacity_in(nodes.len(), ctx.arena);
    let mut param_pos = HashMap::<Symbol, Pos>::default();
    for (i, type_param) in nodes.iter().enumerate() {
        let name = ctx.define_symbol(type_param.name.value.as_str());
        let constraints = ctx.arena.alloc(OnceCell::default());
        type_params.push(TypeArg::new(i, name, constraints));
        if let Some(declared_at) = param_pos.get(&name) {
            let declared_at = ctx.files.location(*declared_at);
            ctx.errors
                .redeclared_symbol(type_param.name.pos, declared_at, name);
        } else {
            param_pos.insert(name, type_param.name.pos);
        }
    }

    let type_params = type_params.into_bump_slice();
    let scope = get_typeparam_scope(ctx, scope, type_params);
    init_typearg_constraints(ctx, &scope, type_params, constraint_nodes);
    type_params
}

fn init_typearg_constraints<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    type_params: &[TypeArg<'a>],
    nodes: &[WhereConstraintNode],
) {
    let mut constraints = vec![Vec::<Constraint>::default(); type_params.len()];

    for node in nodes {
        let target_name = ctx.define_symbol(&node.target.value);
        let Some(target_idx) = type_params
            .iter()
            .position(|type_param| type_param.name == target_name)
        else {
            ctx.errors
                .undeclared_symbol(node.target.pos, &node.target.value);
            continue;
        };

        let name = node.name.value.as_str();
        let arg_count = node.arguments.len();
        let get_arg = |idx: usize| {
            node.arguments
                .get(idx)
                .map(|arg| get_type_from_node(ctx, &scope, arg))
                .unwrap_or_else(|| {
                    ctx.define_type(Type {
                        kind: TypeKind::Anonymous,
                        repr: TypeRepr::Unknown,
                    })
                })
        };

        let constraint = match name {
            "numeric" => {
                check_constraint_arg_count(ctx, node.pos, name, 0, arg_count);
                Constraint::Numeric { pos: node.pos }
            }
            "integer" => {
                check_constraint_arg_count(ctx, node.pos, name, 0, arg_count);
                Constraint::Integer { pos: node.pos }
            }
            "derefable" => {
                check_constraint_arg_count(ctx, node.pos, name, 0, arg_count);
                Constraint::Derefable { pos: node.pos }
            }
            "comparable" => {
                check_constraint_arg_count(ctx, node.pos, name, 1, arg_count);
                Constraint::Comparable {
                    pos: node.pos,
                    other: get_arg(0),
                }
            }
            "ordered" => {
                check_constraint_arg_count(ctx, node.pos, name, 1, arg_count);
                Constraint::Ordered {
                    pos: node.pos,
                    other: get_arg(0),
                }
            }
            "castable" => {
                check_constraint_arg_count(ctx, node.pos, name, 1, arg_count);
                Constraint::CastableTo {
                    pos: node.pos,
                    target: get_arg(0),
                }
            }
            _ => {
                ctx.errors
                    .report(node.name.pos, format!("Unknown generic constraint @{name}"));
                continue;
            }
        };
        constraints[target_idx].push(constraint);
    }

    for (type_param, constraints) in type_params.iter().zip(constraints) {
        let constraints = ctx.arena.alloc_slice_copy(&constraints);
        type_param
            .constraints
            .set(constraints)
            .expect("type arg constraints should only be initialized once");
    }
}

fn check_constraint_arg_count<E: ErrorReporter>(
    ctx: &Context<'_, '_, E>,
    pos: Pos,
    name: &str,
    expected: usize,
    found: usize,
) {
    if expected != found {
        ctx.errors.report(
            pos,
            format!("Generic constraint @{name} expects {expected} type arguments, found {found}"),
        );
    }
}

pub(crate) fn get_typeparam_scope<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
    scope: &Scopes<'a>,
    type_params: &[TypeArg<'a>],
) -> Scopes<'a> {
    let mut type_param_table = IndexMap::<Symbol, TypeObject>::default();
    for type_param in type_params {
        if !type_param_table.contains_key(&type_param.name) {
            let ty = ctx.define_type(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::TypeArg(*type_param),
            });
            type_param_table.insert(type_param.name, ty.into());
        }
    }

    let mut scope = scope.clone();
    if !type_param_table.is_empty() {
        let new_type_scope = scope.type_scopes.new_child(type_param_table);
        scope = scope.with_type_scope(new_type_scope);
    }

    scope
}

pub(crate) fn check_circular_type<E: ErrorReporter>(ctx: &Context<'_, '_, E>) {
    let dep_list = build_struct_dependency_list(ctx);

    let mut visited = IndexSet::<DefId>::default();
    let mut in_chain = IndexSet::<DefId>::default();
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

fn build_struct_dependency_list<'a, E: ErrorReporter>(
    ctx: &Context<'a, '_, E>,
) -> IndexMap<DefId<'a>, IndexSet<DefId<'a>>> {
    let mut adjlist = IndexMap::<DefId, IndexSet<DefId>>::default();
    let type_objects = ctx
        .scopes
        .values()
        .flat_map(|scopes| scopes.type_scopes.iter())
        .map(|(_, obj)| obj);

    for type_object in type_objects {
        let TypeRepr::Struct(struct_type) = &type_object.repr else {
            continue;
        };

        let Some(def_id) = type_object.kind.get_def_id() else {
            continue;
        };

        let dependencies = struct_type
            .body
            .get()
            .expect("missing struct body")
            .fields
            .values()
            .filter_map(|ty| ty.kind.get_def_id())
            .collect::<IndexSet<_>>();

        adjlist.insert(def_id, dependencies);
    }

    adjlist
}

fn report_circular_type<E: ErrorReporter>(
    ctx: &Context<'_, '_, E>,
    in_chain: &IndexSet<DefId>,
    start: DefId,
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
        let display = format!("{name}");
        chain_str.push(display);
    }

    let object = ctx
        .scopes
        .get(&start.package)
        .unwrap()
        .type_scopes
        .lookup(start.name)
        .unwrap();

    let pos = object
        .node
        .as_ref()
        .expect("missing strut node in type object")
        .pos;
    ctx.errors.circular_type(pos, &chain_str);
}
