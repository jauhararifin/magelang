use crate::analyze::{Context, Scopes, TypeObject};
use crate::errors::SemanticError;
use crate::interner::Interner;
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{
    ErrorReporter, PathNode, Pos, SignatureNode, StructNode, Token, TypeExprNode, TypeParameterNode,
};
use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;

// TODO: consider creating a new-type for type-args to implement hash, eq, and partial-eq
// to improve performance.
pub type TypeArgs<'a> = [&'a Type<'a>];
pub(crate) type TypeArgsInterner<'a> = Interner<'a, TypeArgs<'a>>;

#[derive(Debug)]
pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub repr: TypeRepr<'a>,
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TypeKind::Builtin(a), TypeKind::Builtin(b)) => a.eq(&b),
            (TypeKind::User(a), TypeKind::User(b)) => a.eq(&b),
            (TypeKind::Inst(a), TypeKind::Inst(b)) => a.eq(&b),
            (TypeKind::Generic(a), TypeKind::Generic(b)) => a.eq(&b),
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
            TypeKind::Builtin(ty) => Display::fmt(ty.name, f),
            TypeKind::User(ty) => Display::fmt(&ty.def_id, f),
            TypeKind::Inst(ty) => Display::fmt(&ty, f),
            TypeKind::Generic(ty) => Display::fmt(&ty, f),
            TypeKind::Anonymous => Display::fmt(&self.repr, f),
        }
    }
}

impl<'a> Type<'a> {
    pub(crate) fn init_body<E: ErrorReporter>(&self, ctx: &Context<'a, E>) {
        let TypeRepr::Struct(struct_type) = &self.repr else {
            return;
        };

        let Some(def_id) = self.kind.get_def_id() else {
            return
        };
        let package = def_id.package;
        let package_scope = ctx.scopes.get(package).expect("missing package scope");

        let scope = if let TypeKind::Generic(kind) = &self.kind {
            Some(get_typeparam_scope(ctx, package_scope, kind.type_params))
        } else {
            None
        };
        let scope = scope.as_ref().unwrap_or(package_scope);

        let mut field_pos = HashMap::<Symbol, Pos>::default();
        let mut fields = IndexMap::<Symbol, &'a Type<'a>>::default();
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
                field_pos.insert(field_name, pos);
                let ty = get_type_from_node(ctx, &scope, &field_node.ty);
                fields.insert(field_name, ty);
            }
        }

        let sized = true; // TODO: set this value properly.
        let body = StructBody { fields, sized };

        struct_type.body.set(body).expect("cannot set struct body");
    }

    pub(crate) fn monomorphize<E: ErrorReporter>(
        &self,
        ctx: &Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        // TODO: use cache to avoid cycle
        todo!();
    }

    pub fn as_func(&self) -> Option<&FuncType<'a>> {
        self.repr.as_func()
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        self.repr.as_struct()
    }

    pub fn is_void(&self) -> bool {
        self.repr.is_void()
    }

    pub fn is_integral(&self) -> bool {
        self.repr.is_integral()
    }

    pub fn is_float(&self) -> bool {
        self.repr.is_float()
    }

    pub fn is_int(&self) -> bool {
        self.repr.is_int()
    }

    pub fn is_bool(&self) -> bool {
        self.repr.is_bool()
    }

    pub fn is_opaque(&self) -> bool {
        self.repr.is_opaque()
    }

    pub fn is_sized(&self) -> bool {
        self.repr.is_sized()
    }

    pub fn is_unknown(&self) -> bool {
        self.repr.is_unknown()
    }

    pub(crate) fn is_arithmetic(&self) -> bool {
        self.repr.is_arithmetic()
    }

    pub(crate) fn is_assignable_with(&self, other: &Self) -> bool {
        self.kind.eq(&other.kind) || self.repr.eq(&other.repr)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TypeKind<'a> {
    Builtin(BuiltinType<'a>), // TODO: maybe builtin and anonymous are the same
    User(UserType<'a>),
    Inst(InstType<'a>),
    Generic(GenericType<'a>),
    Anonymous,
}

impl<'a> TypeKind<'a> {
    fn get_def_id(&self) -> Option<DefId<'a>> {
        match self {
            TypeKind::Builtin(..) => None,
            TypeKind::User(ty) => Some(ty.def_id),
            TypeKind::Inst(ty) => Some(ty.def_id),
            TypeKind::Generic(ty) => Some(ty.def_id),
            TypeKind::Anonymous => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BuiltinType<'a> {
    pub name: Symbol<'a>,
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
        for ty in self.type_args.iter() {
            Display::fmt(ty, f)?;
            write!(f, ",")?;
        }
        write!(f, ">")
    }
}

#[derive(Debug)]
pub struct GenericType<'a> {
    pub def_id: DefId<'a>,
    pub type_params: &'a [TypeArg<'a>],
    pub(crate) mono_cache: RefCell<HashMap<&'a TypeArgs<'a>, &'a Type<'a>>>,
}

impl<'a> Display for GenericType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.def_id, f)?;
        write!(f, "::<")?;
        for ty in self.type_params.iter() {
            Display::fmt(ty.name, f)?;
            write!(f, ",")?;
        }
        write!(f, ">")
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
    Int(IntSign, BitSize),
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
        matches!(
            self,
            Self::Int(..) | Self::Float(..) | Self::Ptr(..) | Self::ArrayPtr(..)
        )
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

    pub(crate) fn is_sized(&self) -> bool {
        match self {
            TypeRepr::Unknown => true,
            TypeRepr::Void => true,
            TypeRepr::Struct(struct_type) => {
                struct_type.body.get().expect("missing struct body").sized
            }
            TypeRepr::Func(..) => true,
            TypeRepr::Opaque => false,
            TypeRepr::Bool => true,
            TypeRepr::Int(..) => true,
            TypeRepr::Float(..) => true,
            TypeRepr::Ptr(..) => true,
            TypeRepr::ArrayPtr(..) => true,
            TypeRepr::TypeArg(..) => true,
        }
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
    pub(crate) node: StructNode, // TODO: remove this node.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a Type<'a>>,
    pub sized: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType<'a> {
    // TODO: using the arena to allocate vec, or use slice
    // alltogeher
    pub params: &'a [&'a Type<'a>],
    pub return_type: &'a Type<'a>,
}

impl<'a> Display for FuncType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for ty in self.params {
            Display::fmt(ty, f)?;
            write!(f, ",")?;
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeArg<'a> {
    pub(crate) index: usize,
    pub(crate) name: Symbol<'a>,
}

impl<'a> TypeArg<'a> {
    pub(crate) fn new(index: usize, name: Symbol<'a>) -> Self {
        Self { index, name }
    }

    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        _: &'b Context<'a, E>,
        type_args: &'a TypeArgs<'a>,
    ) -> &'a Type<'a> {
        type_args
            .get(self.index)
            .expect("missing type arg at the index")
    }
}

pub(crate) fn get_type_from_node<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
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
                params.push(get_type_from_node(ctx, scope, param_node));
            }

            let return_type = if let Some(expr) = &node.return_type {
                get_type_from_node(ctx, &scope, expr)
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
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    node: &PathNode,
) -> &'a Type<'a> {
    let Some(object) = get_type_object_from_path(ctx, scope, &node.names) else {
        return ctx.define_type(Type{kind: TypeKind::Anonymous, repr: TypeRepr::Unknown});
    };

    let TypeKind::Generic(generic_type) = &object.kind else {
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

    object.monomorphize(ctx, type_args)
}

fn get_type_object_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    names: &[Token],
) -> Option<&'b TypeObject<'a>> {
    assert!(!names.is_empty());

    let name = names.first().expect("path contains empty names");
    let name = ctx.define_symbol(name.value.as_str());

    if names.len() == 1 {
        let Some(object) = scope.type_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[0].pos, &names[0].value);
            return None;
        };
        Some(object)
    } else {
        let Some(import_object) = scope.import_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[0].pos, &names[0].value);
            return None;
        };

        let Some(scope) = ctx.scopes.get(&import_object.package) else {
            ctx.errors.undeclared_symbol(names[1].pos, &names[1].value);
            return None;
        };

        let name = ctx.define_symbol(names[1].value.as_ref());
        let Some(object) = scope.type_scopes.lookup(name) else {
            ctx.errors.undeclared_symbol(names[1].pos, &names[1].value);
            return None;
        };

        Some(object)
    }
}

pub(crate) fn get_func_type_from_signature<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
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
    ctx: &Context<'a, E>,
    nodes: &[TypeParameterNode],
) -> &'a [TypeArg<'a>] {
    let mut type_params = BumpVec::with_capacity_in(nodes.len(), ctx.arena);
    let mut param_pos = HashMap::<Symbol, Pos>::default();
    for (i, type_param) in nodes.iter().enumerate() {
        let name = ctx.define_symbol(type_param.name.value.as_str());
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

pub(crate) fn get_typeparam_scope<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
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

pub(crate) fn check_circular_type<E: ErrorReporter>(ctx: &Context<'_, E>) {
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
    ctx: &Context<'a, E>,
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
    ctx: &Context<'_, E>,
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

    let TypeRepr::Struct(struct_type) = &object.ty.repr else {
        unreachable!();
    };
    let pos = struct_type.node.pos;

    ctx.errors.circular_type(pos, &chain_str);
}
