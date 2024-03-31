use crate::analyze::{Context, Scopes, TypeObject, TypeObjectKind};
use crate::errors::SemanticError;
use crate::generic_ty::{GenericTypeRepr, TypeArgs};
use crate::interner::Interner;
use crate::{DefId, Symbol};
use bumpalo::collections::Vec as BumpVec;
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{ErrorReporter, Identifier, PathNode, Pos, SignatureNode, TypeExprNode};
use std::cell::OnceCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;

#[derive(PartialEq, Eq)]
pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub repr: TypeRepr<'a>,
}

impl<'a> Hash for Type<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state)
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
    pub(crate) fn init_body<E: ErrorReporter>(&'a self, ctx: &Context<'a, E>) {
        let TypeRepr::Struct(struct_type) = &self.repr else {
            return;
        };

        let Some(def_id) = self.kind.get_def_id() else {
            return;
        };

        let package = def_id.package;
        let scope = ctx
            .package_scopes
            .get(package)
            .expect("missing package scope");

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
        let mut fields = IndexMap::<Symbol, &'a Type<'a>>::default();
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
                let ty = get_type_from_node(ctx, scope, &field_node.ty);
                fields.insert(field_name, ty);
            }
        }

        let body = StructBody { fields };
        struct_type.body.set(body).expect("cannot set struct body");
    }

    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        self.repr.as_struct()
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum TypeKind<'a> {
    User(DefId<'a>),
    Inst(DefId<'a>, &'a TypeArgs<'a>),
    Anonymous,
}

impl<'a> TypeKind<'a> {
    pub(crate) fn get_def_id(&self) -> Option<DefId<'a>> {
        match self {
            TypeKind::User(def_id) => Some(*def_id),
            TypeKind::Inst(def_id, _) => Some(*def_id),
            TypeKind::Anonymous => None,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
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
}

impl<'a> TypeRepr<'a> {
    pub fn as_struct(&self) -> Option<&StructType<'a>> {
        if let Self::Struct(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct StructType<'a> {
    pub body: OnceCell<StructBody<'a>>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct StructBody<'a> {
    pub fields: IndexMap<Symbol<'a>, &'a Type<'a>>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FuncType<'a> {
    pub params: &'a [&'a Type<'a>],
    pub return_type: &'a Type<'a>,
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

fn get_type_from_node<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &TypeExprNode,
) -> &'a Type<'a> {
    match node {
        TypeExprNode::Invalid(..) => ctx.types.define(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        }),
        TypeExprNode::Path(node) => get_type_from_path(ctx, scope, node),
        TypeExprNode::Ptr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.types.define(Type {
                kind: TypeKind::Anonymous,
                repr: TypeRepr::Ptr(element_ty),
            })
        }
        TypeExprNode::ArrayPtr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.types.define(Type {
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
                get_type_from_node(ctx, scope, expr)
            } else {
                ctx.types.define(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Void,
                })
            };

            ctx.types.define(Type {
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

fn get_type_from_path<'a, E: ErrorReporter>(
    ctx: &Context<'a, E>,
    scope: &Scopes<'a>,
    node: &PathNode,
) -> &'a Type<'a> {
    let mut type_args = node
        .args
        .iter()
        .map(|node| get_type_from_node(ctx, scope, node))
        .collect::<Vec<_>>();

    let Some(object) = get_type_object_from_path(ctx, scope, &node.names) else {
        return ctx.types.define(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Unknown,
        });
    };

    match &object.kind {
        TypeObjectKind::Regular(ty) => {
            if !node.args.is_empty() {
                ctx.errors.non_generic_value(node.pos());
            }
            ty
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
                let unknown_type = ctx.types.define(Type {
                    kind: TypeKind::Anonymous,
                    repr: TypeRepr::Unknown,
                });
                type_args.push(unknown_type);
            }
            let type_args = ctx.typeargs.define(&type_args);

            generic_ty.monomorphize_shallow(ctx, type_args)
        }
    }
}

pub(crate) fn get_type_object_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    names: &[Identifier],
) -> Option<&'b TypeObject<'a>> {
    assert!(!names.is_empty());

    let name = names.first().expect("path contains empty names");
    let name = ctx.symbols.define(name.value.as_str());

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

        let Some(scope) = ctx.package_scopes.get(&import_object.package) else {
            ctx.errors.undeclared_symbol(names[1].pos, &names[1].value);
            return None;
        };

        let name = ctx.symbols.define(names[1].value.as_ref());
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
    signature: &SignatureNode,
) -> FuncType<'a> {
    let mut param_pos = HashMap::<Symbol, Pos>::default();
    let mut params = BumpVec::with_capacity_in(signature.parameters.len(), ctx.arena);
    for param_node in &signature.parameters {
        let name = ctx.symbols.define(&param_node.name.value);
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
        ctx.types.define(Type {
            kind: TypeKind::Anonymous,
            repr: TypeRepr::Void,
        })
    };

    FuncType {
        params: params.into_bump_slice(),
        return_type,
    }
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
        .package_scopes
        .values()
        .flat_map(|scopes| scopes.type_scopes.iter())
        .map(|(_, obj)| obj);

    for type_object in type_objects {
        let dependencies = match type_object.kind {
            TypeObjectKind::Regular(ty) => {
                let Some(def_id) = ty.kind.get_def_id() else {
                    continue;
                };

                let TypeRepr::Struct(struct_type) = &ty.repr else {
                    continue;
                };

                let Some(def_id) = ty.kind.get_def_id() else {
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
            TypeObjectKind::Generic(ty) => {
                let Some(def_id) = ty.kind.get_def_id() else {
                    continue;
                };

                let GenericTypeRepr::Struct(struct_type) = &ty.repr else {
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
        };
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
        .package_scopes
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
