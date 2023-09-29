use crate::analyze::{Context, Scopes, TypeObject};
use crate::errors::SemanticError;
use crate::interner::{Interned, Interner};
use crate::{DefId, Symbol};
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, PathNode, StructNode, Token, TypeExprNode};
use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

pub(crate) type TypeInterner<'a> = Interner<'a, Type<'a>>;
pub(crate) type InternType<'a> = Interned<'a, Type<'a>>;

pub(crate) type TypeArgsInterner<'a> = Interner<'a, [InternType<'a>]>;
pub(crate) type InternTypeArgs<'a> = Interned<'a, [InternType<'a>]>;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) enum Type<'a> {
    Unknown,
    Struct(StructType<'a>),
    Inst(InstType<'a>),
    Func(FuncType<'a>),
    Void,
    Opaque,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(InternType<'a>),
    ArrayPtr(InternType<'a>),
    TypeArg(TypeArg<'a>),
}

impl<'a> Type<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> InternType<'a> {
        match self {
            Self::Unknown => ctx.define_type(Self::Unknown),
            Self::Struct(struct_type) => struct_type.monomorphize(ctx, type_args),
            Self::Inst(inst_type) => inst_type.monomorphize(ctx, type_args),
            Self::Func(..) => todo!(),
            Self::Void => ctx.define_type(Self::Void),
            Self::Opaque => ctx.define_type(Self::Opaque),
            Self::Bool => ctx.define_type(Self::Bool),
            Self::Int(sign, size) => ctx.define_type(Type::Int(*sign, *size)),
            Self::Float(ty) => ctx.define_type(Type::Float(*ty)),
            Self::Ptr(el) => ctx.define_type(Type::Ptr(el.monomorphize(ctx, type_args))),
            Self::ArrayPtr(el) => ctx.define_type(Type::ArrayPtr(el.monomorphize(ctx, type_args))),
            Self::TypeArg(arg) => arg.monomorphize(ctx, type_args),
        }
    }

    pub(crate) fn as_inst(&self) -> Option<&InstType<'a>> {
        if let Self::Inst(t) = self {
            Some(t)
        } else {
            None
        }
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unknown => write!(f, "{{unknown}}"),
            Type::Struct(ty) => ty.def_id.fmt(f),
            Type::Inst(ty) => ty.fmt(f),
            Type::Func(func_type) => write!(f, "{}", func_type),
            Type::Void => write!(f, "void"),
            Type::Opaque => write!(f, "opaque"),
            Type::Bool => write!(f, "bool"),
            Type::Int(sign, size) => {
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
            Type::Float(float_ty) => match float_ty {
                FloatType::F32 => write!(f, "f32"),
                FloatType::F64 => write!(f, "f64"),
            },
            Type::Ptr(ty) => {
                write!(f, "*{}", ty)
            }
            Type::ArrayPtr(ty) => {
                write!(f, "[*]{}", ty)
            }
            Type::TypeArg(arg) => {
                write!(f, "{}", arg.name)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct StructType<'a> {
    pub(crate) def_id: DefId<'a>,
    pub(crate) type_params: Vec<TypeArg<'a>>,
    pub(crate) body: OnceCell<StructBody<'a>>,
    pub(crate) sized: OnceCell<bool>,
    pub(crate) node: &'a StructNode,
    pub(crate) mono_cache: RefCell<HashMap<InternTypeArgs<'a>, InternType<'a>>>,
}

impl<'a> Hash for StructType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
    }
}

impl<'a> StructType<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> InternType<'a> {
        {
            let mut cache = self.mono_cache.borrow_mut();
            if let Some(ty) = cache.get(&type_args) {
                return *ty;
            } else {
                let ty = ctx.define_type(Type::Inst(InstType {
                    def_id: self.def_id,
                    type_args,
                    body: OnceCell::default(),
                }));
                cache.insert(type_args, ty);
            }
        }

        let body = self.body.get().expect("missing struct body");
        let fields = body
            .fields
            .iter()
            .map(|(name, ty)| (*name, ty.monomorphize(ctx, type_args)))
            .collect::<IndexMap<_, _>>();
        let substituted_body = StructBody { fields };

        let cache = self.mono_cache.borrow_mut();
        let interned_ty = cache.get(&type_args).unwrap();
        let Type::Inst(ty) = interned_ty.as_ref() else {
            unreachable!();
        };
        ty.body
            .set(substituted_body)
            .expect("cannot set instance body");
        *interned_ty
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct StructBody<'a> {
    pub(crate) fields: IndexMap<Symbol<'a>, InternType<'a>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) struct InstType<'a> {
    pub def_id: DefId<'a>,
    pub type_args: InternTypeArgs<'a>,
    pub body: OnceCell<StructBody<'a>>, // only relevant on concrete type
}

impl<'a> InstType<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> InternType<'a> {
        let ty = ctx
            .scopes
            .get(&self.def_id.package)
            .expect("missing package scope")
            .type_scopes
            .lookup(self.def_id.name)
            .expect("missing type");

        let substituted_type_args = type_args
            .iter()
            .map(|ty| ty.monomorphize(ctx, type_args))
            .collect::<Vec<_>>();
        let substituted_type_args = ctx.define_typeargs(&substituted_type_args);

        ty.monomorphize(ctx, substituted_type_args)
    }
}

impl<'a> Hash for InstType<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
        self.type_args.hash(state);
    }
}

impl<'a> Display for InstType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.def_id.fmt(f)?;
        write!(f, "::<")?;
        for ty in self.type_args.iter() {
            ty.fmt(f)?;
            write!(f, ",")?;
        }
        write!(f, ">")
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) struct FuncType<'a> {
    pub params: Vec<InternType<'a>>,
    pub return_type: InternType<'a>,
}

impl<'a> FuncType<'a> {
    pub(crate) fn monomorphize<'b, E: ErrorReporter>(
        &self,
        ctx: &'b Context<'a, E>,
        type_args: InternTypeArgs<'a>,
    ) -> InternType<'a> {
        let params = self
            .params
            .iter()
            .map(|ty| ty.monomorphize(ctx, type_args))
            .collect();
        let return_type = self.return_type.monomorphize(ctx, type_args);
        ctx.define_type(Type::Func(FuncType {
            params,
            return_type,
        }))
    }
}

impl<'a> Display for FuncType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for ty in &self.params {
            ty.fmt(f)?;
            write!(f, ",")?;
        }
        write!(f, "):")?;
        self.return_type.fmt(f)
    }
}

type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) enum FloatType {
    F32,
    F64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct TypeArg<'a> {
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
        type_args: InternTypeArgs<'a>,
    ) -> InternType<'a> {
        *type_args
            .get(self.index)
            .expect("missing type arg at the index")
    }
}

pub(crate) fn get_type_from_node<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    node: &TypeExprNode,
) -> InternType<'a> {
    match node {
        TypeExprNode::Invalid(..) => ctx.define_type(Type::Unknown),
        TypeExprNode::Path(node) => get_type_from_path(ctx, scope, node),
        TypeExprNode::Ptr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.define_type(Type::Ptr(element_ty))
        }
        TypeExprNode::ArrayPtr(node) => {
            let element_ty = get_type_from_node(ctx, scope, &node.ty);
            ctx.define_type(Type::ArrayPtr(element_ty))
        }
        TypeExprNode::Grouped(node) => get_type_from_node(ctx, scope, &node),
    }
}

fn get_type_from_path<'a, 'b, E: ErrorReporter>(
    ctx: &'b Context<'a, E>,
    scope: &'b Scopes<'a>,
    node: &PathNode,
) -> InternType<'a> {
    let Some(object) = get_type_object_from_path(ctx, scope, &node.names) else {
        return ctx.define_type(Type::Unknown);
    };

    let Type::Struct(struct_type) = object.ty.as_ref() else {
        return object.ty;
    };

    let is_not_generic = struct_type.type_params.is_empty();
    if is_not_generic {
        if !node.args.is_empty() {
            ctx.errors.non_generic_value(node.pos());
        }
        return object.ty;
    }

    let required_type_param = struct_type.type_params.len();
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
        let unknown_type = ctx.define_type(Type::Unknown);
        type_args.push(unknown_type.into());
    }
    let type_args = ctx.define_typeargs(&type_args);

    ctx.define_type(Type::Inst(InstType {
        def_id: struct_type.def_id,
        type_args,
        body: OnceCell::default(),
    }))
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
        return Some(object);
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
