use crate::analyze::TypeCheckContext;
use crate::errors::SemanticError;
use crate::interner::{Id, Interner, SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::scope::{build_scope_for_typeparam, Object};
use crate::symbols::SymbolId;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, NamedTypeNode, Pos, SignatureNode, TypeExprNode};
use std::cell::{LazyCell, OnceCell};
use std::collections::HashMap;
use std::hash::Hash;

pub type TypeInterner = Interner<Type>;
pub type TypeId = Id<Type>;

pub type TypeArgsInterner = Interner<[TypeId]>;
pub type TypeArgsId = Id<[TypeId]>;

pub type IntSign = bool;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Type {
    Unknown,
    NamedStruct(NamedStructType),
    NamedStructInst(NamedStructInstType),
    Func(FuncType),
    Void,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(TypeId),
    ArrayPtr(TypeId),
    TypeArg(TypeArg),
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::Int(..) | Self::Float(..) | Self::Ptr(..) | Self::ArrayPtr(..)
        )
    }

    pub fn is_integral(&self) -> bool {
        matches!(self, Self::Int(..) | Self::Ptr(..) | Self::ArrayPtr(..))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Float(..))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int(..))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn as_named_struct(&self) -> Option<&NamedStructType> {
        if let Self::NamedStruct(ty) = self {
            Some(ty)
        } else {
            None
        }
    }

    pub fn as_func(&self) -> Option<&FuncType> {
        if let Self::Func(ty) = self {
            Some(ty)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedStructType {
    pub def_id: DefId,
    pub body: OnceCell<StructBody>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructBody {
    pub fields: IndexMap<SymbolId, TypeId>,
}

impl Hash for StructBody {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in &self.fields {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl Hash for NamedStructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct NamedStructInstType {
    pub def_id: DefId,
    pub type_args: TypeArgsId,
    pub body: StructBody,
}

impl Hash for NamedStructInstType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.def_id.hash(state);
        self.type_args.hash(state);
    }
}

#[derive(Default, Debug, PartialEq, Eq, Hash, Clone)]
pub struct FuncType {
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
}

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
pub struct TypeArg {
    pub index: usize,
    pub symbol: SymbolId,
}

pub fn get_type_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<'_, E>,
    node: &TypeExprNode,
) -> TypeId {
    let unknown_type = LazyCell::new(|| ctx.types.define(Type::Unknown));

    match node {
        TypeExprNode::Invalid(..) => *unknown_type,
        TypeExprNode::Named(named_type_node) => get_type_from_named_type_node(ctx, named_type_node),
        TypeExprNode::Ptr(node) => {
            let element_type_id = get_type_from_node(ctx, &node.ty);
            ctx.types.define(Type::Ptr(element_type_id))
        }
        TypeExprNode::ArrayPtr(node) => {
            let element_type_id = get_type_from_node(ctx, &node.ty);
            ctx.types.define(Type::ArrayPtr(element_type_id))
        }
        TypeExprNode::Instance(node) => {
            let Some(obj) = get_object_from_named_type_node(ctx, &node.ty) else {
                ctx.errors.expr_not_a_type(node.ty.pos());
                return *unknown_type;
            };

            let Object::GenericStruct(generic_struct) = obj else {
                ctx.errors.expr_not_a_type(node.ty.pos());
                return *unknown_type;
            };

            let required_type_param = generic_struct.node.type_params.len();
            let mut type_args: Vec<TypeId> = node
                .args
                .iter()
                .map(|node| get_type_from_node(ctx, node))
                .collect();

            if type_args.len() != required_type_param {
                ctx.errors.type_arguments_count_mismatch(
                    node.ty.pos(),
                    required_type_param,
                    type_args.len(),
                );
            }
            while type_args.len() < required_type_param {
                type_args.push(ctx.types.define(Type::Unknown));
            }

            let mut instanced_fields = IndexMap::default();
            let generic_body = generic_struct.body.get().expect("missing struct body");
            for (name, type_id) in &generic_body.fields {
                let instanced_type = substitute_generic_args(ctx, &type_args, *type_id);
                instanced_fields.insert(*name, instanced_type);
            }
            let instanced_struct_body = StructBody {
                fields: instanced_fields,
            };

            let ty = Type::NamedStructInst(NamedStructInstType {
                def_id: generic_struct.def_id,
                type_args: ctx.typeargs.define(&type_args),
                body: instanced_struct_body,
            });
            ctx.types.define(ty)
        }
        TypeExprNode::Grouped(node) => get_type_from_node(ctx, node),
    }
}

fn get_type_from_named_type_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<'_, E>,
    named_type_node: &NamedTypeNode,
) -> TypeId {
    let unknown_type = LazyCell::new(|| ctx.types.define(Type::Unknown));

    let Some(object) = get_object_from_named_type_node(ctx, named_type_node) else {
        let name = match named_type_node {
            NamedTypeNode::Ident(name_tok) => &name_tok.value,
            NamedTypeNode::Selection(.., name_tok) => &name_tok.value,
        };

        ctx.errors.undeclared_symbol(named_type_node.pos(), name);
        return *unknown_type;
    };

    let Some(type_id) = object.type_id() else {
        ctx.errors.expr_not_a_type(named_type_node.pos());
        return *unknown_type;
    };
    type_id
}

fn get_object_from_named_type_node<'ctx, E: ErrorReporter>(
    ctx: &'ctx TypeCheckContext<'ctx, E>,
    named_type_node: &NamedTypeNode,
) -> Option<&'ctx Object> {
    match named_type_node {
        NamedTypeNode::Ident(name_tok) => {
            let name = ctx.symbols.define(&name_tok.value);
            ctx.scope.lookup(name)
        }
        NamedTypeNode::Selection(package_tok, name_tok) => {
            let package_name = ctx.symbols.define(&package_tok.value);
            let obj = ctx.scope.lookup(package_name)?;
            let import_obj = obj.as_import()?;
            let package_scope = ctx.package_scopes.get(&import_obj.package)?;

            let name = ctx.symbols.define(&name_tok.value);
            package_scope.lookup(name)
        }
    }
}

pub fn substitute_generic_args<E>(
    ctx: &TypeCheckContext<'_, E>,
    args: &[TypeId],
    generic_type_id: TypeId,
) -> TypeId {
    let generic_type = ctx.types.get(generic_type_id);
    match generic_type.as_ref() {
        Type::Unknown
        | Type::NamedStruct(..)
        | Type::Void
        | Type::Bool
        | Type::Int(..)
        | Type::Float(..) => generic_type_id,
        Type::NamedStructInst(named_struct_inst_type) => {
            let typeargs = ctx.typeargs.get(named_struct_inst_type.type_args);
            let typeargs: Vec<TypeId> = typeargs
                .iter()
                .map(|type_id| substitute_generic_args(ctx, args, *type_id))
                .collect();
            ctx.types.define(Type::NamedStructInst(NamedStructInstType {
                def_id: named_struct_inst_type.def_id,
                type_args: ctx.typeargs.define(&typeargs),
                body: StructBody {
                    fields: named_struct_inst_type
                        .body
                        .fields
                        .iter()
                        .map(|(name, type_id)| {
                            (*name, substitute_generic_args(ctx, args, *type_id))
                        })
                        .collect(),
                },
            }))
        }
        Type::Func(func_type) => {
            let params: Vec<TypeId> = func_type
                .params
                .iter()
                .map(|type_id| substitute_generic_args(ctx, args, *type_id))
                .collect();
            let return_type = substitute_generic_args(ctx, args, func_type.return_type);
            ctx.types.define(Type::Func(FuncType {
                params,
                return_type,
            }))
        }
        Type::TypeArg(type_arg) => {
            if let Some(type_id) = args.get(type_arg.index) {
                *type_id
            } else {
                ctx.types.define(Type::Unknown)
            }
        }
        Type::Ptr(element_type_id) => {
            let element_type_id = substitute_generic_args(ctx, args, *element_type_id);
            ctx.types.define(Type::Ptr(element_type_id))
        }
        Type::ArrayPtr(element_type_id) => {
            let element_type_id = substitute_generic_args(ctx, args, *element_type_id);
            ctx.types.define(Type::ArrayPtr(element_type_id))
        }
    }
}

pub fn get_func_type_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<E>,
    signature: &SignatureNode,
) -> FuncType {
    let scope = build_scope_for_typeparam(ctx, &signature.type_params);
    let ctx = ctx.with_scope(scope);

    let mut param_pos = HashMap::<SymbolId, Pos>::default();
    let mut params = Vec::default();
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

        let type_id = get_type_from_node(&ctx, &param_node.ty);
        params.push(type_id);
    }

    let return_type = if let Some(expr) = &signature.return_type {
        get_type_from_node(&ctx, expr)
    } else {
        ctx.types.define(Type::Void)
    };

    FuncType {
        params,
        return_type,
    }
}

pub fn display_type_id<E>(ctx: &TypeCheckContext<'_, E>, type_id: TypeId) -> String {
    display_type(ctx, ctx.types.get(type_id).as_ref())
}

pub fn display_type<E>(ctx: &TypeCheckContext<'_, E>, ty: &Type) -> String {
    match ty {
        Type::Unknown => String::from("{unknown}"),
        Type::NamedStruct(named_type) => format!(
            "{}.{}",
            ctx.symbols.get(named_type.def_id.package),
            ctx.symbols.get(named_type.def_id.name),
        ),
        Type::NamedStructInst(named_inst_type) => {
            let mut s = format!(
                "{}.{}<",
                ctx.symbols.get(named_inst_type.def_id.package),
                ctx.symbols.get(named_inst_type.def_id.name),
            );
            let type_args = ctx.typeargs.get(named_inst_type.type_args);
            for ty in type_args.iter() {
                s.push_str(&display_type_id(ctx, *ty));
                s.push(',');
            }
            s.push('>');
            s
        }
        Type::Func(func_type) => {
            let mut s = String::from("fn(");
            for ty in &func_type.params {
                s.push_str(&display_type_id(ctx, *ty));
            }
            s.push_str("):");
            s.push_str(&display_type_id(ctx, func_type.return_type));
            s
        }
        Type::Void => String::from("void"),
        Type::Bool => String::from("bool"),
        Type::Int(sign, size) => {
            format!(
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
            FloatType::F32 => String::from("f32"),
            FloatType::F64 => String::from("f64"),
        },
        Type::Ptr(type_id) => {
            format!("*{}", display_type_id(ctx, *type_id))
        }
        Type::ArrayPtr(type_id) => {
            format!("[*]{}", display_type_id(ctx, *type_id))
        }
        Type::TypeArg(typearg) => ctx.symbols.get(typearg.symbol).as_ref().into(),
    }
}

pub fn is_type_assignable<E>(ctx: &TypeCheckContext<E>, target: TypeId, source: TypeId) -> bool {
    let target_ty = ctx.types.get(target);
    let source_ty = ctx.types.get(source);
    if target_ty.is_unknown() || source_ty.is_unknown() {
        return true;
    }

    target == source
}
