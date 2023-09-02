use crate::analyze::TypeCheckContext;
use crate::errors::SemanticError;
use crate::interner::{Id, Interner, SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::scope::{build_scope_for_typeparam, get_object_from_path, Object};
use crate::symbols::SymbolId;
use indexmap::IndexMap;
use magelang_syntax::{ErrorReporter, PathNode, Pos, SignatureNode, TypeExprNode};
use std::cell::OnceCell;
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
    // Opaque is type with unknown representation and unknown size. This is useful to
    // represent externref type in webassembly.
    // Opaque type doesn't have zero value, and can only be get from calling native function.
    Opaque,
    Bool,
    Int(IntSign, BitSize),
    Float(FloatType),
    Ptr(TypeId),
    ArrayPtr(ArrayPtrType),
    TypeArg(TypeArg),
}

impl Type {
    pub fn is_sized(&self) -> bool {
        match self {
            Self::Unknown
            | Type::Func(..)
            | Type::Void
            | Type::Opaque
            | Type::Bool
            | Type::Int(..)
            | Type::Float(..)
            | Type::Ptr(..) => true,
            Type::NamedStruct(struct_type) => struct_type.sized,
            Type::NamedStructInst(struct_type) => struct_type.sized,
            Type::ArrayPtr(array_ptr) => array_ptr.sized,
            Type::TypeArg(typearg) => typearg.sized,
        }
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self, Self::Opaque)
    }

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
    pub sized: bool,
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
    pub sized: bool,
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
pub struct ArrayPtrType {
    pub element: TypeId,
    pub sized: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeArg {
    pub index: usize,
    pub symbol: SymbolId,
    pub sized: bool,
}

pub fn get_type_from_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<'_, E>,
    node: &TypeExprNode,
) -> TypeId {
    match node {
        TypeExprNode::Invalid(..) => ctx.types.define(Type::Unknown),
        TypeExprNode::Path(node) => get_type_from_path_node(ctx, node),
        TypeExprNode::Ptr(node) => {
            let element_type_id = get_type_from_node(ctx, &node.ty);
            ctx.types.define(Type::Ptr(element_type_id))
        }
        TypeExprNode::ArrayPtr(node) => {
            let element = get_type_from_node(ctx, &node.ty);

            if !ctx.types.get(element).is_sized() {
                todo!("report error: cannot use unsized type for array ptr");
            }

            ctx.types.define(Type::ArrayPtr(ArrayPtrType {
                element,
                sized: true,
            }))
        }
        TypeExprNode::Grouped(node) => get_type_from_node(ctx, node),
    }
}

fn get_type_from_path_node<E: ErrorReporter>(
    ctx: &TypeCheckContext<'_, E>,
    node: &PathNode,
) -> TypeId {
    let Some(object) = get_object_from_path(ctx, &node.names) else {
        return ctx.types.define(Type::Unknown);
    };

    let is_generic = !node.args.is_empty();

    match object {
        Object::Import(..)
        | Object::Global(..)
        | Object::Local(..)
        | Object::Func(..)
        | Object::GenericFunc(..) => {
            ctx.errors.expr_not_a_type(node.pos());
            ctx.types.define(Type::Unknown)
        }
        Object::Type(type_id) => {
            if is_generic {
                ctx.errors.non_generic_value(node.pos());
            }
            *type_id
        }
        Object::Struct(struct_object) => {
            if is_generic {
                ctx.errors.non_generic_value(node.pos());
            }
            struct_object.type_id
        }
        Object::GenericStruct(generic_struct) => {
            let required_type_param = generic_struct.node.type_params.len();

            let mut type_args = Vec::<TypeId>::default();
            for node in &node.args {
                let type_id = get_type_from_node(ctx, node);
                type_args.push(type_id);

                let ty = ctx.types.get(type_id);
                if !ty.is_sized() {
                    // TODO: in the future, support unsized type for type arg.
                    todo!("report error: can't use unsized type for type arguments");
                }
            }

            if type_args.len() != required_type_param {
                ctx.errors.type_arguments_count_mismatch(
                    node.pos(),
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

            let sized = instanced_fields
                .values()
                .all(|type_id| ctx.types.get(*type_id).is_sized());

            let instanced_struct_body = StructBody {
                fields: instanced_fields,
            };

            let ty = Type::NamedStructInst(NamedStructInstType {
                def_id: generic_struct.def_id,
                type_args: ctx.typeargs.define(&type_args),
                body: instanced_struct_body,
                sized,
            });
            ctx.types.define(ty)
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
        | Type::Opaque
        | Type::Bool
        | Type::Int(..)
        | Type::Float(..) => generic_type_id,
        Type::NamedStructInst(named_struct_inst_type) => {
            let typeargs = ctx.typeargs.get(named_struct_inst_type.type_args);
            let typeargs: Vec<TypeId> = typeargs
                .iter()
                .map(|type_id| substitute_generic_args(ctx, args, *type_id))
                .collect();

            let fields = named_struct_inst_type
                .body
                .fields
                .iter()
                .map(|(name, type_id)| (*name, substitute_generic_args(ctx, args, *type_id)))
                .collect::<IndexMap<_, _>>();
            let sized = fields
                .values()
                .all(|type_id| ctx.types.get(*type_id).is_sized());

            ctx.types.define(Type::NamedStructInst(NamedStructInstType {
                def_id: named_struct_inst_type.def_id,
                type_args: ctx.typeargs.define(&typeargs),
                body: StructBody { fields },
                sized,
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
        Type::ArrayPtr(array_ptr) => {
            let element = substitute_generic_args(ctx, args, array_ptr.element);
            let sized = ctx.types.get(element).is_sized();
            ctx.types
                .define(Type::ArrayPtr(ArrayPtrType { element, sized }))
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
        Type::Opaque => String::from("opaque"),
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
        Type::ArrayPtr(array_ptr) => {
            format!("[*]{}", display_type_id(ctx, array_ptr.element))
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
