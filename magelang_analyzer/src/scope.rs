use crate::analyze::{Context, TypeCheckContext};
use crate::expr::Expr;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::symbols::SymbolId;
use crate::ty::{BitSize, FloatType, StructBody, Type, TypeArg, TypeId};
use indexmap::IndexMap;
use magelang_syntax::{
    BlockStatementNode, GlobalNode, Pos, SignatureNode, StructNode, TypeParameterNode,
};
use std::cell::OnceCell;
use std::rc::Rc;

pub struct Scope {
    table: IndexMap<SymbolId, Object>,
    parent: Option<Rc<Scope>>,
}

impl Scope {
    pub fn new(table: IndexMap<SymbolId, Object>) -> Self {
        Self {
            table,
            parent: None,
        }
    }

    pub fn new_child(self: &Rc<Self>, table: IndexMap<SymbolId, Object>) -> Self {
        Self {
            table,
            parent: Some(self.clone()),
        }
    }

    pub fn lookup(&self, name: SymbolId) -> Option<&Object> {
        if let Some(object) = self.table.get(&name) {
            Some(object)
        } else if let Some(ref parent) = self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &Object)> {
        self.table.iter()
    }
}

#[derive(Debug)]
pub enum Object {
    Import(ImportObject),
    Type(TypeId),
    Struct(StructObject),
    Global(GlobalObject),
    Local(LocalObject),
    Func(FuncObject),
    GenericStruct(GenericStructObject),
    GenericFunc(GenericFuncObject),
}

impl Object {
    pub fn as_struct(&self) -> Option<&StructObject> {
        if let Self::Struct(struct_object) = self {
            Some(struct_object)
        } else {
            None
        }
    }

    pub fn as_import(&self) -> Option<&ImportObject> {
        if let Self::Import(import_object) = self {
            Some(import_object)
        } else {
            None
        }
    }

    pub fn as_generic_func(&self) -> Option<&GenericFuncObject> {
        if let Self::GenericFunc(obj) = self {
            Some(obj)
        } else {
            None
        }
    }

    pub fn type_id(&self) -> Option<TypeId> {
        match self {
            Self::Struct(struct_object) => Some(struct_object.type_id),
            Self::Type(type_id) => Some(*type_id),
            _ => None,
        }
    }

    pub fn pos(&self) -> Option<Pos> {
        match self {
            Self::Import(obj) => Some(obj.pos),
            Self::Type(..) => None,
            Self::Struct(obj) => Some(obj.node.pos),
            Self::Global(obj) => Some(obj.node.pos),
            Self::Local(..) => todo!(),
            Self::Func(obj) => Some(obj.signature.pos),
            Self::GenericStruct(obj) => Some(obj.node.pos),
            Self::GenericFunc(obj) => Some(obj.signature.pos),
        }
    }
}

#[derive(Debug)]
pub struct ImportObject {
    pub package: SymbolId,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct StructObject {
    pub def_id: DefId,
    pub node: StructNode,
    pub type_id: TypeId,
}

#[derive(Debug)]
pub struct GlobalObject {
    pub def_id: DefId,
    pub node: GlobalNode,
    pub ty: OnceCell<TypeId>,
    pub value: OnceCell<Expr>,
}

#[derive(Debug)]
pub struct LocalObject {
    pub id: usize,
    pub ty: TypeId,
}

#[derive(Debug)]
pub struct FuncObject {
    pub def_id: DefId,
    pub signature: SignatureNode,
    pub body_node: Option<BlockStatementNode>,
    pub ty: OnceCell<TypeId>,
    pub annotations: Rc<[Annotation]>,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub name: String,
    pub arguments: Vec<String>,
}

#[derive(Debug)]
pub struct GenericStructObject {
    pub def_id: DefId,
    pub node: StructNode,
    pub body: OnceCell<StructBody>,
}

#[derive(Debug)]
pub struct GenericFuncObject {
    pub def_id: DefId,
    pub signature: SignatureNode,
    pub body_node: Option<BlockStatementNode>,
    pub ty: OnceCell<TypeId>,
    pub annotations: Rc<[Annotation]>,
}

pub fn get_builtin_scope<E>(ctx: &Context<'_, E>) -> Rc<Scope> {
    let i8_type = ctx.types.define(Type::Int(true, BitSize::I8));
    let i16_type = ctx.types.define(Type::Int(true, BitSize::I16));
    let i32_type = ctx.types.define(Type::Int(true, BitSize::I32));
    let i64_type = ctx.types.define(Type::Int(true, BitSize::I64));
    let isize_type = ctx.types.define(Type::Int(true, BitSize::ISize));
    let u8_type = ctx.types.define(Type::Int(false, BitSize::I8));
    let u16_type = ctx.types.define(Type::Int(false, BitSize::I16));
    let u32_type = ctx.types.define(Type::Int(false, BitSize::I32));
    let u64_type = ctx.types.define(Type::Int(false, BitSize::I64));
    let usize_type = ctx.types.define(Type::Int(false, BitSize::ISize));
    let f32_type = ctx.types.define(Type::Float(FloatType::F32));
    let f64_type = ctx.types.define(Type::Float(FloatType::F64));
    let void_type = ctx.types.define(Type::Void);
    let bool_type = ctx.types.define(Type::Bool);

    let scope = Scope::new(IndexMap::from([
        (ctx.symbols.define("i8"), Object::Type(i8_type)),
        (ctx.symbols.define("i16"), Object::Type(i16_type)),
        (ctx.symbols.define("i32"), Object::Type(i32_type)),
        (ctx.symbols.define("i64"), Object::Type(i64_type)),
        (ctx.symbols.define("isize"), Object::Type(isize_type)),
        (ctx.symbols.define("u8"), Object::Type(u8_type)),
        (ctx.symbols.define("u16"), Object::Type(u16_type)),
        (ctx.symbols.define("u32"), Object::Type(u32_type)),
        (ctx.symbols.define("u64"), Object::Type(u64_type)),
        (ctx.symbols.define("f32"), Object::Type(f32_type)),
        (ctx.symbols.define("f64"), Object::Type(f64_type)),
        (ctx.symbols.define("usize"), Object::Type(usize_type)),
        (ctx.symbols.define("void"), Object::Type(void_type)),
        (ctx.symbols.define("bool"), Object::Type(bool_type)),
    ]));
    Rc::new(scope)
}

pub fn build_scope_for_typeparam<E>(
    ctx: &TypeCheckContext<E>,
    type_params: &[TypeParameterNode],
) -> Rc<Scope> {
    let mut typeparam_table = IndexMap::<SymbolId, Object>::default();
    for (index, typeparam) in type_params.iter().enumerate() {
        let typeparam = ctx.symbols.define(&typeparam.name.value);
        let ty = ctx.types.define(Type::TypeArg(TypeArg {
            index,
            symbol: typeparam,
        }));
        let type_param_obj = Object::Type(ty);
        typeparam_table.insert(typeparam, type_param_obj);
    }
    Rc::new(ctx.scope.new_child(typeparam_table))
}
