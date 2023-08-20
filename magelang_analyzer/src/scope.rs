use crate::analyze::Context;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::DefId;
use crate::symbols::SymbolId;
use crate::ty::{BitSize, FloatType, StructBody, Type, TypeId};
use indexmap::{IndexMap, IndexSet};
use magelang_syntax::{BlockStatementNode, GlobalNode, SignatureNode, StructNode};
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
    BuiltinType(TypeId),
    Struct(StructObject),
    Global(GlobalObject),
    Local(LocalObject),
    Func(FuncObject),
    GenericStruct(GenericStructObject),
    GenericFunc(GenericFuncObject),
}

#[derive(Debug)]
pub struct ImportObject {
    pub package: SymbolId,
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
    pub type_params: IndexSet<SymbolId>,
    pub node: StructNode,
    pub body: OnceCell<StructBody>,
}

#[derive(Debug)]
pub struct GenericFuncObject {
    pub def_id: DefId,
    pub signature: SignatureNode,
    pub body_node: Option<BlockStatementNode>,
    pub type_params: IndexSet<SymbolId>,
    pub ty: OnceCell<TypeId>,
    pub annotations: Rc<[Annotation]>,
}

pub fn get_builtin_scope<'ctx, E>(ctx: &Context<'ctx, E>) -> Rc<Scope> {
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
        (ctx.symbols.define("i8"), Object::BuiltinType(i8_type)),
        (ctx.symbols.define("i16"), Object::BuiltinType(i16_type)),
        (ctx.symbols.define("i32"), Object::BuiltinType(i32_type)),
        (ctx.symbols.define("i64"), Object::BuiltinType(i64_type)),
        (ctx.symbols.define("isize"), Object::BuiltinType(isize_type)),
        (ctx.symbols.define("u8"), Object::BuiltinType(u8_type)),
        (ctx.symbols.define("u16"), Object::BuiltinType(u16_type)),
        (ctx.symbols.define("u32"), Object::BuiltinType(u32_type)),
        (ctx.symbols.define("u64"), Object::BuiltinType(u64_type)),
        (ctx.symbols.define("f32"), Object::BuiltinType(f32_type)),
        (ctx.symbols.define("f64"), Object::BuiltinType(f64_type)),
        (ctx.symbols.define("usize"), Object::BuiltinType(usize_type)),
        (ctx.symbols.define("void"), Object::BuiltinType(void_type)),
        (ctx.symbols.define("bool"), Object::BuiltinType(bool_type)),
    ]));
    Rc::new(scope)
}
