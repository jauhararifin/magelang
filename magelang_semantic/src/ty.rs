use crate::def::{DefId, FuncId, GenFuncId, GenStructId, GlobalId};
use crate::symbol::SymbolId;
use indexmap::IndexMap;
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct TypeId(usize);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct FuncTypeId(TypeId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct StructTypeId(TypeId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct TypeArgsId(usize);

pub trait TypeDb {
    fn define_type(&self, value: Rc<Type>) -> TypeId;
    fn get_type(&self, type_id: TypeId) -> Rc<Type>;

    fn define_func_type(&self, func_type: FuncType) -> FuncTypeId {
        let type_id = self.define_type(Rc::new(func_type.into()));
        FuncTypeId(type_id)
    }

    fn get_func_type(&self, func_type_id: FuncTypeId) -> FuncType {
        self.get_type(func_type_id.0).as_func().unwrap()
    }

    fn define_struct_type(&self, struct_type: StructId) -> StructTypeId {
        let type_id = self.define_type(Rc::new(struct_type.into()));
        StructTypeId(type_id)
    }

    fn get_struct_type(&self, struct_type_id: StructTypeId) -> StructId {
        self.get_type(struct_type_id.0).as_struct().unwrap()
    }

    fn define_typeargs(&self, value: Rc<[TypeId]>) -> TypeId;
    fn get_typeargs(&self, value: TypeArgsId) -> Rc<[TypeId]>;

    fn get_global_type_id(&self, global_id: GlobalId) -> TypeId;
    fn get_func_type_id(&self, func_id: FuncId) -> FuncTypeId;
    fn get_generic_struct_type_id(&self, struct_gen_id: GenStructId) -> StructTypeId;
    fn get_generic_func_type_id(&self, func_gen_id: GenFuncId) -> FuncTypeId;
    fn get_generic_struct_inst_type_id(&self, struct_gen_id: GenStructId, typeargs_id: TypeArgsId) -> StructTypeId;
    fn get_generic_func_inst_type_id(&self, func_gen_id: GenFuncId, typeargs_id: TypeArgsId) -> FuncTypeId;
}

#[derive(Clone)]
pub enum Type {
    Void,
    Int(IntType),
    Float(FloatType),
    Bool,
    Pointer(TypeId),
    ArrayPtr(TypeId),
    Func(FuncType),
    Struct(StructId),
    Opaque(usize),
}

impl Type {
    pub fn as_func(&self) -> Option<FuncType> {
        match self {
            Self::Func(func_type) => Some(func_type.clone()),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<StructId> {
        match self {
            Self::Struct(struct_id) => Some(*struct_id),
            _ => None,
        }
    }
}

#[derive(Clone)]
pub struct IntType {
    sign: bool,
    size: BitSize,
}

#[derive(Clone)]
pub enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Clone)]
pub struct FloatType {
    size: FloatSize,
}

#[derive(Clone)]
pub enum FloatSize {
    F32,
    F64,
}

#[derive(Clone)]
pub struct FuncType {
    pub params: Rc<[TypeId]>,
    pub return_type: TypeId,
}

impl From<FuncType> for Type {
    fn from(value: FuncType) -> Self {
        Self::Func(value)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct StructId(DefId);

impl From<StructId> for Type {
    fn from(value: StructId) -> Self {
        Self::Struct(value)
    }
}

#[derive(Clone)]
pub struct StructField {
    fields: IndexMap<SymbolId, TypeId>,
}
