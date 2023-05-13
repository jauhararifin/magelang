use magelang_common::SymbolId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(usize);

pub struct TypeLoader {
    type_to_id: RefCell<HashMap<Rc<Type>, TypeId>>,
    id_to_type: RefCell<HashMap<TypeId, Rc<Type>>>,
}

impl Default for TypeLoader {
    fn default() -> Self {
        Self {
            type_to_id: RefCell::new(HashMap::new()),
            id_to_type: RefCell::new(HashMap::new()),
        }
    }
}

impl TypeLoader {
    pub fn declare_type(&self, ty: Type) -> TypeId {
        let mut type_to_id = self.type_to_id.borrow_mut();
        let mut id_to_type = self.id_to_type.borrow_mut();

        let ty = Rc::new(ty);
        if let Some(id) = type_to_id.get(&ty) {
            *id
        } else {
            let id = TypeId(type_to_id.len());
            type_to_id.insert(ty.clone(), id);
            id_to_type.insert(id, ty);
            id
        }
    }

    pub fn get_type(&self, type_id: TypeId) -> Option<Rc<Type>> {
        self.id_to_type.borrow().get(&type_id).map(Rc::clone)
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Type {
    Invalid,
    Void,
    Isize,
    I64,
    I32,
    I16,
    I8,
    Usize,
    U64,
    U32,
    U16,
    U8,
    F32,
    F64,
    Bool,
    Func(FuncType),
    Slice(SliceType),
    Pointer(PointerType),
    ArrayPtr(ArrayPtrType),
    Opaque(SymbolId),
}

impl Type {
    pub fn is_assignable_with(&self, other: &Self) -> bool {
        if self.is_invalid() || other.is_invalid() {
            return true;
        }
        self == other
    }

    pub fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn is_int(&self) -> bool {
        matches!(
            self,
            Self::I64
                | Self::I32
                | Self::I16
                | Self::I8
                | Self::U64
                | Self::U32
                | Self::U16
                | Self::U8
                | Self::Usize
                | Self::Isize
        )
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::Isize
                | Self::I64
                | Self::I32
                | Self::I16
                | Self::I8
                | Self::Usize
                | Self::U64
                | Self::U32
                | Self::U16
                | Self::U8
                | Self::F64
                | Self::F32
        )
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Isize
                | Self::I64
                | Self::I32
                | Self::I16
                | Self::I8
                | Self::Usize
                | Self::U64
                | Self::U32
                | Self::U16
                | Self::U8
                | Self::F64
                | Self::F32
                | Self::Slice(..)
                | Self::Pointer(..)
                | Self::ArrayPtr(..)
        )
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct FuncType {
    pub type_parameters: Vec<SymbolId>,
    pub parameters: Vec<TypeId>,
    pub return_type: Option<TypeId>,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct SliceType {
    pub element_type: TypeId,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct PointerType {
    pub element_type: TypeId,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct ArrayPtrType {
    pub element_type: TypeId,
}
