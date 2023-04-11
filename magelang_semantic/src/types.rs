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

pub trait TypeDisplay {
    fn display(&self, type_loader: &TypeLoader) -> String;
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum Type {
    Invalid,
    Void,
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    F32,
    F64,
    Bool,
    Func(FuncType),
}

impl Type {
    pub fn is_assignable_with(&self, other: &Self) -> bool {
        self == other
    }

    pub fn is_bool(&self) -> bool {
        if let Self::Bool = self {
            true
        } else {
            false
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Self::I64 | Self::I32 | Self::I16 | Self::I8 | Self::U64 | Self::U32 | Self::U16 | Self::U8 => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Self::I64
            | Self::I32
            | Self::I16
            | Self::I8
            | Self::U64
            | Self::U32
            | Self::U16
            | Self::U8
            | Self::F64
            | Self::F32 => true,
            _ => false,
        }
    }
}

impl TypeDisplay for Type {
    fn display(&self, type_loader: &TypeLoader) -> String {
        match self {
            Self::Invalid => String::from("INVALID"),
            Self::Void => String::from("void"),
            Self::I64 => String::from("i64"),
            Self::I32 => String::from("i32"),
            Self::I16 => String::from("i16"),
            Self::I8 => String::from("i8"),
            Self::U64 => String::from("u64"),
            Self::U32 => String::from("u32"),
            Self::U16 => String::from("u16"),
            Self::U8 => String::from("u8"),
            Self::F32 => String::from("f32"),
            Self::F64 => String::from("f64"),
            Self::Bool => String::from("boolean"),
            Self::Func(func_ty) => func_ty.display(type_loader),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub struct FuncType {
    pub parameters: Vec<TypeId>,
    pub return_type: Option<TypeId>,
}

impl TypeDisplay for FuncType {
    fn display(&self, type_loader: &TypeLoader) -> String {
        let mut s = String::from("func(");
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                s.push(',');
            }
            s.push_str(&type_loader.get_type(*param).unwrap().display(type_loader));
        }
        s.push(')');
        if let Some(ref ret_type) = self.return_type {
            s.push(':');
            s.push_str(&type_loader.get_type(*ret_type).unwrap().display(type_loader));
        }
        s
    }
}
