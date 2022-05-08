use std::fmt::Debug;

#[derive(Clone)]
pub struct Local {
    pub typ: Type,
    pub data: usize, // pointer to the data.
}

impl Debug for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match &self.typ {
                Type::I64 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const i64)),
                Type::F32 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const f32)),
                Type::F64 => write!(f, "{:?}@{} {}", self.typ, self.data, &*(self.data as *const f64)),
                Type::Array(typ) => write!(f, "array {:?}@{}", typ.elem.as_ref(), self.data),
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    I64,
    F32,
    F64,
    Array(ArrayType),
}

#[derive(Debug, Clone)]
pub struct ArrayType {
    pub size: u64,
    pub dims: Vec<u64>,
    pub elem: Box<Type>,
}

pub trait IntoType {
    fn into() -> Type;
}

impl IntoType for i64 {
    fn into() -> Type {
        Type::I64
    }
}

impl IntoType for u64 {
    fn into() -> Type {
        Type::I64
    }
}

impl IntoType for f32 {
    fn into() -> Type {
        Type::F32
    }
}

impl IntoType for f64 {
    fn into() -> Type {
        Type::F64
    }
}

impl Type {
    pub fn size(&self) -> u64 {
        match &self {
            Type::I64 => 8,
            Type::F32 => 4,
            Type::F64 => 8,
            Type::Array(_) => std::mem::size_of::<usize>() as u64,
        }
    }

    pub fn unwrap_array(&self) -> &ArrayType {
        if let Type::Array(array_type) = self {
            array_type
        } else {
            panic!("the type is not an array type");
        }
    }
}
