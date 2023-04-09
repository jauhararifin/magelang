use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(usize);

pub struct TypeLoader {
    type_to_id: RefCell<HashMap<Rc<Type>, TypeId>>,
    id_to_type: RefCell<HashMap<TypeId, Rc<Type>>>,
}

impl TypeLoader {
    pub fn new() -> Self {
        Self {
            type_to_id: RefCell::new(HashMap::new()),
            id_to_type: RefCell::new(HashMap::new()),
        }
    }

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

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Invalid,
    Void,
    Int(IntType),
    Func(FuncType),
}

impl Type {
    pub fn is_assignable_with(&self, other: &Self) -> bool {
        self == other
    }
}

impl TypeDisplay for Type {
    fn display(&self, type_loader: &TypeLoader) -> String {
        match self {
            Self::Invalid => String::from("INVALID"),
            Self::Void => String::from("void"),
            Self::Int(int_ty) => int_ty.display(type_loader),
            Self::Func(func_ty) => func_ty.display(type_loader),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct IntType {
    pub bitsize: usize,
    pub signed: bool,
}

impl TypeDisplay for IntType {
    fn display(&self, _type_loader: &TypeLoader) -> String {
        format!("{}{}", if self.signed { "i" } else { "u" }, self.bitsize)
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FuncType {
    pub parameters: Vec<TypeId>,
    pub return_type: Option<TypeId>,
}

impl TypeDisplay for FuncType {
    fn display(&self, type_loader: &TypeLoader) -> String {
        let mut s = String::from("func(");
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                s.push_str(",");
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
