use indexmap::IndexMap;
use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{Type, TypeId, TypeLoader};
use std::rc::Rc;

pub struct Scope {
    parent: Option<Rc<Scope>>,
    kind: ScopeKind,
    symbols: IndexMap<SymbolId, Object>,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Object {
    Package(SymbolId),
    Func(TypeId),
    Local(TypeId, usize),
    Type(TypeId),
}

impl Object {
    pub fn as_type(&self) -> Option<TypeId> {
        if let Object::Type(ty) = self {
            Some(*ty)
        } else {
            None
        }
    }
}

pub enum ScopeKind {
    Global,
    Package(SymbolId),
    Function(Option<TypeId>),
    Basic,
    Loop,
}

pub const ISIZE: &str = "isize";
pub const I64: &str = "i64";
pub const I32: &str = "i32";
pub const I16: &str = "i16";
pub const I8: &str = "i8";
pub const USIZE: &str = "usize";
pub const U64: &str = "u64";
pub const U32: &str = "u32";
pub const U16: &str = "u16";
pub const U8: &str = "u8";
pub const BOOL: &str = "bool";
pub const F64: &str = "f64";
pub const F32: &str = "f32";

impl Scope {
    pub fn global(type_loader: &TypeLoader, symbol_loader: &SymbolLoader) -> Rc<Self> {
        let mut symbols = IndexMap::<SymbolId, Object>::new();

        let isize_id = symbol_loader.declare_symbol(ISIZE);
        let i64_id = symbol_loader.declare_symbol(I64);
        let i32_id = symbol_loader.declare_symbol(I32);
        let i16_id = symbol_loader.declare_symbol(I16);
        let i8_id = symbol_loader.declare_symbol(I8);
        let usize_id = symbol_loader.declare_symbol(USIZE);
        let u64_id = symbol_loader.declare_symbol(U64);
        let u32_id = symbol_loader.declare_symbol(U32);
        let u16_id = symbol_loader.declare_symbol(U16);
        let u8_id = symbol_loader.declare_symbol(U8);
        let bool_id = symbol_loader.declare_symbol(BOOL);
        let f64_id = symbol_loader.declare_symbol(F64);
        let f32_id = symbol_loader.declare_symbol(F32);

        symbols.insert(isize_id, Object::Type(type_loader.declare_type(Type::Isize)));
        symbols.insert(i64_id, Object::Type(type_loader.declare_type(Type::I64)));
        symbols.insert(i32_id, Object::Type(type_loader.declare_type(Type::I32)));
        symbols.insert(i16_id, Object::Type(type_loader.declare_type(Type::I16)));
        symbols.insert(i8_id, Object::Type(type_loader.declare_type(Type::I8)));
        symbols.insert(usize_id, Object::Type(type_loader.declare_type(Type::Usize)));
        symbols.insert(u64_id, Object::Type(type_loader.declare_type(Type::U64)));
        symbols.insert(u32_id, Object::Type(type_loader.declare_type(Type::U32)));
        symbols.insert(u16_id, Object::Type(type_loader.declare_type(Type::U16)));
        symbols.insert(u8_id, Object::Type(type_loader.declare_type(Type::U8)));
        symbols.insert(bool_id, Object::Type(type_loader.declare_type(Type::Bool)));
        symbols.insert(f64_id, Object::Type(type_loader.declare_type(Type::F64)));
        symbols.insert(f32_id, Object::Type(type_loader.declare_type(Type::F32)));

        Rc::new(Self {
            parent: None,
            kind: ScopeKind::Global,
            symbols,
        })
    }

    pub fn new_child(self: &Rc<Self>, kind: ScopeKind, symbols: IndexMap<SymbolId, Object>) -> Rc<Self> {
        Rc::new(Self {
            parent: Some(self.clone()),
            kind,
            symbols,
        })
    }

    pub fn get(&self, name: SymbolId) -> Option<Object> {
        if let Some(t) = self.symbols.get(&name) {
            Some(t.clone())
        } else if let Some(ref parent) = self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    pub fn package_name(&self) -> Option<SymbolId> {
        if let ScopeKind::Package(name) = self.kind {
            Some(name)
        } else if let Some(ref parent) = self.parent {
            parent.package_name()
        } else {
            None
        }
    }

    pub fn return_type(&self) -> Option<TypeId> {
        if let ScopeKind::Function(ref ret_type) = self.kind {
            *ret_type
        } else if let Some(ref parent) = self.parent {
            parent.return_type()
        } else {
            None
        }
    }

    pub fn is_inside_loop(&self) -> bool {
        if let ScopeKind::Loop = self.kind {
            true
        } else if let Some(ref parent) = self.parent {
            parent.is_inside_loop()
        } else {
            false
        }
    }
}
