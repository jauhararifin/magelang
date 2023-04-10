use indexmap::IndexMap;
use magelang_common::{SymbolId, SymbolLoader};
use magelang_semantic::{IntType, Type, TypeId, TypeLoader};
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
}

pub const I64: &str = "i64";

impl Scope {
    pub fn global(type_loader: &TypeLoader, symbol_loader: &SymbolLoader) -> Rc<Self> {
        let mut symbols = IndexMap::<SymbolId, Object>::new();

        let i64_id = symbol_loader.declare_symbol(I64);

        symbols.insert(
            i64_id,
            Object::Type(type_loader.declare_type(Type::Int(IntType {
                bitsize: 64,
                signed: true,
            }))),
        );

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
}
