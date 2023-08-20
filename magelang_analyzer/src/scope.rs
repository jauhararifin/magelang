use crate::name::DefId;
use crate::symbols::SymbolId;
use crate::ty::{StructBody, TypeId};
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
    Type(TypeId),
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

