use crate::ast::{
    ExprNode, FunctionNode, ImportNode, ItemNode, Location, SignatureNode, StructNode, TypeParameterNode,
};
use crate::def::{DefId, FuncId, GenFuncId, GenStructId, GlobalId, StructId};
use crate::error::ErrorAccumulator;
use crate::package::{PackageDb, PackageId};
use crate::symbol::{SymbolDb, SymbolId};
use crate::ty::{TypeDb, TypeId};
use crate::value::value_from_string_lit;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Scope {
    pub parent: Option<Rc<Scope>>,
    pub kind: ScopeKind,
    pub symbols: IndexMap<SymbolId, Object>,
}

pub enum ScopeKind {
    Function(TypeId),
    Basic,
    Loop,
}

impl Scope {
    pub fn new(kind: ScopeKind, symbols: IndexMap<SymbolId, Object>) -> Rc<Self> {
        Rc::new(Self {
            parent: None,
            kind,
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

    pub fn return_type(&self) -> Option<TypeId> {
        if let ScopeKind::Function(ref ret_type) = self.kind {
            Some(*ret_type)
        } else if let Some(ref parent) = self.parent {
            parent.return_type()
        } else {
            None
        }
    }

    pub fn inside_loop(&self) -> bool {
        if let ScopeKind::Loop = self.kind {
            true
        } else if let Some(ref parent) = self.parent {
            parent.inside_loop()
        } else {
            false
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &Object)> {
        self.symbols.iter()
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Object {
    Invalid,
    Import(PackageId),
    Local {
        ty: TypeId,
        idx: usize,
    },
    Global(GlobalId),
    Func {
        func_id: FuncId,
        is_native: bool,
    },
    Type(TypeId),
    GenericStruct {
        typeparams: Rc<[SymbolId]>,
        gen_struct_id: GenStructId,
    },
    GenericFunc {
        typeparams: Rc<[SymbolId]>,
        gen_func_id: GenFuncId,
        is_native: bool,
    },
}

impl Default for Object {
    fn default() -> Self {
        Self::Invalid
    }
}

impl Object {
    pub fn as_import(&self) -> Option<PackageId> {
        if let Self::Import(name) = self {
            Some(*name)
        } else {
            None
        }
    }

    pub fn as_type(&self) -> Option<TypeId> {
        if let Self::Type(type_id) = self {
            Some(*type_id)
        } else {
            None
        }
    }
}

pub trait ScopeDb: PackageDb + TypeDb + SymbolDb + ErrorAccumulator {
    fn get_builtin_scope(&self) -> Rc<Scope>;
    fn get_package_scope(&self, package_id: PackageId) -> Rc<Scope>;
}

pub fn get_package_scope(db: &impl ScopeDb, package_id: PackageId) -> Rc<Scope> {
    let ast_info = db.get_package_ast(package_id);
    let mut symbol_table = IndexMap::<SymbolId, Object>::default();
    for item in ast_info.root.items.iter() {
        let name = db.define_symbol(item.name().into());
        let def_id = DefId::new(package_id, name);
        if symbol_table.contains_key(&name) {
            continue;
        }

        let object = match item.as_ref() {
            ItemNode::Import(node) => get_imported_package(db, node).map(Object::Import).unwrap_or_default(),
            ItemNode::Struct(node) => object_from_struct_node(db, def_id, node),
            ItemNode::Global(..) => Object::Global(def_id.into()),
            ItemNode::Function(node) => object_from_func_node(db, def_id, node),
            ItemNode::NativeFunction(node) => object_from_signature_node(db, def_id, node),
        };

        symbol_table.insert(name, object);
    }

    let builtin_scope = db.get_builtin_scope();
    builtin_scope.new_child(ScopeKind::Basic, symbol_table)
}

fn object_from_struct_node(db: &impl ScopeDb, def_id: DefId, node: &StructNode) -> Object {
    if node.type_params.is_empty() {
        let struct_id: StructId = def_id.into();
        let type_id = db.define_struct_type(struct_id.into()).into();
        Object::Type(type_id)
    } else {
        let typeparams = node
            .type_params
            .iter()
            .map(|node| db.define_symbol(node.name.value.clone()))
            .collect();
        Object::GenericStruct {
            typeparams,
            gen_struct_id: def_id.into(),
        }
    }
}

fn object_from_func_node(db: &impl ScopeDb, def_id: DefId, node: &FunctionNode) -> Object {
    if node.signature.type_params.is_empty() {
        Object::Func {
            func_id: def_id.into(),
            is_native: false,
        }
    } else {
        let typeparams = node
            .signature
            .type_params
            .iter()
            .map(|node| db.define_symbol(node.name.value.clone()))
            .collect();
        Object::GenericFunc {
            typeparams,
            gen_func_id: def_id.into(),
            is_native: false,
        }
    }
}

fn object_from_signature_node(db: &impl ScopeDb, def_id: DefId, node: &SignatureNode) -> Object {
    if node.type_params.is_empty() {
        Object::Func {
            func_id: def_id.into(),
            is_native: true,
        }
    } else {
        let typeparams = node
            .type_params
            .iter()
            .map(|node| db.define_symbol(node.name.value.clone()))
            .collect();
        Object::GenericFunc {
            typeparams,
            gen_func_id: def_id.into(),
            is_native: true,
        }
    }
}

fn get_imported_package(db: &impl ScopeDb, import_node: &ImportNode) -> Option<PackageId> {
    let package_name = value_from_string_lit(&import_node.path.value)?;
    let Ok(package_name) = String::from_utf8(package_name.as_ref().into()) else {
        db.invalid_utf8_package(import_node.path.loc);
        return None;
    };
    let package_name = db.define_symbol(package_name.into());
    Some(package_name.into())
}

pub fn get_object_from_expr(db: &impl ScopeDb, scope: &Rc<Scope>, node: &ExprNode) -> Object {
    match node {
        ExprNode::Ident(token) => {
            let name = db.define_symbol(token.value.clone());
            scope.get(name).unwrap_or(Object::Invalid)
        }
        ExprNode::Selection(selection_node) => {
            let ExprNode::Ident(token) = selection_node.value.as_ref() else { return Object::Invalid; };
            let package_name = db.define_symbol(token.value.clone());
            let package_object = scope.get(package_name).unwrap_or(Object::Invalid);
            let Object::Import(package_name) = package_object else { return Object::Invalid };
            let package_scope = db.get_package_scope(package_name);
            let name = db.define_symbol(selection_node.selection.value.clone());
            package_scope.get(name).unwrap_or(Object::Invalid)
        }
        _ => Object::Invalid,
    }
}

pub fn get_typeparams_scope(
    db: &(impl ScopeDb + TypeDb),
    scope: &Rc<Scope>,
    type_params: &[TypeParameterNode],
) -> Rc<Scope> {
    let mut declared_at = HashMap::<SymbolId, Location>::default();
    let mut symbol_table = IndexMap::<SymbolId, Object>::default();
    for type_param in type_params {
        let name = db.define_symbol(type_param.name.value.clone());
        if let Some(location) = declared_at.get(&name) {
            let loc = type_param.name.loc;
            db.redeclared_symbol(&type_param.name.value, location, loc);
        } else {
            let location = db.get_location(type_param.name.loc);
            declared_at.insert(name, location);
            let type_id = db.define_generic_arg_type(name);
            symbol_table.insert(name, Object::Type(type_id));
        }
    }

    scope.new_child(ScopeKind::Basic, symbol_table)
}
