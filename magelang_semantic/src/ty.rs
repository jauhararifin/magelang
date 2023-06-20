use crate::def::{DefDb, FuncId, GenFuncId, GenStructId, GlobalId, StructId};
use crate::scope::{get_object_from_expr, Object, Scope, ScopeDb};
use crate::symbol::SymbolId;
use indexmap::IndexMap;
use magelang_syntax::{ArrayPtrExprNode, DerefExprNode, ExprNode, IndexExprNode, SelectionExprNode, Token};
use std::rc::Rc;

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct TypeId(usize);

impl From<usize> for TypeId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<TypeId> for usize {
    fn from(value: TypeId) -> Self {
        value.0
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct FuncTypeId(TypeId);

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct StructTypeId(TypeId);

impl From<StructTypeId> for TypeId {
    fn from(value: StructTypeId) -> Self {
        value.0
    }
}

#[derive(PartialEq, Eq, Clone, Hash, Debug, Copy)]
pub struct TypeArgsId(usize);

impl From<usize> for TypeArgsId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<TypeArgsId> for usize {
    fn from(value: TypeArgsId) -> Self {
        value.0
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum Type {
    Invalid,
    Void,
    Int(IntType),
    Float(FloatType),
    Bool,
    Pointer(TypeId),
    ArrayPtr(TypeId),
    Func(FuncType),
    Struct(StructId),
    Opaque(SymbolId),
}

impl Default for Type {
    fn default() -> Self {
        Self::Invalid
    }
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct IntType {
    sign: bool,
    size: BitSize,
}

impl IntType {
    pub fn new(sign: bool, size: BitSize) -> Self {
        Self { sign, size }
    }

    pub fn isize() -> Self {
        Self::new(true, BitSize::ISize)
    }

    pub fn i8() -> Self {
        Self::new(true, BitSize::I8)
    }

    pub fn i16() -> Self {
        Self::new(true, BitSize::I16)
    }

    pub fn i32() -> Self {
        Self::new(true, BitSize::I32)
    }

    pub fn i64() -> Self {
        Self::new(true, BitSize::I64)
    }

    pub fn usize() -> Self {
        Self::new(false, BitSize::ISize)
    }

    pub fn u8() -> Self {
        Self::new(false, BitSize::I8)
    }

    pub fn u16() -> Self {
        Self::new(false, BitSize::I16)
    }

    pub fn u32() -> Self {
        Self::new(false, BitSize::I32)
    }

    pub fn u64() -> Self {
        Self::new(false, BitSize::I64)
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct FloatType {
    size: FloatSize,
}

impl FloatType {
    pub fn f32() -> Self {
        Self { size: FloatSize::F32 }
    }

    pub fn f64() -> Self {
        Self { size: FloatSize::F64 }
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum FloatSize {
    F32,
    F64,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct FuncType {
    pub params: Rc<[TypeId]>,
    pub return_type: TypeId,
}

impl From<FuncType> for Type {
    fn from(value: FuncType) -> Self {
        Self::Func(value)
    }
}

impl From<StructId> for Type {
    fn from(value: StructId) -> Self {
        Self::Struct(value)
    }
}

#[derive(Clone)]
pub struct StructField {
    fields: IndexMap<SymbolId, TypeId>,
}

pub trait TypeDb: DefDb {
    fn define_type(&self, value: Rc<Type>) -> TypeId;
    fn get_type(&self, type_id: TypeId) -> Rc<Type>;

    fn define_invalid_type(&self) -> TypeId {
        self.define_type(Rc::default())
    }

    fn define_ptr_type(&self, element_type_id: TypeId) -> TypeId {
        self.define_type(Rc::new(Type::Pointer(element_type_id)))
    }

    fn define_array_ptr_type(&self, element_type_id: TypeId) -> TypeId {
        self.define_type(Rc::new(Type::ArrayPtr(element_type_id)))
    }

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

    fn define_typeargs(&self, value: Rc<[TypeId]>) -> TypeArgsId;
    fn get_typeargs(&self, typeargs_id: TypeArgsId) -> Rc<[TypeId]>;

    fn get_global_type_id(&self, global_id: GlobalId) -> TypeId;
    fn get_func_type_id(&self, func_id: FuncId) -> FuncTypeId;
    fn get_generic_struct_type_id(&self, struct_gen_id: GenStructId) -> StructTypeId;
    fn get_generic_func_type_id(&self, func_gen_id: GenFuncId) -> FuncTypeId;
    fn get_generic_struct_inst_type_id(&self, struct_gen_id: GenStructId, typeargs_id: TypeArgsId) -> StructTypeId;
    fn get_generic_func_inst_type_id(&self, func_gen_id: GenFuncId, typeargs_id: TypeArgsId) -> FuncTypeId;
}

pub fn get_global_type(db: &(impl TypeDb + ScopeDb), global_id: GlobalId) -> TypeId {
    let node = db.get_ast_by_def_id(global_id.into()).expect("global id is not found");
    let global_node = node.as_global().expect("not a global node");
    let scope = db.get_package_scope(global_id.package());
    get_type_from_ast(db, &scope, &global_node.ty)
}

pub fn get_type_from_ast(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, node: &ExprNode) -> TypeId {
    match node {
        ExprNode::Ident(token) => get_type_from_ident(db, scope, token),
        ExprNode::Deref(node) => get_type_from_deref(db, scope, node),
        ExprNode::ArrayPtr(node) => get_type_from_array_ptr(db, scope, node),
        ExprNode::Selection(node) => get_type_from_selection(db, scope, node),
        ExprNode::Index(node) => get_type_from_index(db, scope, node),
        ExprNode::Grouped(node) => get_type_from_ast(db, scope, &node.value),
        ExprNode::IntegerLiteral(..)
        | ExprNode::RealLiteral(..)
        | ExprNode::BooleanLit(..)
        | ExprNode::StringLit(..)
        | ExprNode::Binary(..)
        | ExprNode::Unary(..)
        | ExprNode::Call(..)
        | ExprNode::Cast(..)
        | ExprNode::StructLit(..) => {
            db.not_a_type(todo!());
            db.define_invalid_type()
        }
    }
}

fn get_type_from_ident(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, token: &Token) -> TypeId {
    let name = db.define_symbol(token.value.clone());
    let Some(object) = scope.get(name) else {
        db.undeclared_symbol(token);
        return db.define_invalid_type();
    };
    let Some(type_id) = object.as_type() else {
        db.not_a_type(todo!());
        return db.define_invalid_type();
    };
    type_id
}

fn get_type_from_deref(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, node: &DerefExprNode) -> TypeId {
    let element_type_id = get_type_from_ast(db, scope, &node.value);
    db.define_ptr_type(element_type_id)
}

fn get_type_from_array_ptr(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, node: &ArrayPtrExprNode) -> TypeId {
    let element_type_id = get_type_from_ast(db, scope, &node.element);
    db.define_array_ptr_type(element_type_id)
}

fn get_type_from_selection(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, node: &SelectionExprNode) -> TypeId {
    let ExprNode::Ident(package_tok) = node.value.as_ref() else {
        db.not_a_type(todo!());
        return db.define_invalid_type();
    };
    let package_name_tok = db.define_symbol(package_tok.value.clone());
    let Some(import) = scope.get(package_name_tok) else {
        db.not_a_type(todo!());
        return db.define_invalid_type();
    };
    let Some(package_name) = import.as_import() else {
        db.not_a_type(todo!());
        return db.define_invalid_type();
    };
    let package_scope = db.get_package_scope(package_name);
    let selection = db.define_symbol(node.selection.value.clone());
    let Some(object) = package_scope.get(selection) else {
        db.undeclared_symbol(&node.selection);
        return db.define_invalid_type();
    };
    let Some(type_id) = object.as_type() else {
        db.not_a_type(todo!());
        return db.define_invalid_type();
    };
    type_id
}

fn get_type_from_index(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, node: &IndexExprNode) -> TypeId {
    let object = get_object_from_expr(db, scope, &node.value);
    let Object::GenericStruct { typeparams, struct_id } = object else {
        db.not_a_generic_type(todo!());
        return db.define_invalid_type();
    };

    if typeparams.len() != node.index.len() {
        db.wrong_number_of_type_arguments(todo!(), typeparams.len(), node.index.len());
    }

    let typeargs: Rc<[TypeId]> = node
        .index
        .iter()
        .map(|node| get_type_from_ast(db, scope, node))
        .collect();
    let typeargs_id = db.define_typeargs(typeargs);

    db.get_generic_struct_inst_type_id(struct_id, typeargs_id).into()
}
