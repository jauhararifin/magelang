use crate::def::{DefDb, DefId, FuncId, GenFuncId, GenStructId, GlobalId, StructId};
use crate::error::{Loc, Location};
use crate::package::AstInfo;
use crate::scope::{get_object_from_expr, get_typeparams_scope, Object, Scope, ScopeDb, ScopeKind};
use crate::symbol::SymbolId;
use indexmap::IndexMap;
use magelang_syntax::{
    ArrayPtrExprNode, AstNode, DerefExprNode, ExprNode, IndexExprNode, ItemNode, SelectionExprNode, SignatureNode,
    Token,
};
use std::collections::HashMap;
use std::iter::zip;
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

impl From<FuncTypeId> for TypeId {
    fn from(value: FuncTypeId) -> Self {
        value.0
    }
}

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
    Unknown,
    Void,
    Int(IntType),
    Float(FloatType),
    Bool,
    Pointer(TypeId),
    ArrayPtr(TypeId),
    Func(FuncType),
    Struct(StructType),
    GenericArg(SymbolId),
}

impl Default for Type {
    fn default() -> Self {
        Self::Unknown
    }
}

impl Type {
    pub fn as_func(&self) -> Option<&FuncType> {
        match self {
            Self::Func(func_type) => Some(func_type),
            _ => None,
        }
    }

    pub fn as_struct(&self) -> Option<&StructType> {
        match self {
            Self::Struct(struct_type) => Some(struct_type),
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum StructType {
    Concrete(StructId),
    GenericInst(GenStructId, TypeArgsId),
}

impl From<StructId> for StructType {
    fn from(value: StructId) -> Self {
        StructType::Concrete(value)
    }
}

impl From<StructType> for Type {
    fn from(value: StructType) -> Self {
        Self::Struct(value)
    }
}

pub struct StructField {
    pub fields: IndexMap<SymbolId, TypeId>,
}

pub trait TypeDb: DefDb {
    fn define_type(&self, value: Rc<Type>) -> TypeId;
    fn get_type(&self, type_id: TypeId) -> Rc<Type>;

    fn define_invalid_type(&self) -> TypeId {
        self.define_type(Rc::default())
    }

    fn define_void_type(&self) -> TypeId {
        self.define_type(Rc::new(Type::Void))
    }

    fn define_ptr_type(&self, element_type_id: TypeId) -> TypeId {
        self.define_type(Rc::new(Type::Pointer(element_type_id)))
    }

    fn define_array_ptr_type(&self, element_type_id: TypeId) -> TypeId {
        self.define_type(Rc::new(Type::ArrayPtr(element_type_id)))
    }

    fn define_generic_arg_type(&self, name: SymbolId) -> TypeId {
        self.define_type(Rc::new(Type::GenericArg(name)))
    }

    fn define_func_type(&self, params: &[TypeId], return_type: TypeId) -> FuncTypeId {
        let func_type = FuncType {
            params: params.into(),
            return_type,
        };
        let type_id = self.define_type(Rc::new(func_type.into()));
        FuncTypeId(type_id)
    }

    fn get_func_type(&self, func_type_id: FuncTypeId) -> FuncType {
        self.get_type(func_type_id.0).as_func().unwrap().clone()
    }

    fn define_struct_type(&self, struct_type: StructType) -> StructTypeId {
        let type_id = self.define_type(Rc::new(struct_type.into()));
        StructTypeId(type_id)
    }

    fn get_struct_type(&self, struct_type_id: StructTypeId) -> StructType {
        self.get_type(struct_type_id.0).as_struct().unwrap().clone()
    }

    fn define_typeargs(&self, value: Rc<[TypeId]>) -> TypeArgsId;
    fn get_typeargs(&self, typeargs_id: TypeArgsId) -> Rc<[TypeId]>;

    fn get_global_type_id(&self, global_id: GlobalId) -> TypeId;
    fn get_func_type_id(&self, func_id: FuncId) -> FuncTypeId;
    fn get_struct_field(&self, struct_type_id: StructTypeId) -> Rc<StructField>;
    fn get_generic_func_type_id(&self, gen_func_id: GenFuncId) -> FuncTypeId;
    fn get_generic_func_inst_type_id(&self, gen_func_id: GenFuncId, typeargs_id: TypeArgsId) -> FuncTypeId;
}

pub fn get_global_type(db: &(impl TypeDb + ScopeDb), global_id: GlobalId) -> TypeId {
    let ast_info = db.get_package_ast(global_id.package());
    let node = db.get_ast_by_def_id(global_id.into()).expect("global id is not found");
    let global_node = node.as_global().expect("not a global node");
    let scope = db.get_package_scope(global_id.package());
    get_type_from_expr(db, &ast_info, &scope, &global_node.ty)
}

pub fn get_func_type(db: &(impl TypeDb + ScopeDb), func_id: FuncId) -> FuncTypeId {
    let ast_info = db.get_package_ast(func_id.package());
    let node = db.get_ast_by_def_id(func_id.into()).expect("func id not found");
    let scope = db.get_package_scope(func_id.package());
    let signature = match node.as_ref() {
        ItemNode::Function(func_node) => &func_node.signature,
        ItemNode::NativeFunction(signature) => signature,
        _ => unreachable!("not a function"),
    };
    func_type_by_ast(db, &ast_info, &scope, signature)
}

pub fn get_generic_func_type(db: &(impl TypeDb + ScopeDb), gen_func_id: GenFuncId) -> FuncTypeId {
    let ast_info = db.get_package_ast(gen_func_id.package());
    let node = db
        .get_ast_by_def_id(gen_func_id.into())
        .expect("generic func id not found");

    let scope = db.get_package_scope(gen_func_id.package());
    let signature = match node.as_ref() {
        ItemNode::Function(func_node) => &func_node.signature,
        ItemNode::NativeFunction(signature) => signature,
        _ => unreachable!("not a generic func"),
    };
    let scope = get_typeparams_scope(db, &ast_info, &scope, &signature.type_params);
    func_type_by_ast(db, &ast_info, &scope, signature)
}

/// func_type_by_ast returns the interned function type. This function works for both
/// generic function and normal function.
fn func_type_by_ast(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &SignatureNode,
) -> FuncTypeId {
    let mut declared_at = HashMap::<SymbolId, Location>::default();
    let mut params = Vec::default();
    for param in &node.parameters {
        let name = db.define_symbol(param.name.value.clone());
        if let Some(pos) = declared_at.get(&name) {
            db.redeclared_symbol(&param.name.value, pos, Loc::new(ast_info.path, param.name.pos));
        } else {
            declared_at.insert(name, db.get_location(ast_info, param.name.pos));
        }
        let type_id = get_type_from_expr(db, ast_info, scope, &param.type_expr);
        params.push(type_id);
    }

    let return_type = if let Some(return_type_expr) = &node.return_type {
        get_type_from_expr(db, ast_info, scope, return_type_expr)
    } else {
        db.define_void_type()
    };

    db.define_func_type(&params, return_type)
}

pub fn get_struct_field(db: &(impl TypeDb + ScopeDb), struct_type_id: StructTypeId) -> Rc<StructField> {
    let struct_type = db.get_struct_type(struct_type_id);
    let def_id: DefId = match struct_type {
        StructType::Concrete(struct_id) => struct_id.into(),
        StructType::GenericInst(gen_struct_id, _) => gen_struct_id.into(),
    };

    let ast_info = db.get_package_ast(def_id.package);
    let struct_node = db.get_ast_by_def_id(def_id).expect("struct node is not found");
    let struct_node = struct_node.as_struct().expect("not a struct node");

    let mut scope = db.get_package_scope(def_id.package);
    if let StructType::GenericInst(_, typeargs_id) = struct_type {
        let typeargs = db.get_typeargs(typeargs_id);
        let typeparams = &struct_node.type_params;
        let mut type_table = IndexMap::<SymbolId, Object>::default();
        for (typeparam, typearg) in zip(typeparams.iter(), typeargs.iter()) {
            let name = db.define_symbol(typeparam.name.value.clone());
            type_table.entry(name).or_insert(Object::Type(*typearg));
        }
        scope = scope.new_child(ScopeKind::Basic, type_table);
    }

    let mut declared_at = HashMap::<SymbolId, Location>::default();
    let mut fields = IndexMap::<SymbolId, TypeId>::default();
    for field in &struct_node.fields {
        let name = db.define_symbol(field.name.value.clone());
        let loc = Loc::new(ast_info.path, field.pos);
        if let Some(location) = declared_at.get(&name) {
            db.redeclared_symbol(&field.name.value, location.clone(), loc);
        } else {
            let location = db.get_location(&ast_info, field.pos);
            declared_at.insert(name, location);
            let type_id = get_type_from_expr(db, &ast_info, &scope, &field.type_expr);
            fields.insert(name, type_id);
        }
    }

    Rc::new(StructField { fields })
}

pub fn get_type_from_expr(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &ExprNode,
) -> TypeId {
    match node {
        ExprNode::Ident(token) => get_type_from_ident(db, ast_info, scope, token),
        ExprNode::Deref(node) => get_type_from_deref(db, ast_info, scope, node),
        ExprNode::ArrayPtr(node) => get_type_from_array_ptr(db, ast_info, scope, node),
        ExprNode::Selection(node) => get_type_from_selection(db, ast_info, scope, node),
        ExprNode::Index(node) => get_type_from_index(db, ast_info, scope, node),
        ExprNode::Grouped(node) => get_type_from_expr(db, ast_info, scope, &node.value),
        ExprNode::IntegerLiteral(..)
        | ExprNode::RealLiteral(..)
        | ExprNode::BooleanLit(..)
        | ExprNode::StringLit(..)
        | ExprNode::Binary(..)
        | ExprNode::Unary(..)
        | ExprNode::Call(..)
        | ExprNode::Cast(..)
        | ExprNode::StructLit(..) => {
            db.not_a_type(Loc::new(ast_info.path, node.get_pos()));
            db.define_invalid_type()
        }
    }
}

fn get_type_from_ident(db: &(impl TypeDb + ScopeDb), ast_info: &AstInfo, scope: &Rc<Scope>, token: &Token) -> TypeId {
    let name = db.define_symbol(token.value.clone());
    let Some(object) = scope.get(name) else {
        db.undeclared_symbol(Loc::new(ast_info.path, token.pos), &token.value);
        return db.define_invalid_type();
    };
    let Some(type_id) = object.as_type() else {
        db.not_a_type(Loc::new(ast_info.path, token.pos));
        return db.define_invalid_type();
    };
    type_id
}

fn get_type_from_deref(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &DerefExprNode,
) -> TypeId {
    let element_type_id = get_type_from_expr(db, ast_info, scope, &node.value);
    db.define_ptr_type(element_type_id)
}

fn get_type_from_array_ptr(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &ArrayPtrExprNode,
) -> TypeId {
    let element_type_id = get_type_from_expr(db, ast_info, scope, &node.element);
    db.define_array_ptr_type(element_type_id)
}

fn get_type_from_selection(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &SelectionExprNode,
) -> TypeId {
    let ExprNode::Ident(package_tok) = node.value.as_ref() else {
        db.not_a_type(Loc::new(ast_info.path, node.get_pos()));
        return db.define_invalid_type();
    };
    let package_name_tok = db.define_symbol(package_tok.value.clone());
    let Some(import) = scope.get(package_name_tok) else {
        db.not_a_type(Loc::new(ast_info.path, node.get_pos()));
        return db.define_invalid_type();
    };
    let Some(package_name) = import.as_import() else {
        db.not_a_type(Loc::new(ast_info.path, node.get_pos()));
        return db.define_invalid_type();
    };
    let package_scope = db.get_package_scope(package_name);
    let selection = db.define_symbol(node.selection.value.clone());
    let Some(object) = package_scope.get(selection) else {
        db.undeclared_symbol(Loc::new(ast_info.path, node.get_pos()), &node.selection.value);
        return db.define_invalid_type();
    };
    let Some(type_id) = object.as_type() else {
        db.not_a_type(Loc::new(ast_info.path, node.get_pos()));
        return db.define_invalid_type();
    };
    type_id
}

fn get_type_from_index(
    db: &(impl TypeDb + ScopeDb),
    ast_info: &AstInfo,
    scope: &Rc<Scope>,
    node: &IndexExprNode,
) -> TypeId {
    let object = get_object_from_expr(db, scope, &node.value);
    let Object::GenericStruct { typeparams, gen_struct_id } = object else {
        db.not_a_generic_type(Loc::new(ast_info.path, node.get_pos()));
        return db.define_invalid_type();
    };

    if typeparams.len() != node.index.len() {
        db.wrong_number_of_type_arguments(
            Loc::new(ast_info.path, node.get_pos()),
            typeparams.len(),
            node.index.len(),
        );
    }

    let typeargs: Rc<[TypeId]> = node
        .index
        .iter()
        .map(|node| get_type_from_expr(db, ast_info, scope, node))
        .collect();
    let typeargs_id = db.define_typeargs(typeargs);

    db.define_struct_type(StructType::GenericInst(gen_struct_id, typeargs_id))
        .into()
}

pub fn get_generic_func_inst_type_id(
    db: &(impl TypeDb + ScopeDb),
    gen_func_id: GenFuncId,
    typeargs_id: TypeArgsId,
) -> FuncTypeId {
    let node = db
        .get_ast_by_def_id(gen_func_id.into())
        .expect("not a generic function");
    let signature = match node.as_ref() {
        ItemNode::Function(node) => &node.signature,
        ItemNode::NativeFunction(node) => node,
        _ => unreachable!("not a generic function"),
    };

    let type_params = &signature.type_params;
    assert!(!type_params.is_empty(), "not a generic function but a normal function");

    let scope = db.get_package_scope(gen_func_id.package());
    let typeargs = db.get_typeargs(typeargs_id);
    let mut type_table = IndexMap::<SymbolId, Object>::default();
    for (typeparam, typearg) in zip(type_params.iter(), typeargs.iter()) {
        let name = db.define_symbol(typeparam.name.value.clone());
        type_table.entry(name).or_insert(Object::Type(*typearg));
    }
    let scope = scope.new_child(ScopeKind::Basic, type_table);

    let func_type_id = db.get_generic_func_type_id(gen_func_id);
    let func_type = db.get_func_type(func_type_id);
    substitute_func_type_with_typeargs(db, &scope, &func_type)
}

fn substitute_type_with_typeargs(db: &(impl TypeDb + ScopeDb), scope: &Rc<Scope>, type_id: TypeId) -> TypeId {
    let ty = db.get_type(type_id);
    match ty.as_ref() {
        Type::Unknown | Type::Void | Type::Int(..) | Type::Float(..) | Type::Bool => type_id,
        Type::Struct(struct_type) => match struct_type {
            StructType::GenericInst(gen_struct_id, typeargs_id) => {
                let typeargs = db.get_typeargs(*typeargs_id);
                let substituted_typeargs = typeargs
                    .iter()
                    .map(|type_id| substitute_type_with_typeargs(db, scope, *type_id))
                    .collect::<Vec<_>>();
                let substituted_typeargs_id = db.define_typeargs(substituted_typeargs.into());
                db.define_struct_type(StructType::GenericInst(*gen_struct_id, substituted_typeargs_id))
                    .into()
            }
            StructType::Concrete(..) => type_id,
        },
        Type::Func(func_type) => substitute_func_type_with_typeargs(db, scope, func_type).into(),
        Type::Pointer(element_type_id) => {
            db.define_ptr_type(substitute_type_with_typeargs(db, scope, *element_type_id))
        }
        Type::ArrayPtr(element_type_id) => {
            db.define_array_ptr_type(substitute_type_with_typeargs(db, scope, *element_type_id))
        }
        Type::GenericArg(name) => {
            let Some(object) = scope.get(*name) else {
                unreachable!("the generic arg is not in the scope");
            };
            if let Object::Type(substitution) = object {
                substitution
            } else {
                type_id
            }
        }
    }
}

fn substitute_func_type_with_typeargs(
    db: &(impl TypeDb + ScopeDb),
    scope: &Rc<Scope>,
    func_type: &FuncType,
) -> FuncTypeId {
    let substituted_params = func_type
        .params
        .iter()
        .map(|type_id| substitute_type_with_typeargs(db, scope, *type_id))
        .collect::<Vec<_>>();
    let substitued_return = substitute_type_with_typeargs(db, scope, func_type.return_type);
    db.define_func_type(&substituted_params, substitued_return)
}
