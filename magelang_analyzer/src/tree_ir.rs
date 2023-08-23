use crate::analyze::TypeCheckContext;
use crate::expr;
use crate::interner::{SizedInterner, UnsizedInterner};
use crate::name::{DefId, Name};
use crate::scope::{Object, Scope};
use crate::statements;
use crate::symbols::SymbolInterner;
use crate::ty;
use indexmap::IndexMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct SymbolId(usize);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct TypeArgs(Vec<TypeId>);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TypeArgsId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct FunctionId(usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct GlobalId(usize);

#[derive(Debug, Default)]
pub struct Module {
    pub symbols: Vec<String>,
    pub types: Vec<Type>,
    pub typeargs: Vec<TypeArgs>,
    pub globals: Vec<Global>,
    pub functions: Vec<Function>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Struct(StructType),
    Func(FuncType),
    Void,
    Bool,
    Int(IntType),
    Float(FloatType),
    Ptr(TypeId),
    ArrayPtr(TypeId),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructType {
    pub id: ObjectId,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructField {
    pub name: SymbolId,
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncType {
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct IntType {
    pub sign: bool,
    pub size: BitSize,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum BitSize {
    I8,
    I16,
    I32,
    I64,
    ISize,
}

impl IntType {
    pub fn i8() -> Self {
        Self {
            sign: true,
            size: BitSize::I8,
        }
    }

    pub fn i16() -> Self {
        Self {
            sign: true,
            size: BitSize::I16,
        }
    }

    pub fn i32() -> Self {
        Self {
            sign: true,
            size: BitSize::I32,
        }
    }

    pub fn i64() -> Self {
        Self {
            sign: true,
            size: BitSize::I64,
        }
    }

    pub fn isize() -> Self {
        Self {
            sign: true,
            size: BitSize::ISize,
        }
    }

    pub fn u8() -> Self {
        Self {
            sign: false,
            size: BitSize::I8,
        }
    }

    pub fn u16() -> Self {
        Self {
            sign: false,
            size: BitSize::I16,
        }
    }

    pub fn u32() -> Self {
        Self {
            sign: false,
            size: BitSize::I32,
        }
    }

    pub fn u64() -> Self {
        Self {
            sign: false,
            size: BitSize::I64,
        }
    }

    pub fn usize() -> Self {
        Self {
            sign: false,
            size: BitSize::ISize,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum FloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjectId {
    Concrete {
        package_id: SymbolId,
        name_id: SymbolId,
    },
    GenericInst {
        package_id: SymbolId,
        name_id: SymbolId,
        typeargs_id: TypeArgsId,
    },
}

#[derive(Debug)]
pub struct Global {
    pub id: ObjectId,
    pub type_id: TypeId,
    pub value: Expr,
}

#[derive(Debug)]
pub struct Expr {
    pub ty: TypeId,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    ConstI8(u8),
    ConstI16(u16),
    ConstI32(u32),
    ConstI64(u64),
    ConstIsize(u64),
    ConstF32(f32),
    ConstF64(f64),
    ConstBool(bool),
    Zero(TypeId),
    StructLit(TypeId, Vec<Expr>),
    Bytes(Rc<[u8]>),

    Local(usize),
    Global(GlobalId),
    Func(FunctionId),

    GetElement(Box<Expr>, usize),
    GetElementAddr(Box<Expr>, usize),
    GetIndex(Box<Expr>, Box<Expr>),
    Deref(Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),
    ShiftLeft(Box<Expr>, Box<Expr>),
    ShiftRight(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NEq(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    GEq(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    LEq(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    BitNot(Box<Expr>),
    Not(Box<Expr>),
    Cast(Box<Expr>, TypeId),
}

#[derive(Debug)]
pub struct Function {
    pub id: ObjectId,
    pub ty: FuncType,
    pub locals: Vec<TypeId>,
    pub statement: Statement,
    pub annotations: Rc<[Annotation]>,
}

#[derive(Debug, Clone)]
pub struct Annotation {
    pub name: String,
    pub arguments: Vec<String>,
}

#[derive(Debug)]
pub enum Statement {
    Native,
    Block(Vec<Statement>),
    If(IfStatement),
    While(WhileStatement),
    Return(Option<Expr>),
    Expr(Expr),
    Assign(Expr, Expr),
    Continue,
    Break,
}

#[derive(Debug)]
pub struct IfStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
    pub else_stmt: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub cond: Expr,
    pub body: Box<Statement>,
}

impl From<DefId> for ObjectId {
    fn from(value: DefId) -> Self {
        Self::Concrete {
            package_id: SymbolId(value.package.0),
            name_id: SymbolId(value.name.0),
        }
    }
}

pub fn build_ir<E>(ctx: &TypeCheckContext<E>) -> Module {
    let name_maps = map_names(ctx);
    let type_mapper = TypeMapper::default();

    let mut globals = Vec::default();
    let mut functions = Vec::default();

    for (_, scope) in ctx.package_scopes {
        for (_, object) in scope.iter() {
            match object {
                Object::Func(func_object) => {
                    let func_type = ctx
                        .types
                        .get(*func_object.ty.get().expect("missing func type"));
                    let func_type = func_type.as_func().expect("not a func type");

                    let func = build_function_ir(
                        ctx,
                        &name_maps,
                        &type_mapper,
                        func_object.def_id.into(),
                        func_type,
                        func_object.body.get().expect("missing func body"),
                        func_object.annotations.clone(),
                    );
                    functions.push(func);
                }
                Object::GenericFunc(generic_func_object) => {
                    let Some(monomorphized) = generic_func_object.monomorphized.get() else {
                        continue;
                    };

                    for (typeargs_id, (type_id, body)) in monomorphized {
                        let func_type = ctx.types.get(*type_id);
                        let func_type = func_type.as_func().expect("not a func type");

                        let func = build_function_ir(
                            ctx,
                            &name_maps,
                            &type_mapper,
                            ObjectId::GenericInst {
                                package_id: SymbolId(generic_func_object.def_id.package.0),
                                name_id: SymbolId(generic_func_object.def_id.name.0),
                                typeargs_id: type_mapper.get_typeargs_id(ctx, *typeargs_id),
                            },
                            func_type,
                            body,
                            generic_func_object.annotations.clone(),
                        );
                        functions.push(func);
                    }
                }
                Object::Global(global_object) => {
                    let type_id = *global_object.ty.get().expect("missing global type");
                    let ir_type_id = type_mapper.get_type_id(ctx, type_id);
                    let value = build_expr_ir(
                        ctx,
                        &name_maps,
                        &type_mapper,
                        &[],
                        global_object.value.get().expect("missing global value"),
                    );
                    globals.push(Global {
                        id: global_object.def_id.into(),
                        type_id: ir_type_id,
                        value,
                    });
                }
                _ => continue,
            }
        }
    }

    let symbols = ctx
        .symbols
        .take()
        .into_iter()
        .map(|s| s.as_ref().into())
        .collect();

    let types = type_mapper
        .type_maps
        .take()
        .into_values()
        .map(|(_, ty)| ty)
        .collect();

    let typeargs = type_mapper
        .typeargs_maps
        .take()
        .into_values()
        .map(|(_, type_ids)| TypeArgs(type_ids))
        .collect();

    Module {
        symbols,
        types,
        typeargs,
        globals,
        functions,
    }
}

fn build_function_ir<E>(
    ctx: &TypeCheckContext<E>,
    name_maps: &NameMaps,
    type_mapper: &TypeMapper,
    object_id: ObjectId,
    func_type: &ty::FuncType,
    body: &statements::Statement,
    annotations: Rc<[Annotation]>,
) -> Function {
    let mut locals = Vec::default();
    for type_id in func_type.params.iter() {
        locals.push(type_mapper.get_type_id(ctx, *type_id));
    }
    let statement = build_stmt_ir(ctx, &name_maps, &type_mapper, &[], body);

    Function {
        id: object_id,
        ty: FuncType {
            parameters: func_type
                .params
                .iter()
                .map(|type_id| type_mapper.get_type_id(ctx, *type_id))
                .collect(),
            return_type: type_mapper.get_type_id(ctx, func_type.return_type),
        },
        locals,
        statement,
        annotations,
    }
}

fn build_stmt_ir<E>(
    ctx: &TypeCheckContext<E>,
    name_maps: &NameMaps,
    type_mapper: &TypeMapper,
    generic_args: &[ty::TypeId],
    stmt: &statements::Statement,
) -> Statement {
    todo!();
}

fn build_expr_ir<E>(
    ctx: &TypeCheckContext<E>,
    name_maps: &NameMaps,
    type_mapper: &TypeMapper,
    generic_args: &[ty::TypeId],
    ex: &expr::Expr,
) -> Expr {
    let type_id = type_mapper.get_type_id(ctx, ex.ty);

    let kind = match &ex.kind {
        expr::ExprKind::Invalid => unreachable!("there should be no invalid expr"),
        expr::ExprKind::ConstI8(val) => ExprKind::ConstI8(*val),
        expr::ExprKind::ConstI16(val) => ExprKind::ConstI16(*val),
        expr::ExprKind::ConstI32(val) => ExprKind::ConstI32(*val),
        expr::ExprKind::ConstI64(val) => ExprKind::ConstI64(*val),
        expr::ExprKind::ConstIsize(val) => ExprKind::ConstIsize(*val),
        expr::ExprKind::ConstF32(val) => ExprKind::ConstF32(*val),
        expr::ExprKind::ConstF64(val) => ExprKind::ConstF64(*val),
        expr::ExprKind::ConstBool(val) => ExprKind::ConstBool(*val),
        expr::ExprKind::Zero => ExprKind::Zero(type_id),
        expr::ExprKind::StructLit(type_id, values) => ExprKind::StructLit(
            type_mapper.get_type_id(
                ctx,
                ty::substitute_generic_args(ctx, generic_args, *type_id),
            ),
            values
                .iter()
                .map(|value| build_expr_ir(ctx, name_maps, type_mapper, generic_args, value))
                .collect(),
        ),
        expr::ExprKind::Bytes(val) => ExprKind::Bytes(val.clone()),
        expr::ExprKind::Local(idx) => ExprKind::Local(*idx),
        expr::ExprKind::Global(def_id) => ExprKind::Global(
            *name_maps
                .global_to_idx
                .get(def_id)
                .expect("missing global index"),
        ),
        expr::ExprKind::Func(def_id) => ExprKind::Func(
            *name_maps
                .func_to_idx
                .get(&Name::Def(*def_id))
                .expect("missing func index"),
        ),
        expr::ExprKind::FuncInst(def_id, typeargs_id) => {
            let typeargs = ctx.typeargs.get(*typeargs_id);
            let typeargs: Vec<ty::TypeId> = typeargs
                .iter()
                .map(|type_id| ty::substitute_generic_args(ctx, generic_args, *type_id))
                .collect();
            let typeargs_id = ctx.typeargs.define(&typeargs);
            ExprKind::Func(
                *name_maps
                    .func_to_idx
                    .get(&Name::Instance(*def_id, typeargs_id))
                    .expect("missing generic func instance index"),
            )
        }
        expr::ExprKind::GetElement(target, field) => ExprKind::GetElement(
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                target,
            )),
            *field,
        ),
        expr::ExprKind::GetElementAddr(target, field) => ExprKind::GetElementAddr(
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                target,
            )),
            *field,
        ),
        expr::ExprKind::GetIndex(arr, index) => ExprKind::GetIndex(
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                arr,
            )),
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                index,
            )),
        ),
        expr::ExprKind::Deref(expr) => ExprKind::Deref(Box::new(build_expr_ir(
            ctx,
            name_maps,
            type_mapper,
            generic_args,
            expr,
        ))),
        expr::ExprKind::Call(callee, arguments) => ExprKind::Call(
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                callee,
            )),
            arguments
                .iter()
                .map(|expr| build_expr_ir(ctx, name_maps, type_mapper, generic_args, expr))
                .collect(),
        ),
        expr::ExprKind::Add(a, b) => ExprKind::Add(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Sub(a, b) => ExprKind::Sub(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Mul(a, b) => ExprKind::Mul(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Div(a, b) => ExprKind::Div(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Mod(a, b) => ExprKind::Mod(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::BitOr(a, b) => ExprKind::BitOr(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::BitAnd(a, b) => ExprKind::BitAnd(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::BitXor(a, b) => ExprKind::BitXor(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::ShiftLeft(a, b) => ExprKind::ShiftLeft(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::ShiftRight(a, b) => ExprKind::ShiftRight(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::And(a, b) => ExprKind::And(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Or(a, b) => ExprKind::Or(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Eq(a, b) => ExprKind::Eq(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::NEq(a, b) => ExprKind::NEq(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Gt(a, b) => ExprKind::Gt(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::GEq(a, b) => ExprKind::GEq(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Lt(a, b) => ExprKind::Lt(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::LEq(a, b) => ExprKind::LEq(
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, a)),
            Box::new(build_expr_ir(ctx, name_maps, type_mapper, generic_args, b)),
        ),
        expr::ExprKind::Neg(val) => ExprKind::Neg(Box::new(build_expr_ir(
            ctx,
            name_maps,
            type_mapper,
            generic_args,
            val,
        ))),
        expr::ExprKind::BitNot(val) => ExprKind::BitNot(Box::new(build_expr_ir(
            ctx,
            name_maps,
            type_mapper,
            generic_args,
            val,
        ))),
        expr::ExprKind::Not(val) => ExprKind::Not(Box::new(build_expr_ir(
            ctx,
            name_maps,
            type_mapper,
            generic_args,
            val,
        ))),
        expr::ExprKind::Cast(val, type_id) => ExprKind::Cast(
            Box::new(build_expr_ir(
                ctx,
                name_maps,
                type_mapper,
                generic_args,
                val,
            )),
            type_mapper.get_type_id(
                ctx,
                ty::substitute_generic_args(ctx, generic_args, *type_id),
            ),
        ),
    };

    Expr { ty: type_id, kind }
}

struct NameMaps {
    func_to_idx: IndexMap<Name, FunctionId>,
    global_to_idx: IndexMap<DefId, GlobalId>,
}

fn map_names<E>(ctx: &TypeCheckContext<E>) -> NameMaps {
    let mut func_to_idx = IndexMap::<Name, FunctionId>::default();
    let mut global_to_idx = IndexMap::<DefId, GlobalId>::default();

    for (_, scope) in ctx.package_scopes {
        for (_, object) in scope.iter() {
            match object {
                Object::Func(func_object) => {
                    let func_id = FunctionId(func_to_idx.len());
                    func_to_idx.insert(Name::Def(func_object.def_id), func_id);
                }
                Object::GenericFunc(generic_func_object) => {
                    let Some(monomorphized) = generic_func_object.monomorphized.get() else {
                        continue;
                    };
                    for (typeargs_id, _) in monomorphized {
                        let func_id = FunctionId(func_to_idx.len());
                        func_to_idx.insert(
                            Name::Instance(generic_func_object.def_id, *typeargs_id),
                            func_id,
                        );
                    }
                }
                Object::Global(global_object) => {
                    let global_id = GlobalId(global_to_idx.len());
                    global_to_idx.insert(global_object.def_id, global_id);
                }
                _ => continue,
            }
        }
    }

    NameMaps {
        func_to_idx,
        global_to_idx,
    }
}

impl From<ty::BitSize> for BitSize {
    fn from(value: ty::BitSize) -> Self {
        match value {
            ty::BitSize::I8 => BitSize::I8,
            ty::BitSize::I16 => BitSize::I16,
            ty::BitSize::I32 => BitSize::I32,
            ty::BitSize::I64 => BitSize::I64,
            ty::BitSize::ISize => BitSize::ISize,
        }
    }
}

impl From<ty::FloatType> for FloatType {
    fn from(value: ty::FloatType) -> Self {
        match value {
            ty::FloatType::F32 => Self::F32,
            ty::FloatType::F64 => Self::F64,
        }
    }
}

#[derive(Default)]
struct TypeMapper {
    type_maps: RefCell<IndexMap<ty::TypeId, (TypeId, Type)>>,
    typeargs_maps: RefCell<IndexMap<ty::TypeArgsId, (TypeArgsId, Vec<TypeId>)>>,
}

impl TypeMapper {
    fn get_tir_type<E>(&self, ctx: &TypeCheckContext<E>, typ: &ty::Type) -> Type {
        match typ {
            ty::Type::Unknown => unreachable!("found unknown type"),
            ty::Type::TypeArg(..) => unreachable!("found type argument"),
            ty::Type::NamedStruct(named_struct) => Type::Struct(self.get_struct(
                ctx,
                named_struct.def_id.into(),
                named_struct.body.get().expect("missing struct body"),
            )),
            ty::Type::NamedStructInst(named_struct) => Type::Struct(self.get_struct(
                ctx,
                ObjectId::GenericInst {
                    package_id: SymbolId(named_struct.def_id.package.into()),
                    name_id: SymbolId(named_struct.def_id.name.into()),
                    typeargs_id: self.get_typeargs_id(ctx, named_struct.type_args),
                },
                &named_struct.body,
            )),
            ty::Type::Func(func_type) => Type::Func(FuncType {
                parameters: func_type
                    .params
                    .iter()
                    .map(|type_id| self.get_type_id(ctx, *type_id))
                    .collect(),
                return_type: self.get_type_id(ctx, func_type.return_type),
            }),
            ty::Type::Void => Type::Void,
            ty::Type::Bool => Type::Bool,
            ty::Type::Int(sign, size) => Type::Int(IntType {
                sign: *sign,
                size: (*size).into(),
            }),
            ty::Type::Float(float_type) => Type::Float((*float_type).into()),
            ty::Type::Ptr(element_type_id) => Type::Ptr(self.get_type_id(ctx, *element_type_id)),
            ty::Type::ArrayPtr(element_type_id) => {
                Type::ArrayPtr(self.get_type_id(ctx, *element_type_id))
            }
        }
    }

    fn get_struct<E>(
        &self,
        ctx: &TypeCheckContext<E>,
        id: ObjectId,
        body: &ty::StructBody,
    ) -> StructType {
        let mut fields = Vec::default();
        for (name, type_id) in &body.fields {
            let name = SymbolId(name.0);
            let ty = self.get_type_id(ctx, *type_id);
            fields.push(StructField { name, ty });
        }

        StructType { id, fields }
    }

    fn get_type_id<E>(&self, ctx: &TypeCheckContext<E>, type_id: ty::TypeId) -> TypeId {
        {
            let type_maps = self.type_maps.borrow();
            if let Some((id, _)) = type_maps.get(&type_id) {
                return *id;
            }
        }

        let id = {
            let mut type_maps = self.type_maps.borrow_mut();
            let next_id = TypeId(type_maps.len());
            type_maps.insert(type_id, (next_id, Type::Void));
            next_id
        };

        let ty = ctx.types.get(type_id);
        let ir_type = self.get_tir_type(ctx, &ty);

        let mut type_maps = self.type_maps.borrow_mut();
        type_maps.insert(type_id, (id, ir_type));
        id
    }

    fn get_typeargs_id<E>(
        &self,
        ctx: &TypeCheckContext<E>,
        typeargs_id: ty::TypeArgsId,
    ) -> TypeArgsId {
        {
            let typeargs_maps = self.typeargs_maps.borrow();
            if let Some((id, _)) = typeargs_maps.get(&typeargs_id) {
                return *id;
            }
        }

        let id = {
            let mut typeargs_maps = self.typeargs_maps.borrow_mut();
            let next_id = TypeArgsId(typeargs_maps.len());
            typeargs_maps.insert(typeargs_id, (next_id, Vec::default()));
            next_id
        };

        let typeargs = ctx.typeargs.get(typeargs_id);
        let ir_typeargs = typeargs
            .iter()
            .map(|type_id| self.get_type_id(ctx, *type_id))
            .collect::<Vec<_>>();

        let mut typeargs_maps = self.typeargs_maps.borrow_mut();
        typeargs_maps.insert(typeargs_id, (id, ir_typeargs));
        id
    }
}
