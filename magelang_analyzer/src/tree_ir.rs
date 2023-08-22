use crate::analyze::TypeCheckContext;
use crate::name::{DefId, Name};
use crate::scope::Object;
use indexmap::IndexMap;
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
    StructLit(usize, Vec<Expr>),
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
    Cast(Box<Expr>, usize),
}

#[derive(Debug)]
pub struct Function {
    pub id: ObjectId,
    pub ty: FuncType,
    pub locals: Vec<TypeId>,
    pub statement: Statement,
    pub tags: Rc<[Tag]>,
}

#[derive(Debug, Clone)]
pub struct Tag {
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

pub fn build_ir<E>(ctx: &TypeCheckContext<E>) {
    let name_maps = map_names(ctx);
}

struct NameMaps {
    func_to_idx: IndexMap<Name, FunctionId>,
    global_to_idx: IndexMap<DefId, usize>,
}

fn map_names<E>(ctx: &TypeCheckContext<E>) -> NameMaps {
    let mut func_to_idx = IndexMap::<Name, FunctionId>::default();
    let mut global_to_idx = IndexMap::<DefId, usize>::default();

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
                    global_to_idx.insert(global_object.def_id, global_to_idx.len());
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
