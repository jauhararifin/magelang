use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

#[derive(Debug, Clone)]
pub struct Header {
    pub package_name: String,
    pub types: Vec<TypeDecl>,
    pub vars: Vec<VarHeader>,
    pub functions: Vec<FnHeader>,
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub package_name: String,

    pub type_declarations: Vec<TypeDecl>,
    pub var_declarations: Vec<Var>,
    pub fn_declarations: Vec<FnDecl>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub header: VarHeader,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarHeader {
    pub name: String,
    pub typ: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub header: FnHeader,
    pub body: Statement,
}

#[derive(Debug, Clone)]
pub struct FnHeader {
    pub name: String,
    pub native: bool,
    pub typ: Rc<Type>, // This always in the Fn variant.
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int { signed: bool, size: u8 },
    Float { size: u8 },
    Bool,
    Void,
    Fn(FnType),
    Struct { fields: HashMap<String, Field> },
}

impl Type {
    pub fn is_number(&self) -> bool {
        matches!(self, Type::Int { signed: _, size: _ } | Type::Float { size: _ })
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int { signed: _, size: _ })
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Type::Bool)
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Type::Fn(_))
    }

    pub fn unwrap_func(&self) -> &FnType {
        if let Type::Fn(f) = self {
            f
        } else {
            panic!("type is not a function typ")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub arguments: Vec<Argument>,
    pub return_type: Option<TypePtr>,
}

#[derive(Debug, Clone)]
pub struct TypePtr(pub RefCell<Weak<Type>>);

impl PartialEq for TypePtr {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0.borrow().upgrade();
        let b = other.0.borrow().upgrade();
        if let Some(a) = a {
            if let Some(b) = b {
                return a == b;
            }
            return false;
        }
        return b.is_none();
    }
}

impl Eq for TypePtr {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub index: usize,
    pub typ: TypePtr,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub index: usize,
    pub name: String,
    pub typ: RefCell<Weak<Type>>,
}

impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        // TODO jauhararifin: check the value instead of just the pointer
        self.index == other.index && self.name == other.name && self.typ.borrow().ptr_eq(&other.typ.borrow())
    }
}

impl Eq for Argument {}

pub const VOID: Type = Type::Void;
pub const BOOL: Type = Type::Bool;
pub const I8: Type = Type::Int { signed: true, size: 8 };
pub const I16: Type = Type::Int { signed: true, size: 16 };
pub const I32: Type = Type::Int { signed: true, size: 32 };
pub const I64: Type = Type::Int { signed: true, size: 64 };
pub const U8: Type = Type::Int { signed: false, size: 8 };
pub const U16: Type = Type::Int {
    signed: false,
    size: 16,
};
pub const U32: Type = Type::Int {
    signed: false,
    size: 32,
};
pub const U64: Type = Type::Int {
    signed: false,
    size: 64,
};
pub const F32: Type = Type::Float { size: 32 };
pub const F64: Type = Type::Float { size: 64 };

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Var),
    Assign(Assign),
    Return(Return),
    If(If),
    While(While),
    Block(BlockStatement),
    Expr(Expr),
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub receiver: Expr,
    pub op: AssignOp,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Assign,
    PlusAssign,
    MinusAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub assignable: bool,
    pub typ: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident(String),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Bool(bool),
    String(String),
    Struct(StructLit),
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
    Selector(Selector),
    // TODO: add struct literal.
}

#[derive(Debug, Clone)]
pub struct StructLit {
    pub fields: Vec<FieldValue>,
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    pub index: usize,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: BinOp,
    pub a: Box<Expr>,
    pub b: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    And,
    Or,
    GT,
    LT,
    GTEq,
    LTEq,
    Eq,
    NotEq,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: UnaryOp,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitNot,
    Not,
}

#[derive(Debug, Clone)]
pub struct FunctionCall {
    pub func: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub target: Type,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Selector {
    pub source: Box<Expr>,
    pub selection: String,
    pub selection_index: usize,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}
