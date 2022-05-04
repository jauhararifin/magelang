use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct Header {
    pub functions: Vec<FnHeader>,
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub functions: Vec<FnDecl>,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub header: FnHeader,
    pub body: Option<Statement>, // native functions don't have body.
}

#[derive(Debug, Clone)]
pub struct FnHeader {
    pub name: String,
    pub native: bool,
    pub typ: FnType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    Void,
    Int(IntType),
    Float(FloatType),
    Fn(FnType),
}

impl Type {
    pub fn is_number(&self) -> bool {
        matches!(self, Type::Int(_) | Type::Float(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Type::Int(_))
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
            panic!("type is not a function type")
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntType {
    pub signed: bool,
    pub size: u8,
}

impl IntType {
    pub fn signed(size: u8) -> Self {
        Self { signed: true, size }
    }
    pub fn unsigned(size: u8) -> Self {
        Self { signed: false, size }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FloatType {
    pub size: u8,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub native: bool,
    pub arguments: Vec<Argument>,
    pub return_type: Option<Rc<Type>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Argument {
    pub index: usize,
    pub name: String,
    pub type_kind: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub fields: HashMap<String, Field>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub index: usize,
    pub type_kind: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Var(Var),
    Assign(Assign),
    Return(Return),
    If(If),
    While(While),
    Block(BlockStatement),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Var {
    pub header: VarHeader,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarHeader {
    pub name: String,
    pub type_kind: Type,
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
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub assignable: bool,
    pub type_kind: Type,
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
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
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
