use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Root {
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
    pub name: String,
    pub typ: Type,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub name: String,
    pub native: bool,
    pub params: Vec<Param>,
    pub ret_type: Option<Type>,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Int { signed: bool, size: u8 },
    Float { size: u8 },
    Bool,
    Struct { fields: HashMap<String, Field> },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    index: usize,
    typ: Type,
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
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: BlockStatement,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub assignable: bool,
    pub typ: Type,
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
    Binary(Binary),
    Unary(Unary),
    FunctionCall(FunctionCall),
    Cast(Cast),
    Selector(Selector),
    // TODO: add struct literal.
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
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}
