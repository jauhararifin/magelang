use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Program {
    pub definitions: Vec<Def>,

    pub types: HashMap<String, Rc<Type>>,
    pub functions: HashMap<String, FnDef>,
}

#[derive(Debug)]
pub enum Def {
    FnDef(FnDef),
    VarDef(VarDef),
    TypeDef(Type),
}

#[derive(Debug)]
pub struct VarDef {
    pub name: String,
    pub typ: Rc<Type>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: String,
    pub typ: Rc<Type>,
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Block(BlockStmt),
    Assign(AssignStmt),
    If(IfStmt),
    While(WhileStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
    Var(VarStmt),
}

#[derive(Debug)]
pub struct BlockStmt {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub target: Expr,
    pub kind: AssignKind,
    pub value: Expr,
}

#[derive(Debug)]
pub enum AssignKind {
    Assign,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    SHR,
    SHL,
    BitAnd,
    BitXor,
    BitOr,
}

#[derive(Debug)]
pub struct IfStmt {
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub value: Expr,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Expr,
}

#[derive(Debug)]
pub struct VarStmt {
    pub receiver: Rc<Var>,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Var {
    pub name: String,
    pub typ: Rc<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeKind {
    Void,
    Bool,
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Fn(FnType),
    Ptr(Ptr),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntType {
    pub signed: bool,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FloatType {
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StructField {
    pub name: String,
    pub offset: usize,
    pub typ: Rc<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub params: Vec<Rc<Var>>,
    pub ret_type: Rc<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ptr {
    pub elem: Rc<Type>,
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Selector {}

#[derive(Debug)]
pub struct Expr {
    pub typ: Rc<Type>,
    pub kind: ExprKind,
    pub assignable: bool,
}

#[derive(Debug)]
pub enum ExprKind {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOpKind),
    FnCall(FnCall),
    BoolLit(bool),
    I8Lit(i8),
    I16Lit(i16),
    I32Lit(i32),
    I64Lit(i64),
    U8Lit(i8),
    U16Lit(i16),
    U32Lit(i32),
    U64Lit(i64),
    F32Lit(f32),
    F64Lit(f32),
    StringLit(String),
    VarExpr(VarExpr),
}

#[derive(Debug)]
pub struct BinaryOp {
    pub op: BinaryOpKind,
    pub typ: Rc<Type>,
    pub a: Box<Expr>,
    pub b: Box<Expr>,
}

#[derive(Debug)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    BitAnd,
    Or,
    BitOr,
    BitXor,
    GT,
    LT,
    GTEq,
    LTEq,
    Eq,
    NotEq,
    SHL,
    SHR,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub typ: Type,
    pub a: Box<Expr>,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Not,
    BitNot,
    Plus,
    Minus,
    Addr,
    Deref,
}

#[derive(Debug)]
pub struct FnCall {
    pub ptr: Box<Expr>,
    pub args: Vec<Expr>,
    pub typ: Type,
}

#[derive(Debug)]
pub struct VarExpr {
    pub var: Rc<Var>,
}
