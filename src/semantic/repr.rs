use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Program {
    pub types: HashMap<String, Rc<Type>>,
    pub functions: HashMap<String, FnDef>,
}

#[derive(Debug)]
pub struct VarDef {
    pub name: String,
    pub typ: Rc<Type>,
    pub value: Expr,
}

#[derive(Debug)]
pub struct FnDef {
    pub id: Rc<FnId>,
    pub body: Statement,
}

#[derive(Debug)]
pub struct FnId {
    pub name: String,
    pub typ: Rc<Type>,
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
    Shr,
    Shl,
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
    UnaryOp(UnaryOp),
    FnCall(FnCall),
    BoolLit(bool),
    I32Lit(i32),
    F32Lit(f32),
    // TODO: support string lit.
    // StringLit(String),
    VarExpr(VarExpr),
    FnExpr(FnExpr),
    Cast(CastExpr),
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
    Shl,
    Shr,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub typ: Rc<Type>,
    pub a: Box<Expr>,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Not,
    BitNot,
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct FnCall {
    pub ptr: Box<Expr>,
    pub args: Vec<Expr>,
    pub typ: Rc<Type>,
}

#[derive(Debug)]
pub struct VarExpr {
    pub var: Rc<Var>,
}

#[derive(Debug)]
pub struct FnExpr {
    pub func: Rc<FnId>,
}

#[derive(Debug)]
pub struct CastExpr {
    pub value: Box<Expr>,
    pub target: Rc<Type>,
}
