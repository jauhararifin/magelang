use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub definitions: HashMap<String, Def>,
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
    pub typ: Type,
    pub value: Expr,
}

#[derive(Debug)]
pub struct FnDef {
    pub name: String,
    pub typ: Type,
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
    pub name: String,
    pub typ: Type,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub size: i32,
}

#[derive(Debug)]
pub enum TypeKind {
    Void,
    Int(IntType),
    Float(FloatType),
    Struct(StructType),
    Fn(FnType),
    Ptr(Ptr),
}

#[derive(Debug)]
pub struct IntType {
    pub signed: bool,
    pub size: i32,
}

#[derive(Debug)]
pub struct FloatType {
    pub size: i32,
}

#[derive(Debug)]
pub struct StructType {
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub offset: i32,
    pub typ: Type,
}

#[derive(Debug)]
pub struct FnType {
    pub params: Vec<FnParam>,
    pub ret_type: Box<Type>,
}

#[derive(Debug)]
pub struct FnParam {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Ptr {
    pub elem: Box<Type>,
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
    pub typ: Type,
    pub assignable: bool,
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOpKind),
    FnCall(FnCall),
}

#[derive(Debug)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub typ: Type,
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
    SHL,
    SHR,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub typ: Type,
    pub a: Box<Expr>,
    pub b: Box<Expr>,
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
