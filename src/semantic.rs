use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Header {
    pub package_name: String,

    pub types: Vec<Type>,
    pub vars: Vec<VarHeader>,
    pub functions: Vec<FnHeader>,
}

#[derive(Debug, Clone)]
pub struct Unit {
    pub package_name: String,

    pub var_declarations: Vec<Var>,
    pub fn_declarations: Vec<FnDecl>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name {
    pub package: String,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub header: VarHeader,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarHeader {
    pub name: Name,
    pub type_kind: TypeKind,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub header: FnHeader,
    pub body: Option<Statement>, // native functions don't have body.
}

#[derive(Debug, Clone)]
pub struct FnHeader {
    pub name: Name,
    pub native: bool,
    pub typ: FnType,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Type {
    pub name: Option<Name>,
    pub kind: TypeKind,
    // add method here.
}

impl Type {
    pub fn anon(kind: TypeKind) -> Self {
        Self { name: None, kind }
    }

    pub fn invalid() -> Self {
        Self::anon(TypeKind::Invalid)
    }

    pub fn is_anonymous(&self) -> bool {
        self.name.is_none()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeKind {
    Invalid,
    Bool,
    Void,
    Int(IntType),
    Float(FloatType),
    Fn(FnType),
    Struct(Struct),
    Ptr(Ptr),
    Package(String),
}

impl TypeKind {
    pub fn is_invalid(&self) -> bool {
        matches!(self, TypeKind::Invalid)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, TypeKind::Int(_) | TypeKind::Float(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, TypeKind::Int(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, TypeKind::Bool)
    }

    pub fn is_func(&self) -> bool {
        matches!(self, TypeKind::Fn(_))
    }

    pub fn unwrap_func(&self) -> &FnType {
        if let TypeKind::Fn(f) = self {
            f
        } else {
            panic!("type is not a function type")
        }
    }

    pub fn unwrap_struct(&self) -> &Struct {
        if let TypeKind::Struct(s) = self {
            s
        } else {
            panic!("type is not a struct type")
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
    pub return_type: Option<Box<TypeKind>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Argument {
    pub index: usize,
    pub name: String,
    pub type_kind: TypeKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub fields: HashMap<String, Field>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub index: usize,
    pub type_kind: TypeKind,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Ptr {
    pub name: Name,
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
    pub type_kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident(String),
    Package(String),
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
