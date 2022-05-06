use std::rc::Rc;

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
    pub name: Rc<String>,
    pub native: bool,
    pub fn_type: Rc<FnType>,
    pub typ: Rc<Type>,
}

// TODO (jauhararifin): consider adding `Copy` trait to the  `Type`. Since the size is not so big,
// maybe it is better to implement `Copy` trait instead of using `Rc<Type>` everywhere.
// The only thing that causing expensive copy is the FnType which contains strings and vectors.
// Maybe we can consider wrapping the strings and FnType instead.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    Void,
    Int(IntType),
    Float(FloatType),
    Fn(Rc<FnType>),
    Array(Rc<ArrayType>),
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

    pub fn unwrap_func(&self) -> &Rc<FnType> {
        if let Type::Fn(f) = self {
            f
        } else {
            panic!("type is not a function type")
        }
    }

    pub fn try_unwrap_func(&self) -> Option<&Rc<FnType>> {
        if let Type::Fn(f) = self {
            Some(f)
        } else {
            None
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

impl Into<Type> for IntType {
    fn into(self) -> Type {
        Type::Int(self)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FloatType {
    pub size: u8,
}

impl FloatType {
    pub fn new(size: u8) -> Self {
        Self { size }
    }
}

impl Into<Type> for FloatType {
    fn into(self) -> Type {
        Type::Float(self)
    }
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
    pub name: Rc<String>,
    pub typ: Rc<Type>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayType {
    pub size: usize,
    pub elem_type: Rc<Type>,
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
    pub name: Rc<String>,
    pub typ: Rc<Type>,
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
    pub typ: Rc<Type>,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident(Rc<String>),
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
    Index(Index),
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

impl Into<ExprKind> for FunctionCall {
    fn into(self) -> ExprKind {
        ExprKind::FunctionCall(self)
    }
}

#[derive(Debug, Clone)]
pub struct Index {
    pub array: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub target: Rc<Type>,
    pub val: Box<Expr>,
}
