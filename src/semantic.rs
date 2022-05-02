use std::{
    cell::RefCell,
    collections::HashMap,
    ops::Deref,
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
    pub typ: Type,
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
    pub typ: Type, // This always in the Fn variant.
}

#[derive(Debug)]
pub struct Type(Rc<RefCell<ConcreteType>>);

impl Type {
    pub fn from_concrete(t: ConcreteType) -> Self {
        Type(Rc::new(RefCell::new(t)))
    }

    pub fn downgrade(&self) -> TypePtr {
        TypePtr(Rc::downgrade(&self.0))
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Type{}

impl Clone for Type {
    fn clone(&self) -> Self {
        Type(Rc::clone(&self.0))
    }
}

impl Deref for Type {
    type Target = Rc<RefCell<ConcreteType>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ConcreteType {
    Invalid,
    Bool,
    Void,
    Int(IntType),
    Float(FloatType),
    Fn(FnType),
    Struct(Struct),
}

impl ConcreteType {
    pub fn is_number(&self) -> bool {
        matches!(self, ConcreteType::Int(_) | ConcreteType::Float(_))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, ConcreteType::Int(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, ConcreteType::Bool)
    }

    pub fn is_func(&self) -> bool {
        matches!(self, ConcreteType::Fn(_))
    }

    pub fn unwrap_func(&self) -> &FnType {
        if let ConcreteType::Fn(f) = self {
            f
        } else {
            panic!("type is not a function type")
        }
    }

    pub fn unwrap_struct(&self) -> &Struct {
        if let ConcreteType::Struct(s) = self {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FloatType {
    pub size: u8,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnType {
    pub native: bool,
    pub arguments: Vec<Argument>,
    pub return_type: Option<TypePtr>,
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub index: usize,
    pub name: String,
    pub typ: TypePtr,
}

impl PartialEq for Argument {
    fn eq(&self, other: &Self) -> bool {
        // TODO jauhararifin: check the value instead of just the pointer
        self.index == other.index && self.name == other.name && self.typ.0.ptr_eq(&other.typ.0)
    }
}

impl Eq for Argument {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub fields: HashMap<String, Field>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    pub index: usize,
    pub typ: TypePtr,
}

#[derive(Debug, Clone)]
pub struct TypePtr(Weak<RefCell<ConcreteType>>);

impl TypePtr {
    pub fn new() -> Self {
        TypePtr(Weak::new())
    }

    pub fn upgrade(&self) -> Option<Type> {
        self.0.upgrade().map(Type)
    }
}

impl PartialEq for TypePtr {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0.upgrade();
        let b = other.0.upgrade();
        if let (Some(a), Some(b)) = (&a, &b) {
            return a == b;
        }
        return a.is_none() && b.is_none();
    }
}

impl Eq for TypePtr {}

impl Deref for TypePtr {
    type Target = Weak<RefCell<ConcreteType>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
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
