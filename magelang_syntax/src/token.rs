use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos(usize);

impl Pos {
    pub fn new(offset: usize) -> Self {
        Self(offset)
    }
}

impl From<Pos> for usize {
    fn from(value: Pos) -> Self {
        value.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Rc<str>,
    pub pos: Pos,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Invalid,
    Eof,
    Comment,
    Import,
    Struct,
    Fn,
    Let,
    If,
    Else,
    While,
    Ident,
    As,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitOr,
    BitAnd,
    BitXor,
    BitNot,
    ShiftLeft,
    ShiftRight,
    And,
    Not,
    Or,
    Eq,
    NEq,
    Gt,
    GEq,
    Lt,
    LEq,
    Dot,
    OpenBrac,
    CloseBrac,
    OpenBlock,
    CloseBlock,
    OpenSquare,
    CloseSquare,
    Comma,
    Colon,
    SemiColon,
    Equal,
    Return,
    IntegerLit,
    StringLit,
    RealLit,
    True,
    False,
    Continue,
    Break,
    Pound,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => write!(f, "INVALID"),
            Self::Eof => write!(f, "EOF"),
            Self::Comment => write!(f, "COMMENT"),
            Self::Import => write!(f, "IMPORT"),
            Self::Struct => write!(f, "STRUCT"),
            Self::Fn => write!(f, "FN"),
            Self::Let => write!(f, "LET"),
            Self::If => write!(f, "IF"),
            Self::Else => write!(f, "ELSE"),
            Self::While => write!(f, "WHILE"),
            Self::Ident => write!(f, "IDENT"),
            Self::As => write!(f, "AS"),
            Self::Add => write!(f, "ADD"),
            Self::Sub => write!(f, "SUB"),
            Self::Mul => write!(f, "MUL"),
            Self::Div => write!(f, "DIV"),
            Self::Mod => write!(f, "MOD"),
            Self::BitOr => write!(f, "BIT_OR"),
            Self::BitAnd => write!(f, "BIT_AND"),
            Self::BitXor => write!(f, "BIT_XOR"),
            Self::BitNot => write!(f, "BIT_NOT"),
            Self::ShiftLeft => write!(f, "SHIFT_LEFT"),
            Self::ShiftRight => write!(f, "SHIFT_RIGHT"),
            Self::And => write!(f, "AND"),
            Self::Not => write!(f, "NOT"),
            Self::Or => write!(f, "OR"),
            Self::Eq => write!(f, "EQ"),
            Self::NEq => write!(f, "NEQ"),
            Self::Gt => write!(f, "GT"),
            Self::GEq => write!(f, "GEQ"),
            Self::Lt => write!(f, "LT"),
            Self::LEq => write!(f, "LEQ"),
            Self::Dot => write!(f, "DOT"),
            Self::OpenBrac => write!(f, "OPEN_BRAC"),
            Self::CloseBrac => write!(f, "CLOSE_BRAC"),
            Self::OpenBlock => write!(f, "OPEN_BLOCK"),
            Self::CloseBlock => write!(f, "CLOSE_BLOCK"),
            Self::OpenSquare => write!(f, "OPEN_SQUARE"),
            Self::CloseSquare => write!(f, "CLOSE_SQUARE"),
            Self::Comma => write!(f, "COMMA"),
            Self::Colon => write!(f, "COLON"),
            Self::SemiColon => write!(f, "SEMI_COLON"),
            Self::Equal => write!(f, "EQUAL"),
            Self::Return => write!(f, "RETURN"),
            Self::IntegerLit => write!(f, "INTEGER_LIT"),
            Self::StringLit => write!(f, "STRING_LIT"),
            Self::RealLit => write!(f, "REAL_LIT"),
            Self::True => write!(f, "TRUE"),
            Self::False => write!(f, "FALSE"),
            Self::Continue => write!(f, "CONTINUE"),
            Self::Break => write!(f, "BREAK"),
            Self::Pound => write!(f, "POUND"),
        }
    }
}
