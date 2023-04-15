use magelang_common::Span;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: Rc<str>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Invalid,
    Eof,
    Comment,
    Import,
    Fn,
    Let,
    If,
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
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => write!(f, "Invalid"),
            Self::Eof => write!(f, "Eof"),
            Self::Comment => write!(f, "Comment"),
            Self::Import => write!(f, "Import"),
            Self::Fn => write!(f, "Fn"),
            Self::Let => write!(f, "Let"),
            Self::If => write!(f, "If"),
            Self::While => write!(f, "While"),
            Self::Ident => write!(f, "Ident"),
            Self::As => write!(f, "As"),
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Mod => write!(f, "Mod"),
            Self::BitOr => write!(f, "BitOr"),
            Self::BitAnd => write!(f, "BitAnd"),
            Self::BitXor => write!(f, "BitXor"),
            Self::BitNot => write!(f, "BitNot"),
            Self::ShiftLeft => write!(f, "ShiftLeft"),
            Self::ShiftRight => write!(f, "ShiftRight"),
            Self::And => write!(f, "And"),
            Self::Not => write!(f, "Not"),
            Self::Or => write!(f, "Or"),
            Self::Eq => write!(f, "Eq"),
            Self::NEq => write!(f, "NEq"),
            Self::Gt => write!(f, "Gt"),
            Self::GEq => write!(f, "GEq"),
            Self::Lt => write!(f, "Lt"),
            Self::LEq => write!(f, "LEq"),
            Self::Dot => write!(f, "Dot"),
            Self::OpenBrac => write!(f, "OpenBrac"),
            Self::CloseBrac => write!(f, "CloseBrac"),
            Self::OpenBlock => write!(f, "OpenBlock"),
            Self::CloseBlock => write!(f, "CloseBlock"),
            Self::Comma => write!(f, "Comma"),
            Self::Colon => write!(f, "Colon"),
            Self::SemiColon => write!(f, "SemiColon"),
            Self::Equal => write!(f, "Equal"),
            Self::Return => write!(f, "Return"),
            Self::IntegerLit => write!(f, "IntegerLit"),
            Self::StringLit => write!(f, "StringLit"),
            Self::RealLit => write!(f, "RealLit"),
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
        }
    }
}
