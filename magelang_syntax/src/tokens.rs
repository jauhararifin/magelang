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
    Ident,
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
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => write!(f, "Invalid"),
            Self::Eof => write!(f, "Eof"),
            Self::Comment => write!(f, "Comment"),
            Self::Import => write!(f, "Import"),
            Self::Fn => write!(f, "Fn"),
            Self::Ident => write!(f, "Ident"),
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
        }
    }
}
