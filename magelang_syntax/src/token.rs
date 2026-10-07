use crate::ast::BinaryOp;
use crate::number::Number;
use std::fmt::Display;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

const MAX_FILE_SIZE: usize = 2 * 1024 * 1024 * 1024;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pos {
    pub file: FileId,
    pub line: u32,
    pub col: u32,
}

pub struct Location<'a> {
    pub path: &'a Path,
    pub line: u32,
    pub col: u32,
}

impl std::fmt::Display for Location<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path = std::env::current_dir()
            .ok()
            .and_then(|cwd| self.path.strip_prefix(&cwd).ok())
            .unwrap_or(self.path)
            .to_string_lossy();

        write!(f, "{path}:{}:{}", self.line, self.col)
    }
}

#[derive(Default)]
pub struct FileManager {
    paths: Vec<PathBuf>,
}

pub struct File {
    pub id: FileId,
    pub text: String,
}

impl FileManager {
    pub fn open(&mut self, path: PathBuf) -> io::Result<File> {
        let file = std::fs::File::open(&path)?;
        let size = file.metadata()?.len();
        if size > MAX_FILE_SIZE as u64 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Source file exceeds the 2 GiB size limit"));
        }

        let mut source = String::with_capacity(size as usize);
        file.take(MAX_FILE_SIZE as u64 + 1).read_to_string(&mut source)?;
        self.add_file(path, source)
    }

    pub fn add_file(&mut self, path: PathBuf, source: String) -> io::Result<File> {
        if source.len() > MAX_FILE_SIZE {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "Source file exceeds the 2 GiB size limit"));
        }
        let id = FileId(u32::try_from(self.paths.len()).map_err(|_| io::Error::other("Too many source files"))?);
        self.paths.push(path);
        Ok(File { id, text: source })
    }

    pub fn location(&self, pos: Pos) -> Location<'_> {
        Location { path: &self.paths[pos.file.0 as usize], line: pos.line, col: pos.col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) pos: Pos,

    // spacing is true when previous token contains spacing with current token
    pub(crate) spacing: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum TokenKind {
    Invalid(char),
    Eof,
    Comment(String),
    Import,
    Struct,
    Fn,
    Let,
    If,
    Else,
    While,
    For,
    Defer,
    Ident(String),
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
    AssignOp(BinaryOp),
    Return,
    NumberLit { raw: String, value: Number },
    StringLit { raw: String, value: Vec<u8> },
    CharLit { raw: String, value: char },
    Null,
    True,
    False,
    Continue,
    Break,
    AtSign,
}

impl TokenKind {
    pub(crate) fn is_keyword(&self) -> bool {
        match self {
            Self::Import
            | Self::Struct
            | Self::Fn
            | Self::Let
            | Self::If
            | Self::Else
            | Self::While
            | Self::For
            | Self::Defer
            | Self::As
            | Self::Return
            | Self::Null
            | Self::True
            | Self::False
            | Self::Continue
            | Self::Break => true,

            Self::Invalid(..)
            | Self::Eof
            | Self::Comment(..)
            | Self::Ident(..)
            | Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::BitOr
            | Self::BitAnd
            | Self::BitXor
            | Self::BitNot
            | Self::ShiftLeft
            | Self::ShiftRight
            | Self::And
            | Self::Not
            | Self::Or
            | Self::Eq
            | Self::NEq
            | Self::Gt
            | Self::GEq
            | Self::Lt
            | Self::LEq
            | Self::Dot
            | Self::OpenBrac
            | Self::CloseBrac
            | Self::OpenBlock
            | Self::CloseBlock
            | Self::OpenSquare
            | Self::CloseSquare
            | Self::Comma
            | Self::Colon
            | Self::SemiColon
            | Self::Equal
            | Self::AssignOp(..)
            | Self::NumberLit { .. }
            | Self::CharLit { .. }
            | Self::StringLit { .. }
            | Self::AtSign => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenKind::Ident(raw) => write!(f, "'{raw}'"),
            _ => self.kind.fmt(f),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid(..) => write!(f, "INVALID"),
            Self::Eof => write!(f, "EOF"),
            Self::Comment(..) => write!(f, "COMMENT"),
            Self::Import => write!(f, "'import'"),
            Self::Struct => write!(f, "'struct'"),
            Self::Fn => write!(f, "'fn'"),
            Self::Let => write!(f, "'let'"),
            Self::If => write!(f, "'if'"),
            Self::Else => write!(f, "'else'"),
            Self::While => write!(f, "'while'"),
            Self::For => write!(f, "'for'"),
            Self::Defer => write!(f, "'defer'"),
            Self::Ident(..) => write!(f, "IDENT"),
            Self::As => write!(f, "'as'"),
            Self::Add => write!(f, "'+'"),
            Self::Sub => write!(f, "'-'"),
            Self::Mul => write!(f, "'*'"),
            Self::Div => write!(f, "'/'"),
            Self::Mod => write!(f, "'%'"),
            Self::BitOr => write!(f, "'|'"),
            Self::BitAnd => write!(f, "'&'"),
            Self::BitXor => write!(f, "'^'"),
            Self::BitNot => write!(f, "'~'"),
            Self::ShiftLeft => write!(f, "'<<'"),
            Self::ShiftRight => write!(f, "'>>'"),
            Self::And => write!(f, "'&&'"),
            Self::Not => write!(f, "'!'"),
            Self::Or => write!(f, "'||'"),
            Self::Eq => write!(f, "'=='"),
            Self::NEq => write!(f, "'!='"),
            Self::Gt => write!(f, "'>'"),
            Self::GEq => write!(f, "'>='"),
            Self::Lt => write!(f, "'<'"),
            Self::LEq => write!(f, "'<='"),
            Self::Dot => write!(f, "'.'"),
            Self::OpenBrac => write!(f, "'('"),
            Self::CloseBrac => write!(f, "')'"),
            Self::OpenBlock => write!(f, "'{{'"),
            Self::CloseBlock => write!(f, "'}}'"),
            Self::OpenSquare => write!(f, "'['"),
            Self::CloseSquare => write!(f, "']'"),
            Self::Comma => write!(f, "','"),
            Self::Colon => write!(f, "':'"),
            Self::SemiColon => write!(f, "';'"),
            Self::Equal => write!(f, "'='"),
            Self::AssignOp(op) => write!(
                f,
                "'{}='",
                match op {
                    BinaryOp::Add => "+",
                    BinaryOp::Sub => "-",
                    BinaryOp::Mul => "*",
                    BinaryOp::Div => "/",
                    BinaryOp::Mod => "%",
                    BinaryOp::BitAnd => "&",
                    BinaryOp::BitOr => "|",
                    BinaryOp::BitXor => "^",
                    BinaryOp::ShiftLeft => "<<",
                    BinaryOp::ShiftRight => ">>",
                    BinaryOp::And => "&&",
                    BinaryOp::Or => "||",
                    _ => unreachable!("{op:?} has no assignment form"),
                }
            ),
            Self::Return => write!(f, "'return'"),
            Self::NumberLit { .. } => write!(f, "NUMBER_LIT"),
            Self::CharLit { .. } => write!(f, "CHAR_LIT"),
            Self::StringLit { .. } => write!(f, "STRING_LIT"),
            Self::Null => write!(f, "'null'"),
            Self::True => write!(f, "'true'"),
            Self::False => write!(f, "'false'"),
            Self::Continue => write!(f, "'continue'"),
            Self::Break => write!(f, "'break'"),
            Self::AtSign => write!(f, "'@'"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locations_preserve_file_identity() {
        let mut files = FileManager::default();
        let first = files.add_file("first.mg".into(), "é\nvalue".into()).unwrap();
        let first_pos = Pos { file: first.id, line: 2, col: 6 };
        let second = files.add_file("second.mg".into(), String::new()).unwrap();
        let third = files.add_file("third.mg".into(), String::new()).unwrap();
        assert_ne!(first.id, second.id);
        assert_ne!(second.id, third.id);

        let location = files.location(first_pos);
        assert_eq!(location.path, Path::new("first.mg"));
        assert_eq!((location.line, location.col), (2, 6));
        assert_eq!(location.to_string(), "first.mg:2:6");

        for (file, path) in [(second, "second.mg"), (third, "third.mg")] {
            let pos = Pos { file: file.id, line: 1, col: 1 };
            let location = files.location(pos);
            assert_eq!(location.path, Path::new(path));
            assert_eq!((location.line, location.col), (1, 1));
        }
    }

    #[test]
    fn oversized_files_are_rejected_before_reading() {
        let suffix = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_nanos();
        let path = std::env::temp_dir().join(format!("magelang-oversized-{}-{suffix}.mg", std::process::id()));
        let file = std::fs::File::create_new(&path).unwrap();
        file.set_len(MAX_FILE_SIZE as u64 + 1).unwrap();

        let mut files = FileManager::default();
        let result = files.open(path.clone());
        drop(file);
        std::fs::remove_file(path).unwrap();

        let error = result.err().expect("oversized source must be rejected");
        assert_eq!(error.kind(), io::ErrorKind::InvalidData);
        assert_eq!(error.to_string(), "Source file exceeds the 2 GiB size limit");
        assert!(files.paths.is_empty());
    }
}
