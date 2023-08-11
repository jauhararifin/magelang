use std::fmt::Display;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Pos(usize);

impl From<usize> for Pos {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl Pos {
    pub fn with_offset(&self, offset: usize) -> Self {
        Self(self.0 + offset)
    }
}

pub struct Location<'a> {
    path: &'a Path,
    line: usize,
    col: usize,
}

impl<'a> std::fmt::Display for Location<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.path.to_string_lossy(),
            self.line,
            self.col
        )
    }
}

#[derive(Default)]
pub struct FileManager {
    file_offset: Vec<usize>,
    file_path: Vec<PathBuf>,
    lines: Vec<Vec<usize>>,
    last_offset: usize,
}

pub struct File {
    pub offset: Pos,
    pub text: String,
}

impl FileManager {
    pub fn open(&mut self, path: PathBuf) -> Result<File, std::io::Error> {
        let source_code = read_to_string(&path)?;
        Ok(self.add_file(path, source_code))
    }

    pub fn add_file(&mut self, path: PathBuf, source: String) -> File {
        let file_offset = self.last_offset;

        let mut lines = Vec::default();
        for (i, c) in source.char_indices() {
            if c == '\n' {
                lines.push(file_offset + i);
            }
            self.last_offset += 1;
        }

        self.file_offset.push(file_offset);
        self.file_path.push(path);
        self.lines.push(lines);

        File {
            offset: Pos(file_offset),
            text: source,
        }
    }

    pub fn location(&self, pos: Pos) -> Location {
        let i = self.file_offset.partition_point(|x| *x <= pos.0) - 1;

        let file_offset = self.file_offset[i];
        let path: &Path = &self.file_path[i];
        let lines = &self.lines[i];

        let offset = pos.0 - file_offset;
        let line = lines.partition_point(|x| *x < offset) + 1;

        let col = if line == 1 {
            offset + 1
        } else {
            offset - lines[line - 2]
        };

        Location { path, line, col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
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
    AtSign,
}

impl Display for TokenKind {
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
            Self::AtSign => write!(f, "AT_SIGN"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_location() {
        let mut file_manager = FileManager::default();
        let path = PathBuf::from("some_dummy_file");
        let file = file_manager.add_file(path, String::from("aaa\nbbb\nccc\n"));

        let pos = file.offset.with_offset(0);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 1));

        let pos = file.offset.with_offset(1);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 2));

        let pos = file.offset.with_offset(2);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 3));

        let pos = file.offset.with_offset(3);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 4));

        let pos = file.offset.with_offset(4);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 1));

        let pos = file.offset.with_offset(5);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 2));

        let pos = file.offset.with_offset(6);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 3));

        let pos = file.offset.with_offset(7);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 4));

        let pos = file.offset.with_offset(8);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 1));

        let pos = file.offset.with_offset(9);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 2));

        let pos = file.offset.with_offset(10);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 3));

        let pos = file.offset.with_offset(11);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 4));
    }
}
