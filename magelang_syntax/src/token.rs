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

impl std::cmp::Ord for Pos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::cmp::PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp(&other.0))
    }
}

pub struct Location<'a> {
    path: &'a Path,
    line: usize,
    col: usize,
}

impl<'a> std::fmt::Display for Location<'a> {
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
                lines.push(i);
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
    DoubleColon,
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
            Self::Import => write!(f, "'import'"),
            Self::Struct => write!(f, "'struct'"),
            Self::Fn => write!(f, "'fn'"),
            Self::Let => write!(f, "'let'"),
            Self::If => write!(f, "'if'"),
            Self::Else => write!(f, "'else'"),
            Self::While => write!(f, "'while'"),
            Self::Ident => write!(f, "IDENT"),
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
            Self::DoubleColon => write!(f, "'::'"),
            Self::Colon => write!(f, "':'"),
            Self::SemiColon => write!(f, "';'"),
            Self::Equal => write!(f, "'='"),
            Self::Return => write!(f, "'return'"),
            Self::IntegerLit => write!(f, "INTEGER_LIT"),
            Self::StringLit => write!(f, "STRING_LIT"),
            Self::RealLit => write!(f, "REAL_LIT"),
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
    fn test_get_location() {
        let mut file_manager = FileManager::default();
        let path = PathBuf::from("some_dummy_file");
        let file1 = file_manager.add_file(path, String::from("aaa\nbbb\nccc\n"));

        let pos = file1.offset.with_offset(0);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 1));

        let pos = file1.offset.with_offset(1);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 2));

        let pos = file1.offset.with_offset(2);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 3));

        let pos = file1.offset.with_offset(3);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 4));

        let pos = file1.offset.with_offset(4);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 1));

        let pos = file1.offset.with_offset(5);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 2));

        let pos = file1.offset.with_offset(6);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 3));

        let pos = file1.offset.with_offset(7);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 4));

        let pos = file1.offset.with_offset(8);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 1));

        let pos = file1.offset.with_offset(9);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 2));

        let pos = file1.offset.with_offset(10);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 3));

        let pos = file1.offset.with_offset(11);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 4));

        let path = PathBuf::from("other_file");
        let file2 = file_manager.add_file(path, String::from("some other\nfile"));

        let pos = file2.offset.with_offset(1);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 2));

        let pos = file2.offset.with_offset(12);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 2));

        let pos = file1.offset.with_offset(0);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 1));

        let pos = file1.offset.with_offset(1);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 2));

        let pos = file1.offset.with_offset(2);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 3));

        let pos = file1.offset.with_offset(3);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (1, 4));

        let pos = file1.offset.with_offset(4);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 1));

        let pos = file1.offset.with_offset(5);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 2));

        let pos = file1.offset.with_offset(6);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 3));

        let pos = file1.offset.with_offset(7);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (2, 4));

        let pos = file1.offset.with_offset(8);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 1));

        let pos = file1.offset.with_offset(9);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 2));

        let pos = file1.offset.with_offset(10);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 3));

        let pos = file1.offset.with_offset(11);
        let loc = file_manager.location(pos);
        assert_eq!((loc.line, loc.col), (3, 4));
    }
}
