use crate::file::FileId;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub file_id: FileId,
    pub start: usize,
    pub len: usize,
}

impl Span {
    pub fn new(file_id: FileId, offset: usize, len: usize) -> Self {
        Self {
            file_id,
            start: offset,
            len,
        }
    }

    pub fn union(&mut self, other: &Self) {
        if other.start < self.start {
            self.start = other.start;
        }
        if other.start + other.len > self.start + self.len {
            self.len = other.start + other.len - self.start;
        }
    }
}

#[derive(Debug)]
pub struct PosInfo {
    pub path: Rc<Path>,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for PosInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", &self.path.to_str().unwrap(), self.line, self.col)
    }
}
