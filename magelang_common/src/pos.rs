use crate::file::FileId;
use std::path::Path;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    pub file_id: FileId,
    pub offset: u32,
}

impl Pos {
    pub fn new(file_id: FileId, offset: u32) -> Self {
        Self { file_id, offset }
    }
}

#[derive(Debug)]
pub struct PosInfo {
    pub path: Rc<Path>,
    pub line: u32,
    pub col: u32,
}

impl std::fmt::Display for PosInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", &self.path.to_str().unwrap(), self.line, self.col)
    }
}
