use std::{fmt, rc::Rc};

#[derive(Clone, Eq, PartialEq, Default)]
pub struct Pos {
    pub file_name: Rc<String>,
    pub line: usize,
    pub col: usize,
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_name, self.line, self.col)
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "file {}, line {}, col {}", self.file_name, self.line, self.col)
    }
}
