use crate::package::PathId;
use magelang_syntax::Pos;
use std::fmt::Display;
use std::path::Path;
use std::rc::Rc;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Loc(PathId, Pos);

impl Loc {
    pub fn new(path: PathId, pos: Pos) -> Self {
        Self(path, pos)
    }
}

pub struct Location {
    pub path: Rc<Path>,
    pub line: usize,
    pub col: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{}:{}", self.path.as_os_str(), self.line, self.col)
    }
}

pub trait ErrorAccumulator {
    fn report_error(&self, pos: Loc, error: String);

    fn cannot_open_file(&self, path_id: PathId, io_err: &std::io::Error) {
        self.report_error(Loc::new(path_id, Pos::new(0)), format!("Cannot open file: {io_err}"))
    }

    fn redeclared_symbol(&self, name: &str, declared_at: &Location, redeclared_at: Loc) {
        self.report_error(
            redeclared_at,
            format!("Symbol {name} is redeclared. First declared at {declared_at}"),
        )
    }

    fn invalid_utf8_package(&self, loc: Loc) {
        self.report_error(
            loc,
            String::from("The package path is not a valid utf-8 string literal"),
        );
    }
}
