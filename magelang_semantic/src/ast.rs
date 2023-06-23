use crate::error::{Loc, Location};
use crate::package::{AstInfo, PackageDb, PathId};
use magelang_syntax::AstNode;
use std::convert::AsRef;
use std::ops::Deref;

pub struct AstWithInfo<'a, T> {
    pub path: PathId,
    pub lines: &'a [usize],
    pub node: &'a T,
}

impl<'a, T> AstWithInfo<'a, T> {
    pub fn new(ast_info: &'a AstInfo, node: &'a T) -> Self {
        Self {
            path: ast_info.path,
            lines: &ast_info.lines,
            node,
        }
    }

    pub fn map<U>(&self, f: impl FnOnce(&T) -> &U) -> AstWithInfo<U> {
        AstWithInfo {
            path: self.path,
            lines: self.lines,
            node: f(self.node),
        }
    }
}

impl<'a, T> AsRef<T> for AstWithInfo<'a, T> {
    fn as_ref(&self) -> &T {
        self.node
    }
}

impl<'a, T> Deref for AstWithInfo<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.node
    }
}

impl<'a, T> AstWithInfo<'a, T>
where
    T: AstNode,
{
    pub fn loc(&self) -> Loc {
        Loc::new(self.path, self.node.get_pos())
    }

    pub fn location(&self, db: &impl PackageDb) -> Location {
        let path = db.get_path(self.path);
        let offset: usize = self.node.get_pos().into();
        let partition = self.lines.partition_point(|line| *line < offset);
        let line = partition + 1;
        let line_offset = self
            .lines
            .get(partition)
            .or(self.lines.last())
            .cloned()
            .unwrap_or_default();
        let col = offset - line_offset;
        Location { path, line, col }
    }
}

pub trait WithInfo: Sized {
    fn with_info<'a, T>(&'a self, ast_info: &AstWithInfo<'a, T>) -> AstWithInfo<'a, Self> {
        AstWithInfo {
            path: ast_info.path,
            lines: ast_info.lines,
            node: self,
        }
    }
}

impl<T> WithInfo for T {}
