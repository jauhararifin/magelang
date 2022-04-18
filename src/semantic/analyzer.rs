use crate::ast::Root;
use crate::semantic::cycle;

use super::{Error, Program};

pub struct SimpleAnalyzer {}

impl SimpleAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze<'a>(&mut self, root_ast: &'a Root) -> Result<Program, Error<'a>> {
        self.analyze_types(root_ast)?;
        unimplemented!();
    }

    fn analyze_types<'a>(&mut self, root_ast: &'a Root) -> Result<(), Error<'a>> {
        return cycle::analyze(root_ast);
    }
}
