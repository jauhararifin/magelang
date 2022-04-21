use crate::ast;

use super::error::Error;
use super::func_analyzer::FuncAnalyzer;
use super::semantic::Program;
use super::type_analyzer::TypeAnalyzer;

pub struct SimpleAnalyzer {}

impl SimpleAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze<'a>(&mut self, root: &'a ast::Root) -> Result<Program, Error<'a>> {
        for decl in root.declarations.iter() {
            if let ast::Declaration::Var(_) = decl {
                return Err(Error::UnsupportedGlobal);
            }
        }

        let mut type_analyzer = TypeAnalyzer::new(root);
        let types = type_analyzer.analyze()?;

        let mut func_analyzer = FuncAnalyzer::new(root, &mut type_analyzer);
        let functions = func_analyzer.analyze()?;

        Ok(Program { types, functions })
    }
}
