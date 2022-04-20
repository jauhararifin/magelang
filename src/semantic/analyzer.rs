use crate::token;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;

use super::error::Error;
use super::func_analyzer::FuncAnalyzer;
use super::semantic::{
    AssignStmt, BlockStmt, Def, Expr, ExprStmt, FloatType, FnDef, FnType, IfStmt, IntType,
    Program, Ptr, ReturnStmt, Statement, StructField, StructType, Type, TypeKind, VarDef, VarStmt,
    WhileStmt,
};
use super::type_analyzer::TypeAnalyzer;
use super::{cycle, func_analyzer};

pub struct SimpleAnalyzer {
    definitions: Vec<Def>,
    types: HashMap<String, Rc<Type>>,
    functions: HashMap<String, FnDef>,
}

impl SimpleAnalyzer {
    pub fn new() -> Self {
        Self {
            definitions: Vec::new(),
            types: HashMap::new(),
            functions: HashMap::new(),
        }
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

        Ok(Program {
            definitions: Vec::new(),
            types,
            functions,
        })
    }
}
