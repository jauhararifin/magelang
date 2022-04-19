use crate::semantic::semantic::{FnParam, FnType, Statement};
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;

use super::cycle;
use super::error::Error;
use super::semantic::{FnDef, Type, TypeKind};
use super::type_analyzer::TypeAnalyzer;

pub struct FuncAnalyzer<'a: 'b,'b> {
    root: &'a ast::Root,
    type_analyzer: &'b mut TypeAnalyzer<'a>,

    fn_to_ast: HashMap<String, &'a ast::FnDecl>,
    funcs: HashMap<String, Rc<FnDef>>,
}

impl<'a,'b> FuncAnalyzer<'a,'b> {
    pub fn new(root: &'a ast::Root, type_analyzer: &'b mut TypeAnalyzer<'a>) -> Self {
        Self {
            root,
            type_analyzer,
            fn_to_ast: HashMap::new(),
            funcs: HashMap::new(),
        }
    }

    pub fn analyze(&mut self) -> Result<HashMap<String, FnDef>, Error<'a>> {
        cycle::analyze(self.root)?;

        for decl in self.root.declarations.iter() {
            if let ast::Declaration::Fn(fn_decl) = decl {
                let name = &fn_decl.name.value.as_ref().unwrap().clone();
                self.fn_to_ast.insert(name.clone(), &fn_decl);
            }
        }

        let mut result = HashMap::new();
        for (name, typ) in std::mem::replace(&mut self.fn_to_ast, HashMap::new()).iter() {
            let typ = self.analyze_func(typ);
            result.insert(name.clone(), typ);
        }

        Ok(result)
    }

    fn analyze_func(&mut self, func: &'a ast::FnDecl) -> FnDef {
        FnDef {
            name: func.name.value.as_ref().unwrap().clone(),
            typ: self.get_func_type(func),
            body: self.analyze_block_statement(&func.body),
        }
    }

    fn get_func_type(&mut self, func: &'a ast::FnDecl) -> Rc<Type> {
        let ret_type = if let Some(typ) = func.ret_type.as_ref() {
            self.type_analyzer.analyze_type(typ)
        } else {
            Rc::new(Type {
                kind: TypeKind::Void,
                size: 0,
            })
        };

        let mut params = Vec::new();
        for param in func.param.iter() {
            let typ = self.type_analyzer.analyze_type(&param.typ);
            params.push(FnParam {
                name: param.name.value.as_ref().unwrap().clone(),
                typ,
            });
        }

        Rc::new(Type {
            kind: TypeKind::Fn(FnType { params, ret_type }),
            size: 8,
        })
    }

    fn analyze_statement(&self, stmt: &'a ast::Statement) -> Statement {
        unimplemented!();
    }

    fn analyze_block_statement(&self, stmt: &'a ast::BlockStatement) -> Statement {
        unimplemented!();
    }
}
