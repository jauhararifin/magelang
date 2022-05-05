use std::rc::Rc;

use crate::{
    analyzer::types::TypeHelper,
    ast::{FnDeclNode, RootNode},
    errors::Error,
    semantic::{FnHeader, Header},
};

use super::expr::{ExprHelper, Symbol};

pub trait IHeaderCompiler {
    fn compile_header(&self, root: &RootNode) -> Result<Header, Error>;
}

pub struct HeaderCompiler {}

impl HeaderCompiler {
    pub fn new() -> Self {
        Self {}
    }
}

impl IHeaderCompiler for HeaderCompiler {
    fn compile_header(&self, root: &RootNode) -> Result<Header, Error> {
        let mut functions = Vec::new();

        let type_helper = TypeHelper::new();
        let mut expr_helper = ExprHelper::empty(&type_helper);

        let fn_decls = root.declarations.iter().filter_map(|decl| decl.try_unwrap_func());
        for func in fn_decls {
            let func = self.compile_fn_header(&mut expr_helper, &type_helper, func)?;
            functions.push(func);
        }

        Ok(Header { functions })
    }
}

impl HeaderCompiler {
    fn compile_fn_header(
        &self,
        expr_helper: &mut ExprHelper,
        type_helper: &TypeHelper,
        func: &FnDeclNode,
    ) -> Result<FnHeader, Error> {
        let name = func.name.clone_value();

        if expr_helper.find_symbol(&name).is_some() {
            return Err(Error::RedeclaredSymbol);
        }

        let typ = type_helper.get_fn(&func.header);

        let fn_type = typ.unwrap_func();

        expr_helper.add_symbol(Rc::new(Symbol {
            name: name.clone(),
            typ: typ.clone(),
        }));

        Ok(FnHeader {
            name,
            native: fn_type.native,
            fn_type: fn_type.clone(),
            typ,
        })
    }
}
