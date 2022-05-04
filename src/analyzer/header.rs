use crate::{
    analyzer::types::TypeHelper,
    ast::RootNode,
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
            let name = String::from(func.name.unwrap_str());

            if expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            let typ = type_helper.get_fn(&func.header);

            let fn_type = typ.unwrap_func();
            functions.push(FnHeader {
                name: name.clone(),
                native: fn_type.native,
                typ: fn_type.clone(),
            });

            expr_helper.add_symbol(Symbol { name, typ });
        }

        Ok(Header { functions })
    }
}
