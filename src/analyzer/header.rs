use crate::{
    analyzer::types::TypeHelper,
    ast,
    errors::Error,
    semantic::{FnHeader, Header},
};

use super::expr::{ExprHelper, Symbol};

pub struct HeaderCompiler {}

impl HeaderCompiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, root: &ast::Root) -> Result<Header, Error> {
        let mut functions = Vec::new();

        let type_helper = TypeHelper::new();
        let mut expr_helper = ExprHelper::empty(&type_helper);
        let fn_decls = root.declarations.iter().filter_map(|decl| decl.try_unwrap_func());

        for func in fn_decls {
            let name = String::from(func.name.unwrap_str());

            if expr_helper.find_symbol(&name).is_some() {
                return Err(Error::RedeclaredSymbol);
            }

            let type_kind = type_helper.get_fn(&func.header);

            let fn_type = type_kind.unwrap_func();
            functions.push(FnHeader {
                name: name.clone(),
                native: fn_type.native,
                typ: fn_type.clone(),
            });

            expr_helper.add_symbol(Symbol { name, type_kind });
        }

        Ok(Header { functions })
    }
}
