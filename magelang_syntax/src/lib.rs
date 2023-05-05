mod ast;
mod errors;
mod parser;
mod scanner;
mod tokens;

pub use ast::*;
pub use tokens::{Token, TokenKind};

use crate::parser::parse;
use magelang_common::{ErrorAccumulator, FileId, FileLoader, SymbolLoader};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct AstLoader<'err, 'file, 'sym> {
    err_channel: &'err ErrorAccumulator,
    file_loader: &'file FileLoader<'err>,
    symbol_loader: &'sym SymbolLoader,
    cache: RefCell<HashMap<FileId, Rc<PackageNode>>>,
}

impl<'err, 'file, 'sym> AstLoader<'err, 'file, 'sym> {
    pub fn new(
        err_channel: &'err ErrorAccumulator,
        file_loader: &'file FileLoader<'err>,
        symbol_loader: &'sym SymbolLoader,
    ) -> Self {
        Self {
            err_channel,
            file_loader,
            symbol_loader,
            cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_ast(&self, file_id: FileId) -> Rc<PackageNode> {
        self.cache
            .borrow_mut()
            .entry(file_id)
            .or_insert_with(|| {
                let file_info = self.file_loader.get_file(file_id).unwrap();
                parse(self.err_channel, self.symbol_loader, file_id, &file_info.text)
            })
            .clone()
    }
}
