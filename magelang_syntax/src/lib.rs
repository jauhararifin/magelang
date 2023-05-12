mod ast;
mod errors;
mod parser;
mod scanner;
mod tokens;

pub use ast::*;
pub use tokens::{Token, TokenKind};

use crate::parser::parse;
use magelang_common::{ErrorAccumulator, FileId, FileLoader};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct AstLoader<'err, 'file> {
    err_channel: &'err ErrorAccumulator,
    file_loader: &'file FileLoader<'err>,
    cache: RefCell<HashMap<FileId, Rc<PackageNode>>>,
}

impl<'err, 'file> AstLoader<'err, 'file> {
    pub fn new(err_channel: &'err ErrorAccumulator, file_loader: &'file FileLoader<'err>) -> Self {
        Self {
            err_channel,
            file_loader,
            cache: RefCell::new(HashMap::new()),
        }
    }

    pub fn get_ast(&self, file_id: FileId) -> Rc<PackageNode> {
        self.cache
            .borrow_mut()
            .entry(file_id)
            .or_insert_with(|| {
                let file_info = self.file_loader.get_file(file_id).unwrap();
                parse(self.err_channel, file_id, &file_info.text)
            })
            .clone()
    }
}
