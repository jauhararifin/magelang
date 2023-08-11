use crate::token::{FileManager, Pos};
use indexmap::IndexSet;
use std::cell::RefCell;
use std::fmt::Display;

#[derive(PartialEq, Eq, Hash)]
pub struct Error {
    pub pos: Pos,
    pub message: String,
}

impl Error {
    pub fn new(pos: Pos, message: String) -> Self {
        Self { pos, message }
    }

    pub fn display(&self, file_manager: &FileManager) -> impl Display {
        let loc = file_manager.location(self.pos);
        format!("{loc}: {}", self.message)
    }
}

pub trait ErrorReporter {
    fn report(&self, pos: Pos, message: String);
}

#[derive(Default)]
pub struct ErrorManager {
    errors: RefCell<IndexSet<Error>>,
}

impl ErrorReporter for ErrorManager {
    fn report(&self, pos: Pos, message: String) {
        self.errors.borrow_mut().insert(Error { pos, message });
    }
}

impl ErrorManager {
    pub fn take(&mut self) -> Vec<Error> {
        let mut errs = self.errors.borrow_mut();
        errs.drain(..).collect()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_empty()
    }
}
