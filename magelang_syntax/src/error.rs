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

    fn has_errors(&self) -> bool;
}

#[derive(Default)]
pub struct ErrorManager {
    errors: RefCell<IndexSet<Error>>,
}

impl ErrorReporter for ErrorManager {
    fn report(&self, pos: Pos, message: String) {
        self.errors.borrow_mut().insert(Error { pos, message });
    }

    fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }
}

impl ErrorManager {
    pub fn take(&mut self) -> Vec<Error> {
        let mut errs = self.errors.borrow_mut();
        let mut errors: Vec<Error> = errs.drain(..).collect();
        errors.sort_by(|a, b| a.pos.cmp(&b.pos));
        errors
    }

    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_empty()
    }
}
