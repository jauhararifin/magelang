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
    panic_on_error: bool,
    errors: RefCell<IndexSet<Error>>,
}

impl ErrorReporter for ErrorManager {
    fn report(&self, pos: Pos, message: String) {
        let err = Error { pos, message };
        if self.panic_on_error {
            panic!("pos={:?} message={}", err.pos, err.message);
        }
        self.errors.borrow_mut().insert(err);
    }

    fn has_errors(&self) -> bool {
        !self.errors.borrow().is_empty()
    }
}

impl ErrorManager {
    pub fn new_for_debug() -> Self {
        Self {
            panic_on_error: true,
            errors: RefCell::default(),
        }
    }

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
