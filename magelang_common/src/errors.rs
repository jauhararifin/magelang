use crate::pos::Pos;
use std::cell::RefCell;

#[derive(PartialEq, Eq, Debug)]
pub struct Error {
    pub pos: Option<Pos>,
    pub message: String,
}

impl Error {
    pub fn new(pos: Pos, message: String) -> Self {
        Self {
            pos: Some(pos),
            message,
        }
    }

    pub fn standalone(message: String) -> Self {
        Self { pos: None, message }
    }
}

pub struct ErrorAccumulator {
    errors: RefCell<Vec<Error>>,
}

impl Default for ErrorAccumulator {
    fn default() -> Self {
        Self {
            errors: RefCell::new(vec![]),
        }
    }
}

impl ErrorAccumulator {
    // TODO: ignore the exactly same error
    // TODO: if possible, also ignore the same error message in the same line;
    pub fn push(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }

    pub fn take(&self) -> Vec<Error> {
        self.errors.take()
    }
}
