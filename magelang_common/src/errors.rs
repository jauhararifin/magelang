use crate::pos::Pos;
use std::cell::RefCell;

pub struct Error {
    pub span: Option<Pos>,
    pub message: String,
}

impl Error {
    pub fn new(span: Pos, message: String) -> Self {
        Self {
            span: Some(span),
            message,
        }
    }

    pub fn standalone(message: String) -> Self {
        Self { span: None, message }
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
    pub fn push(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }

    pub fn take(&self) -> Vec<Error> {
        self.errors.take()
    }
}
