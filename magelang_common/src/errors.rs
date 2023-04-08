use crate::pos::Span;
use std::cell::RefCell;

pub struct Error {
    pub span: Option<Span>,
    pub message: String,
}

impl Error {
    pub fn new(span: Span, message: String) -> Self {
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

impl ErrorAccumulator {
    pub fn new() -> Self {
        Self {
            errors: RefCell::new(vec![]),
        }
    }

    pub fn push(&self, err: Error) {
        self.errors.borrow_mut().push(err);
    }

    pub fn take(&self) -> Vec<Error> {
        self.errors.take()
    }
}
