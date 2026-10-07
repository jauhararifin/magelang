use crate::token::{FileManager, Pos};
use indexmap::IndexSet;
use std::cell::RefCell;
use std::fmt::Display;

#[derive(PartialEq, Eq, Hash)]
pub struct Error {
    pub pos: Option<Pos>,
    pub message: String,
}

impl Error {
    pub fn new(pos: impl Into<Option<Pos>>, message: String) -> Self {
        Self { pos: pos.into(), message }
    }

    pub fn display(&self, file_manager: &FileManager) -> impl Display {
        match self.pos {
            Some(pos) => format!("{}: {}", file_manager.location(pos), self.message),
            None => self.message.clone(),
        }
    }
}

#[derive(Default)]
pub struct ErrorManager {
    panic_on_error: bool,
    errors: RefCell<IndexSet<Error>>,
}

impl ErrorManager {
    pub fn report(&self, pos: impl Into<Option<Pos>>, message: String) {
        let err = Error::new(pos, message);
        if self.panic_on_error {
            panic!("pos={:?} message={}", err.pos, err.message);
        }
        self.errors.borrow_mut().insert(err);
    }

    pub fn new_for_debug() -> Self {
        Self { panic_on_error: true, errors: RefCell::default() }
    }

    pub fn take(&mut self) -> Vec<Error> {
        let errs = self.errors.get_mut();
        let mut errors: Vec<Error> = errs.drain(..).collect();
        errors.sort_by_key(|error| error.pos);
        errors
    }

    pub fn is_empty(&self) -> bool {
        self.errors.borrow().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn take_clears_diagnostics_and_allows_reporting_again() {
        let mut errors = ErrorManager::default();
        assert!(errors.is_empty());

        for _ in 0..2 {
            errors.report(None, "first error".into());
            errors.report(None, "second error".into());
            errors.report(None, "first error".into());
            assert!(!errors.is_empty());
            let messages: Vec<_> = errors.take().into_iter().map(|error| error.message).collect();
            assert_eq!(messages, ["first error", "second error"]);
            assert!(errors.is_empty());
            assert!(errors.take().is_empty());
        }
    }

    #[test]
    #[should_panic(expected = "pos=None message=debug error")]
    fn debug_manager_panics_on_error() {
        ErrorManager::new_for_debug().report(None, "debug error".into());
    }

    #[test]
    fn diagnostics_without_positions_display_only_the_message() {
        let files = FileManager::default();
        let mut errors = ErrorManager::default();
        let message = "Cannot open file missing.mg: not found";
        errors.report(None, message.into());
        errors.report(None, message.into());

        let errors = errors.take();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].pos, None);
        assert_eq!(errors[0].display(&files).to_string(), message);
    }

    #[test]
    fn diagnostics_are_ordered_by_file_line_and_column() {
        let mut files = FileManager::default();
        let first = files.add_file("first.mg".into(), "one\nabcdefgh\nthree".into()).unwrap();
        let second = files.add_file("second.mg".into(), "two".into()).unwrap();
        let positions = [
            Pos { file: first.id, line: 1, col: 1 },
            Pos { file: first.id, line: 2, col: 3 },
            Pos { file: first.id, line: 2, col: 9 },
            Pos { file: first.id, line: 3, col: 1 },
            Pos { file: second.id, line: 1, col: 1 },
        ];
        let mut errors = ErrorManager::default();
        for pos in positions.into_iter().rev() {
            errors.report(pos, "error".into());
        }
        errors.report(positions[0], "error".into());
        let actual: Vec<_> = errors.take().into_iter().map(|error| error.pos).collect();
        assert_eq!(actual, positions.map(Some));
    }
}
