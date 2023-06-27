use crate::ast::{Loc, Location, PathId};
use magelang_syntax::Pos;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};

pub trait ErrorAccumulator {
    fn report_error(&self, pos: Loc, error: String);

    fn cannot_open_file(&self, path_id: PathId, io_err: &std::io::Error) {
        self.report_error(Loc::new(path_id, Pos::new(0)), format!("Cannot open file: {io_err}"))
    }

    fn redeclared_symbol(&self, name: &str, declared_at: &Location, redeclared_at: Loc) {
        self.report_error(
            redeclared_at,
            format!("Symbol {name} is redeclared. First declared at {declared_at}"),
        )
    }

    fn invalid_utf8_package(&self, loc: Loc) {
        self.report_error(
            loc,
            String::from("The package path is not a valid utf-8 string literal"),
        );
    }

    fn not_a_type(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not a type"));
    }

    fn not_a_value(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not a value"));
    }

    fn not_a_struct(&self, loc: Loc, got: impl Display) {
        self.report_error(loc, format!("Not a struct, but a {got}"));
    }

    fn not_a_generic_type(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not a generic type"));
    }

    fn undeclared_symbol(&self, loc: Loc, name: &str) {
        self.report_error(loc, format!("Symbol {name} is not declared yet"));
    }

    fn wrong_number_of_type_arguments(&self, loc: Loc, expected: usize, found: usize) {
        self.report_error(loc, format!("Expected {expected} type argument(s), but found {found}"));
    }

    fn wrong_number_of_arguments(&self, loc: Loc, expected: usize, found: usize) {
        self.report_error(loc, format!("Expected {expected} argument(s), but found {found}"));
    }

    fn invalid_int_literal(&self, loc: Loc, err: ParseIntError) {
        self.report_error(loc, format!("Expression is not a valid integer literal: {err}"));
    }

    fn invalid_float_literal(&self, loc: Loc, err: ParseFloatError) {
        self.report_error(loc, format!("Expression is not a valid float literal: {err}"));
    }

    fn cannot_deref_non_pointer(&self, loc: Loc) {
        self.report_error(loc, String::from("Cannot dereference non pointer value"));
    }

    fn unop_type_unsupported(&self, loc: Loc, op: impl Display, ty: impl Display) {
        self.report_error(loc, format!("Cannot perform {op} operation on {ty}"));
    }

    fn binop_type_mismatch(&self, loc: Loc, op: impl Display, a: impl Display, b: impl Display) {
        self.report_error(loc, format!("Cannot perform {op} operation for {a} and {b}"));
    }

    fn binop_type_unsupported(&self, loc: Loc, op: impl Display, ty: impl Display) {
        self.report_error(loc, format!("Cannot perform {op} binary operation on {ty}"));
    }

    fn not_callable(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not a callable"));
    }

    fn type_mismatch(&self, loc: Loc, expected: impl Display, got: impl Display) {
        self.report_error(loc, format!("Cannot use {got} for type {expected}"));
    }

    fn unsupported_casting(&self, loc: Loc, from: impl Display, into: impl Display) {
        self.report_error(loc, format!("Cannot perform casting operation from {from} and {into}"));
    }

    fn unexpected_index_num(&self, loc: Loc, expected: usize, found: usize) {
        self.report_error(
            loc,
            format!("Unexpected number of index expression, expected {expected}, found {found}"),
        );
    }

    fn non_int_index(&self, loc: Loc) {
        self.report_error(loc, String::from("Cannot use non-int expression as index"));
    }

    fn not_indexable(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not indexable"));
    }

    fn no_such_field(&self, loc: Loc, name: &str) {
        self.report_error(loc, format!("There is no field called {name}"))
    }

    fn not_assignable(&self, loc: Loc) {
        self.report_error(loc, String::from("Expression is not assignable"));
    }

    fn missing_return_statement(&self, loc: Loc) {
        self.report_error(loc, String::from("Missing return statement in the function"));
    }

    fn unreachable_stmt(&self, loc: Loc) {
        self.report_error(loc, String::from("This statement is unreachable"));
    }

    fn not_in_a_loop(&self, loc: Loc, action: impl Display) {
        self.report_error(loc, format!("{action} can only used inside a loop"));
    }

    fn wrong_number_of_tag_arguments(&self, loc: Loc, name: impl Display, expected: usize, found: usize) {
        self.report_error(
            loc,
            format!("Expected {expected} argument(s) for {name} tag, but found {found}"),
        );
    }

    fn invalid_native_func(&self, loc: Loc) {
        self.report_error(
            loc,
            String::from("The compiler doesn't know how to compile this native function"),
        );
    }

    fn invalid_signature_return_type(&self, loc: Loc, expected: impl Display, got: impl Display) {
        self.report_error(
            loc,
            format!("Invalid native function return type, expected {expected}, got {got}"),
        );
    }
}
