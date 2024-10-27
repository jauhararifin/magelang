use magelang_syntax::{ErrorReporter, Location, Pos};
use std::fmt::Display;
use std::num::ParseFloatError;
use std::path::Path;

pub(crate) trait SemanticError: ErrorReporter {
    fn cannot_open_file(&self, pos: Pos, path: &Path, err: std::io::Error) {
        self.report(pos, format!("Cannot open file {path:?}: {}", err));
    }

    fn redeclared_symbol(&self, redeclared_at: Pos, declared_at: Location, name: &str) {
        self.report(
            redeclared_at,
            format!("Symbol {name} is redeclared. First declared at {declared_at}"),
        );
    }

    fn undeclared_symbol(&self, pos: Pos, name: &str) {
        self.report(pos, format!("Symbol {name} is not declared yet"));
    }

    fn invalid_utf8_package(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The package path is not a valid utf-8 string literal"),
        );
    }

    fn invalid_utf8_string(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The expression is not a valid utf-8 string literal"),
        );
    }

    fn type_arguments_count_mismatch(&self, pos: Pos, expected: usize, found: usize) {
        if found == 0 {
            self.report(
                pos,
                format!("Expected {expected} type arguments, but no type arguments found"),
            );
            return;
        }
        self.report(
            pos,
            format!("Expected {expected} type arguments, but found {found}"),
        );
    }

    fn type_mismatch(&self, pos: Pos, expected: impl Display, found: impl Display) {
        self.report(
            pos,
            format!("Mismatch type, expected {expected} but found {found}"),
        )
    }

    fn missing_return(&self, pos: Pos) {
        self.report(pos, String::from("Missing return statement"))
    }

    fn invalid_int_literal(&self, pos: Pos) {
        self.report(pos, String::from("Expression is not a valid int literal"));
    }

    fn overflowed_int_literal(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The integer value is too large to fit in the desired type"),
        );
    }

    fn invalid_float_literal(&self, pos: Pos, err: ParseFloatError) {
        self.report(
            pos,
            format!("Expression is not a valid float literal: {err}"),
        );
    }

    fn deref_non_pointer(&self, pos: Pos) {
        self.report(
            pos,
            String::from("Cannot dereference non-pointer expression"),
        );
    }

    fn unop_type_unsupported(&self, pos: Pos, op: impl Display, ty: impl Display) {
        self.report(pos, format!("Cannot perform {op} operation on {ty}"));
    }

    fn casting_unsupported(&self, pos: Pos, from: impl Display, into: impl Display) {
        self.report(
            pos,
            format!("Cannot perform casting operation from {from} into {into}"),
        );
    }

    fn division_by_zero(&self, pos: Pos) {
        self.report(pos, format!("Cannot perform division by zero"));
    }

    fn mod_by_zero(&self, pos: Pos) {
        self.report(pos, format!("Cannot perform modulo by zero"));
    }

    fn binop_type_mismatch(&self, pos: Pos, op: impl Display, a: impl Display, b: impl Display) {
        self.report(
            pos,
            format!("Cannot perform {op} operation for {a} and {b}"),
        );
    }

    fn binop_type_unsupported(&self, pos: Pos, op: impl Display, ty: impl Display) {
        self.report(pos, format!("Cannot perform {op} binary operation on {ty}"));
    }

    fn compare_opaque(&self, pos: Pos) {
        self.report(
            pos,
            "The opaque type might not be null, non-null opaque can't be compared".to_string(),
        )
    }

    fn not_callable(&self, pos: Pos) {
        self.report(pos, String::from("Expression is not callable"));
    }

    fn wrong_number_of_arguments(&self, pos: Pos, expected: usize, found: usize) {
        self.report(
            pos,
            format!("Expected {expected} arguments, but found {found}"),
        );
    }

    fn not_indexable(&self, pos: Pos) {
        self.report(pos, String::from("Expression is not indexable"));
    }

    fn non_int_index(&self, pos: Pos) {
        self.report(pos, String::from("Cannot use non-int expression as index"));
    }

    fn non_field_type(&self, pos: Pos, name: &str) {
        self.report(
            pos,
            format!("The expression doesn't have a field named '{name}'"),
        );
    }

    fn non_generic_value(&self, pos: Pos) {
        self.report(pos, "The expression is not a generic".to_string());
    }

    fn non_struct_type(&self, pos: Pos) {
        self.report(pos, String::from("The expression is not a struct"));
    }

    fn undeclared_field(&self, pos: Pos, name: &str) {
        self.report(pos, format!("The field {name} is not declared yet"));
    }

    fn unreachable_statement(&self, pos: Pos) {
        self.report(pos, String::from("This statement is unreachable"));
    }

    fn expr_is_not_assignable(&self, pos: Pos) {
        self.report(pos, String::from("The expression is not assignable"))
    }

    fn operation_outside_loop(&self, pos: Pos, operation: &str) {
        self.report(pos, format!("Cannot use {operation} outside loop"))
    }

    fn circular_type(&self, pos: Pos, cycle: &[String]) {
        self.report(
            pos,
            format!(
                "Invalid recursive type:\n\t{} refers to\n\t{}",
                cycle.join(" refers to\n\t"),
                cycle[0]
            ),
        )
    }

    fn circular_initialization(&self, pos: Pos, cycle: &[String]) {
        self.report(
            pos,
            format!(
                "Found a circular global intialization:\n\t{} depends on\n\t{}",
                cycle.join(" depends on\n\t"),
                cycle[0]
            ),
        )
    }
}

impl<T> SemanticError for T where T: ErrorReporter {}
