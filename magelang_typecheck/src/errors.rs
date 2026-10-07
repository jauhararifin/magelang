use magelang_syntax::{ErrorManager, Location, Pos};
use std::fmt::Display;
use std::num::ParseFloatError;
use std::path::Path;

pub(crate) fn report_cannot_open_file(errors: &ErrorManager, path: &Path, err: std::io::Error) {
    errors.report(None, format!("Cannot open file {path:?}: {err}"));
}

pub(crate) fn report_redeclared_symbol(
    errors: &ErrorManager,
    redeclared_at: Pos,
    declared_at: Location,
    name: &str,
) {
    errors.report(
        redeclared_at,
        format!("Symbol {name} is redeclared. First declared at {declared_at}"),
    );
}

pub(crate) fn report_undeclared_symbol(errors: &ErrorManager, pos: Pos, name: &str) {
    errors.report(pos, format!("Symbol {name} is not declared yet"));
}

pub(crate) fn report_invalid_utf8_package(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The package path is not a valid utf-8 string literal"),
    );
}

pub(crate) fn report_invalid_utf8_string(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The expression is not a valid utf-8 string literal"),
    );
}

pub(crate) fn report_type_arguments_count_mismatch(
    errors: &ErrorManager,
    pos: Pos,
    expected: usize,
    found: usize,
) {
    if found == 0 {
        errors.report(
            pos,
            format!("Expected {expected} type arguments, but no type arguments found"),
        );
        return;
    }
    errors.report(
        pos,
        format!("Expected {expected} type arguments, but found {found}"),
    );
}

pub(crate) fn report_type_mismatch(
    errors: &ErrorManager,
    pos: Pos,
    expected: impl Display,
    found: impl Display,
) {
    errors.report(
        pos,
        format!("Mismatch type, expected {expected} but found {found}"),
    )
}

pub(crate) fn report_missing_return(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Missing return statement"))
}

pub(crate) fn report_invalid_int_literal(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Expression is not a valid int literal"));
}

pub(crate) fn report_overflowed_int_literal(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The integer value is too large to fit in the desired type"),
    );
}

pub(crate) fn report_invalid_float_literal(errors: &ErrorManager, pos: Pos, err: ParseFloatError) {
    errors.report(
        pos,
        format!("Expression is not a valid float literal: {err}"),
    );
}

pub(crate) fn report_deref_non_pointer(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("Cannot dereference non-pointer expression"),
    );
}

pub(crate) fn report_unop_type_unsupported(
    errors: &ErrorManager,
    pos: Pos,
    op: impl Display,
    ty: impl Display,
) {
    errors.report(pos, format!("Cannot perform {op} operation on {ty}"));
}

pub(crate) fn report_casting_unsupported(
    errors: &ErrorManager,
    pos: Pos,
    from: impl Display,
    into: impl Display,
) {
    errors.report(
        pos,
        format!("Cannot perform casting operation from {from} into {into}"),
    );
}

pub(crate) fn report_binop_type_mismatch(
    errors: &ErrorManager,
    pos: Pos,
    op: impl Display,
    a: impl Display,
    b: impl Display,
) {
    errors.report(
        pos,
        format!("Cannot perform {op} operation for {a} and {b}"),
    );
}

pub(crate) fn report_binop_type_unsupported(
    errors: &ErrorManager,
    pos: Pos,
    op: impl Display,
    ty: impl Display,
) {
    errors.report(pos, format!("Cannot perform {op} binary operation on {ty}"));
}

pub(crate) fn report_compare_opaque(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        "The opaque type might not be null, non-null opaque can't be compared".to_string(),
    )
}

pub(crate) fn report_not_callable(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Expression is not callable"));
}

pub(crate) fn report_wrong_number_of_arguments(
    errors: &ErrorManager,
    pos: Pos,
    expected: usize,
    found: usize,
) {
    errors.report(
        pos,
        format!("Expected {expected} arguments, but found {found}"),
    );
}

pub(crate) fn report_not_indexable(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Expression is not indexable"));
}

pub(crate) fn report_non_int_index(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Cannot use non-int expression as index"));
}

pub(crate) fn report_non_field_type(errors: &ErrorManager, pos: Pos, name: &str) {
    errors.report(
        pos,
        format!("The expression doesn't have a field named '{name}'"),
    );
}

pub(crate) fn report_non_generic_value(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, "The expression is not a generic".to_string());
}

pub(crate) fn report_non_struct_type(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("The expression is not a struct"));
}

pub(crate) fn report_undeclared_field(errors: &ErrorManager, pos: Pos, name: &str) {
    errors.report(pos, format!("The field {name} is not declared yet"));
}

pub(crate) fn report_redeclared_field(
    errors: &ErrorManager,
    redeclared_at: Pos,
    declared_at: Location,
    name: &str,
) {
    errors.report(
        redeclared_at,
        format!("Field {name} is already declared at {declared_at}"),
    );
}

pub(crate) fn report_unreachable_statement(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("This statement is unreachable"));
}

pub(crate) fn report_expr_is_not_assignable(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("The expression is not assignable"))
}

pub(crate) fn report_operation_outside_loop(errors: &ErrorManager, pos: Pos, operation: &str) {
    errors.report(pos, format!("Cannot use {operation} outside loop"))
}

pub(crate) fn report_return_inside_defer(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Cannot use return inside defer"))
}

pub(crate) fn report_circular_type(errors: &ErrorManager, pos: Pos, cycle: &[String]) {
    errors.report(
        pos,
        format!(
            "Invalid recursive type:\n\t{} refers to\n\t{}",
            cycle.join(" refers to\n\t"),
            cycle[0]
        ),
    )
}

pub(crate) fn report_circular_initialization(errors: &ErrorManager, pos: Pos, cycle: &[String]) {
    errors.report(
        pos,
        format!(
            "Found a circular global intialization:\n\t{} depends on\n\t{}",
            cycle.join(" depends on\n\t"),
            cycle[0]
        ),
    )
}

pub(crate) fn report_circular_import(errors: &ErrorManager, pos: Pos, cycle: &[String]) {
    errors.report(
        pos,
        format!(
            "Found a circular import:\n\t{} imports\n\t{}",
            cycle.join(" imports\n\t"),
            cycle[0]
        ),
    )
}
