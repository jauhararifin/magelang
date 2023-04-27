use magelang_common::{Error, Pos, Span};
use magelang_syntax::Token;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};

pub(crate) fn redeclared_symbol(name: &str, declared_at: Pos, redeclared_at: Span) -> Error {
    Error::new(
        redeclared_at,
        format!("Symbol {name} is redeclared. First declared at {declared_at}"),
    )
}

pub(crate) fn invalid_integer_literal(span: Span, parse_int_err: ParseIntError) -> Error {
    Error::new(span, format!("Invalid integer literal: {parse_int_err}"))
}

pub(crate) fn invalid_real_literal(span: Span, parse_real_err: ParseFloatError) -> Error {
    Error::new(span, format!("Invalid float literal: {parse_real_err}"))
}

pub(crate) fn empty_package_path(span: Span) -> Error {
    Error::new(span, String::from("Package path cannot be an empty string"))
}

pub(crate) fn not_a_value(span: Span) -> Error {
    Error::new(span, String::from("Expression is not a value"))
}

pub(crate) fn not_a_type(span: Span) -> Error {
    Error::new(span, String::from("Expression is not a type"))
}

pub(crate) fn not_a_func(span: Span) -> Error {
    Error::new(span, String::from("Expression is not a function"))
}

pub(crate) fn not_indexable(span: Span) -> Error {
    Error::new(span, String::from("Expression is not indexable"))
}

pub(crate) fn cannot_used_as_index(span: Span) -> Error {
    Error::new(span, String::from("Expression can't be used as an index"))
}

pub(crate) fn missing_return_value(span: Span) -> Error {
    Error::new(span, String::from("Missing return value"))
}

pub(crate) fn unreachable_statement(span: Span) -> Error {
    Error::new(span, String::from("This statement will never be executed"))
}

pub(crate) fn function_is_void(span: Span) -> Error {
    Error::new(
        span,
        String::from("Function doesn't have return type and shouldn't return any value"),
    )
}

pub(crate) fn type_mismatch(span: Span, expected: impl Display, got: impl Display) -> Error {
    Error::new(span, format!("Cannot use {got} for type {expected}"))
}

pub(crate) fn binop_type_mismatch(span: Span, op: impl Display, a: impl Display, b: impl Display) -> Error {
    Error::new(span, format!("Cannot perform {op} operation for {a} and {b}"))
}

pub(crate) fn binop_type_unsupported(span: Span, op: impl Display, ty: impl Display) -> Error {
    Error::new(span, format!("Cannot perform {op} binary operation on {ty}"))
}

pub(crate) fn unop_type_unsupported(span: Span, op: impl Display, ty: impl Display) -> Error {
    Error::new(span, format!("Cannot perform {op} unary operation on {ty}"))
}

pub(crate) fn casting_unsupported(span: Span, initial_ty: impl Display, target_ty: impl Display) -> Error {
    Error::new(
        span,
        format!("Cannot perform casting from {initial_ty} into {target_ty}"),
    )
}

pub(crate) fn undeclared_symbol(token: Token) -> Error {
    Error::new(
        token.span.clone(),
        format!("Symbol {} is not declared yet", token.value.as_ref()),
    )
}

pub(crate) fn missing_return_statement(span: Span) -> Error {
    Error::new(span, String::from("Missing return statement for function"))
}

pub(crate) fn missing_main() -> Error {
    Error::standalone(String::from("The root package doesn't have main function"))
}

pub(crate) fn invalid_main_func(span: Span) -> Error {
    Error::new(
        span,
        String::from("Invalid signature for main function. Main function cannot have any parameters nor return value"),
    )
}

pub(crate) fn unmatch_function_arguments(span: Span, expected: usize, found: usize) -> Error {
    Error::new(
        span,
        format!("Function expects {expected} arguments, but found {found}"),
    )
}

pub(crate) fn expr_unassignable(span: Span) -> Error {
    Error::new(span, String::from("Expression is not assignable"))
}

pub(crate) fn not_in_a_loop(span: Span, name: &str) -> Error {
    Error::new(span, format!("{name} can only be used in a loop"))
}
