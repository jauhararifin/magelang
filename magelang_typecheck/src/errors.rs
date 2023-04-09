use magelang_common::{Error, Pos, Span};
use magelang_syntax::Token;
use std::fmt::Display;
use std::num::ParseIntError;

pub(crate) fn redeclared_symbol(name: &str, declared_at: Pos, redeclared_at: Span) -> Error {
    Error::new(
        redeclared_at,
        format!("Symbol {name} is redeclared. First declared at {declared_at}"),
    )
}

pub(crate) fn invalid_integer_literal(span: Span, parse_int_err: ParseIntError) -> Error {
    Error::new(span, format!("Invalid integer literal: {parse_int_err}"))
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

pub(crate) fn missing_return_value(span: Span) -> Error {
    Error::new(span, String::from("Missing return value"))
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

pub(crate) fn undeclared_symbol(token: Token) -> Error {
    Error::new(
        token.span.clone(),
        format!("Symbol {} is not declared yet", token.value.as_ref()),
    )
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
