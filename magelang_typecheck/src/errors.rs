use magelang_common::{Error, PosInfo, Pos};
use magelang_syntax::Token;
use std::fmt::Display;
use std::num::ParseFloatError;

pub(crate) fn redeclared_symbol(name: &str, declared_at: PosInfo, redeclared_at: Pos) -> Error {
    Error::new(
        redeclared_at,
        format!("Symbol {name} is redeclared. First declared at {declared_at}"),
    )
}

pub(crate) fn invalid_integer_literal(pos: Pos, _parse_int_err: ()) -> Error {
    Error::new(pos, format!("Invalid integer literal"))
}

pub(crate) fn invalid_real_literal(pos: Pos, parse_real_err: ParseFloatError) -> Error {
    Error::new(pos, format!("Invalid float literal: {parse_real_err}"))
}

pub(crate) fn empty_package_path(pos: Pos) -> Error {
    Error::new(pos, String::from("Package path cannot be an empty string"))
}

pub(crate) fn not_a_valid_utf8_package(pos: Pos) -> Error {
    Error::new(pos, String::from("Package path is not a valid utf-8 string"))
}

pub(crate) fn not_a_value(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression is not a value"))
}

pub(crate) fn not_a_type(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression is not a type"))
}

pub(crate) fn not_a_func(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression is not a function"))
}

pub(crate) fn not_indexable(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression is not indexable"))
}

pub(crate) fn cannot_used_as_index(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression can't be used as an index"))
}

pub(crate) fn missing_return_value(pos: Pos) -> Error {
    Error::new(pos, String::from("Missing return value"))
}

pub(crate) fn unreachable_statement(pos: Pos) -> Error {
    Error::new(pos, String::from("This statement will never be executed"))
}

pub(crate) fn function_is_void(pos: Pos) -> Error {
    Error::new(
        pos,
        String::from("Function doesn't have return type and shouldn't return any value"),
    )
}

pub(crate) fn type_mismatch(pos: Pos, expected: impl Display, got: impl Display) -> Error {
    Error::new(pos, format!("Cannot use {got} for type {expected}"))
}

pub(crate) fn binop_type_mismatch(pos: Pos, op: impl Display, a: impl Display, b: impl Display) -> Error {
    Error::new(pos, format!("Cannot perform {op} operation for {a} and {b}"))
}

pub(crate) fn binop_type_unsupported(pos: Pos, op: impl Display, ty: impl Display) -> Error {
    Error::new(pos, format!("Cannot perform {op} binary operation on {ty}"))
}

pub(crate) fn unop_type_unsupported(pos: Pos, op: impl Display, ty: impl Display) -> Error {
    Error::new(pos, format!("Cannot perform {op} unary operation on {ty}"))
}

pub(crate) fn casting_unsupported(pos: Pos, initial_ty: impl Display, target_ty: impl Display) -> Error {
    Error::new(
        pos,
        format!("Cannot perform casting from {initial_ty} into {target_ty}"),
    )
}

pub(crate) fn undeclared_symbol(token: Token) -> Error {
    Error::new(
        token.pos.clone(),
        format!("Symbol {} is not declared yet", token.value.as_ref()),
    )
}

pub(crate) fn missing_return_statement(pos: Pos) -> Error {
    Error::new(pos, String::from("Missing return statement for function"))
}

pub(crate) fn missing_main() -> Error {
    Error::standalone(String::from("The root package doesn't have main function"))
}

pub(crate) fn invalid_main_func(pos: Pos) -> Error {
    Error::new(
        pos,
        String::from("Invalid signature for main function. Main function cannot have any parameters nor return value"),
    )
}

pub(crate) fn unmatch_function_arguments(pos: Pos, expected: usize, found: usize) -> Error {
    Error::new(
        pos,
        format!("Function expects {expected} arguments, but found {found}"),
    )
}

pub(crate) fn expr_unassignable(pos: Pos) -> Error {
    Error::new(pos, String::from("Expression is not assignable"))
}

pub(crate) fn not_in_a_loop(pos: Pos, name: &str) -> Error {
    Error::new(pos, format!("{name} can only be used in a loop"))
}
