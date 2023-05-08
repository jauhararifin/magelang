use magelang_common::{Error, ErrorAccumulator, FileLoader, Pos};
use magelang_semantic::{TypeDisplay, TypeId, TypeLoader};
use magelang_syntax::Token;
use std::fmt::Display;
use std::num::{ParseFloatError, ParseIntError};

pub(crate) struct TypecheckErrorAccumulator<'err, 'file, 'typ> {
    err_accumulator: &'err ErrorAccumulator,
    file_loader: &'file FileLoader<'err>,
    type_loader: &'typ TypeLoader,
}

impl<'err, 'file, 'typ> TypecheckErrorAccumulator<'err, 'file, 'typ> {
    pub fn new(
        err_accumulator: &'err ErrorAccumulator,
        file_loader: &'file FileLoader<'err>,
        type_loader: &'typ TypeLoader,
    ) -> Self {
        Self {
            err_accumulator,
            file_loader,
            type_loader,
        }
    }

    pub fn empty_package_path(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Package path cannot be an empty string")));
    }

    pub fn not_a_valid_utf8_package(&self, pos: Pos) {
        self.err_accumulator.push(Error::new(
            pos,
            String::from("Package path is not a valid utf-8 string"),
        ));
    }

    pub fn redeclared_symbol(&self, name: &str, declared_at: Pos, redeclared_at: Pos) {
        let declared_at = self
            .file_loader
            .get_file(declared_at.file_id)
            .unwrap()
            .get_pos(&declared_at);
        self.err_accumulator.push(Error::new(
            redeclared_at,
            format!("Symbol {name} is redeclared. First declared at {declared_at}"),
        ));
    }

    pub fn invalid_integer_literal(&self, pos: Pos, parse_int_error: ParseIntError) {
        self.err_accumulator
            .push(Error::new(pos, format!("Invalid integer literal: {parse_int_error}")));
    }

    pub fn invalid_real_literal(&self, pos: Pos, parse_real_err: ParseFloatError) {
        self.err_accumulator
            .push(Error::new(pos, format!("Invalid float literal: {parse_real_err}")));
    }

    pub fn not_a_value(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression is not a value")));
    }

    pub fn not_a_type(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression is not a type")));
    }

    pub fn not_a_func(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression is not a function")));
    }

    pub fn not_a_constant(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("The expression is not a constant")));
    }

    pub fn cannot_deref_a_non_pointer(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Cannot dereference non pointer type")));
    }

    pub fn not_indexable(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression is not indexable")));
    }

    pub fn cannot_used_as_index(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression can't be used as an index")));
    }

    pub fn missing_return_value(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Missing return value")));
    }

    pub fn unreachable_statement(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("This statement will never be executed")));
    }

    pub fn type_mismatch(&self, pos: Pos, expected: TypeId, got: TypeId) {
        let expected = self.type_loader.get_type(expected).unwrap().display(self.type_loader);
        let got = self.type_loader.get_type(got).unwrap().display(self.type_loader);
        self.err_accumulator
            .push(Error::new(pos, format!("Cannot use {got} for type {expected}")));
    }

    pub fn binop_type_mismatch(&self, pos: Pos, op: impl Display, a: TypeId, b: TypeId) {
        let a = self.type_loader.get_type(a).unwrap().display(self.type_loader);
        let b = self.type_loader.get_type(b).unwrap().display(self.type_loader);
        self.err_accumulator.push(Error::new(
            pos,
            format!("Cannot perform {op} operation for {a} and {b}"),
        ));
    }

    pub fn binop_type_unsupported(&self, pos: Pos, op: impl Display, ty: TypeId) {
        let ty = self.type_loader.get_type(ty).unwrap().display(self.type_loader);
        self.err_accumulator
            .push(Error::new(pos, format!("Cannot perform {op} binary operation on {ty}")));
    }

    pub fn unop_type_unsupported(&self, pos: Pos, op: impl Display, ty: TypeId) {
        let ty = self.type_loader.get_type(ty).unwrap().display(self.type_loader);
        self.err_accumulator
            .push(Error::new(pos, format!("Cannot perform {op} unary operation on {ty}")));
    }

    pub fn no_such_builtin(&self, pos: Pos, name: &str) {
        self.err_accumulator
            .push(Error::new(pos, format!("There is no builtin function named {name}")));
    }

    pub fn missing_return_statement(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Missing return statement for function")));
    }

    pub fn missing_main(&self) -> Error {
        Error::standalone(String::from("The root package doesn't have main function"))
    }

    pub fn unmatch_function_arguments(&self, pos: Pos, expected: usize, found: usize) {
        self.err_accumulator.push(Error::new(
            pos,
            format!("Function expects {expected} arguments, but found {found}"),
        ));
    }

    pub fn expr_unassignable(&self, pos: Pos) {
        self.err_accumulator
            .push(Error::new(pos, String::from("Expression is not assignable")));
    }

    pub fn not_in_a_loop(&self, pos: Pos, name: &str) {
        self.err_accumulator
            .push(Error::new(pos, format!("{name} can only be used in a loop")));
    }

    pub fn field_not_found(&self, pos: Pos, field_name: &str) {
        self.err_accumulator
            .push(Error::new(pos, format!("A field named {field_name} is not present")));
    }

    pub fn cannot_use_type_for_local(&self, pos: Pos, ty: TypeId) {
        let ty = self.type_loader.get_type(ty).unwrap().display(self.type_loader);
        self.err_accumulator
            .push(Error::new(pos, format!("Cannot use {ty} type for a local variable")));
    }

    pub fn function_is_void(&self, pos: Pos) {
        self.err_accumulator.push(Error::new(
            pos,
            String::from("Function doesn't have return type and shouldn't return any value"),
        ));
    }

    pub fn casting_unsupported(&self, pos: Pos, initial_ty: TypeId, target_ty: TypeId) {
        let initial_ty = self.type_loader.get_type(initial_ty).unwrap().display(self.type_loader);
        let target_ty = self.type_loader.get_type(target_ty).unwrap().display(self.type_loader);
        self.err_accumulator.push(Error::new(
            pos,
            format!("Cannot perform casting from {initial_ty} into {target_ty}"),
        ));
    }

    pub fn undeclared_symbol(&self, token: Token) {
        self.err_accumulator.push(Error::new(
            token.pos,
            format!("Symbol {} is not declared yet", token.value.as_ref()),
        ));
    }

    pub fn invalid_main_func(&self, pos: Pos) {
        self.err_accumulator.push(Error::new(
            pos,
            String::from(
                "Invalid signature for main function. Main function cannot have any parameters nor return value",
            ),
        ));
    }
}
