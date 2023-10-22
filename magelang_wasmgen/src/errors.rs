use magelang_syntax::{ErrorReporter, Location, Pos};
use magelang_typecheck::Annotation;
use std::path::Path;

pub(crate) trait CodegenError: ErrorReporter {
    fn cannot_read_file(&self, pos: Pos, path: &Path, err: std::io::Error) {
        self.report(pos, format!("Cannot open file {path:?}: {err}"));
    }

    fn import_generic_func(&self, pos: Pos) {
        self.report(pos, String::from("Cannot import generic function"));
    }

    fn export_generic_func(&self, pos: Pos) {
        self.report(pos, String::from("Cannot export generic function"));
    }

    fn func_both_imported_and_exported(&self, pos: Pos) {
        self.report(
            pos,
            String::from("Cannot import and export the same function"),
        );
    }

    fn unknown_compilation_strategy(&self, pos: Pos) {
        self.report(pos, String::from("The compilation strategy is unclear"));
    }

    fn dereferencing_opaque(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The expression contains an opaque type which can't be dereferenced"),
        );
    }

    fn storing_opaque(&self, pos: Pos) {
        self.report(
            pos,
            String::from(
                "The expression contains an opaque type which can't be stored in linear memory",
            ),
        );
    }

    fn duplicated_import(&self, pos: Pos, module: &str, name: &str, declared_at: Location) {
        self.report(
            pos,
            format!(
                "Found duplicated import. {module}.{name} is already imported at {declared_at}"
            ),
        );
    }

    fn duplicated_export(&self, pos: Pos, name: &str, declared_at: Location) {
        self.report(
            pos,
            format!("Found duplicated import. {name} is already exported at {declared_at}"),
        );
    }

    fn duplicated_annotation(&self, annotation: &Annotation) {
        self.report(
            annotation.pos,
            format!("Found multiple annotation for {}", annotation.name),
        );
    }

    fn invalid_main_signature(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The function signature can't be used for main function. Main function shouldn't have any parameters nor return any value"),
        );
    }

    fn multiple_main(&self, pos: Pos, first_main: Location) {
        self.report(
            pos,
            format!(
                "Can only have one main function. Main function already declared at {first_main}"
            ),
        );
    }

    fn unknown_intrinsic(&self, pos: Pos, name: &str) {
        self.report(pos, format!("Unknown intrinsic named {name}"));
    }

    fn unknown_annotation(&self, annotation: &Annotation) {
        self.report(
            annotation.pos,
            format!("Unknown annotation named {}", &annotation.name),
        );
    }

    fn intrinsic_signature_mismatch(&self, pos: Pos) {
        self.report(
            pos,
            String::from("The function signature is not compatible for the defined intrinsic"),
        );
    }

    fn annotation_arg_mismatch(&self, annotation: &Annotation, expected: usize) {
        self.report(
            annotation.pos,
            format!(
                "Expecting {expected} argument(s) for {} annotation, but found {}",
                &annotation.name,
                annotation.arguments.len()
            ),
        );
    }
}

impl<T> CodegenError for T where T: ErrorReporter {}
