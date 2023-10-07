use magelang_syntax::{ErrorReporter, Location, Pos};
use magelang_typecheck::Annotation;
use std::path::Path;

pub(crate) trait CodegenError: ErrorReporter {
    fn cannot_read_file(&self, pos: Pos, path: &Path, err: std::io::Error) {
        self.report(pos, format!("Cannot open file {path:?}: {err}"));
    }

    fn duplicated_annotation(&self, annotation: &Annotation) {
        self.report(
            annotation.pos,
            format!("Found multiple annotation for {}", annotation.name),
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
