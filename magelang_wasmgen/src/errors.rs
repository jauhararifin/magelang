use magelang_syntax::{ErrorReporter, Location, Pos};
use std::path::Path;

pub(crate) trait CodegenError: ErrorReporter {
    fn cannot_read_file(&self, pos: Pos, path: &Path, err: std::io::Error) {
        self.report(pos, format!("Cannot open file {path:?}: {err}"));
    }

    fn duplicated_embed_file_annotation(&self, redeclared_at: Pos, declared_at: Location) {
        self.report(
            redeclared_at,
            format!("Found multiple @embed_file annotations. First declared at {declared_at}"),
        );
    }

    fn annotation_arg_mismatch(&self, pos: Pos, name: &str, expected: usize, found: usize) {
        self.report(
            pos,
            format!("Expecting {expected} argument(s) for {name} annotation, but found {found}"),
        );
    }
}
