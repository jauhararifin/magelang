use magelang_syntax::{ErrorManager, Location, Pos};
use magelang_typecheck::Annotation;
use std::path::Path;

pub(crate) fn report_cannot_read_file(
    errors: &ErrorManager,
    pos: Pos,
    path: &Path,
    err: std::io::Error,
) {
    errors.report(pos, format!("Cannot open file {path:?}: {err}"));
}

pub(crate) fn report_import_generic_func(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Cannot import generic function"));
}

pub(crate) fn report_export_generic_func(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("Cannot export generic function"));
}

pub(crate) fn report_func_both_imported_and_exported(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("Cannot import and export the same function"),
    );
}

pub(crate) fn report_unknown_compilation_strategy(errors: &ErrorManager, pos: Pos) {
    errors.report(pos, String::from("The compilation strategy is unclear"));
}

pub(crate) fn report_dereferencing_opaque(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The expression contains an opaque type which can't be dereferenced"),
    );
}

pub(crate) fn report_storing_opaque(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from(
            "The expression contains an opaque type which can't be stored in linear memory",
        ),
    );
}

pub(crate) fn report_duplicated_import(
    errors: &ErrorManager,
    pos: Pos,
    module: &str,
    name: &str,
    declared_at: Location,
) {
    errors.report(
        pos,
        format!("Found duplicated import. {module}.{name} is already imported at {declared_at}"),
    );
}

pub(crate) fn report_duplicated_export(
    errors: &ErrorManager,
    pos: Pos,
    name: &str,
    declared_at: Location,
) {
    errors.report(
        pos,
        format!("Found duplicated import. {name} is already exported at {declared_at}"),
    );
}

pub(crate) fn report_duplicated_annotation(errors: &ErrorManager, annotation: &Annotation) {
    errors.report(
        annotation.pos,
        format!("Found multiple annotation for {}", annotation.name),
    );
}

pub(crate) fn report_invalid_main_signature(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The function signature can't be used for main function. Main function shouldn't have any parameters nor return any value"),
    );
}

pub(crate) fn report_multiple_main(errors: &ErrorManager, pos: Pos, first_main: Location) {
    errors.report(
        pos,
        format!("Can only have one main function. Main function already declared at {first_main}"),
    );
}

pub(crate) fn report_unknown_intrinsic(errors: &ErrorManager, pos: Pos, name: &str) {
    errors.report(pos, format!("Unknown intrinsic named {name}"));
}

pub(crate) fn report_unknown_annotation(errors: &ErrorManager, annotation: &Annotation) {
    errors.report(
        annotation.pos,
        format!("Unknown annotation named {}", &annotation.name),
    );
}

pub(crate) fn report_intrinsic_signature_mismatch(errors: &ErrorManager, pos: Pos) {
    errors.report(
        pos,
        String::from("The function signature is not compatible for the defined intrinsic"),
    );
}

pub(crate) fn report_annotation_arg_mismatch(
    errors: &ErrorManager,
    annotation: &Annotation,
    expected: usize,
) {
    errors.report(
        annotation.pos,
        format!(
            "Expecting {expected} argument(s) for {} annotation, but found {}",
            &annotation.name,
            annotation.arguments.len()
        ),
    );
}
