#[path = "support/parser_fuzz.rs"]
mod parser_fuzz;

use std::path::Path;

#[test]
fn generated_programs_parse_without_errors() {
    let mut missing_syntax = vec![
        "import ",
        "struct ",
        "fn ",
        "let ",
        "if ",
        "else if ",
        "else {",
        "while ",
        "for ",
        "defer ",
        "return ",
        "break;",
        "continue;",
        " as ",
        "fn(",
        "[*]",
        "pkg.factory<",
        "Record",
        ">>==",
        ".*=",
        "@",
        "😀",
    ];
    for seed in 0..1_024 {
        let depth = [0, 1, 4, 8][seed as usize % 4];
        let source = parser_fuzz::generate(seed, depth, 96);
        missing_syntax.retain(|syntax| !source.contains(syntax));
        let result = std::panic::catch_unwind(|| {
            parser_fuzz::check(Path::new("generated.mg"), source.clone())
        });
        match result {
            Ok(Ok(())) => {}
            Ok(Err(errors)) => panic!("{errors}\n\n{source}"),
            Err(_) => panic!("parser panicked on:\n{source}"),
        }
    }
    assert!(
        missing_syntax.is_empty(),
        "corpus missed: {missing_syntax:?}"
    );
}

#[test]
fn generation_is_reproducible() {
    for seed in [0, 1, 42, u64::MAX] {
        assert_eq!(
            parser_fuzz::generate(seed, 6, 256),
            parser_fuzz::generate(seed, 6, 256)
        );
    }
}

#[test]
fn zero_budget_terminates_even_with_a_large_depth_limit() {
    for seed in [0, 1, 42, u64::MAX] {
        let source = parser_fuzz::generate(seed, u8::MAX, 0);
        assert!(source.len() < 8_192, "terminal-only program is too large");
        parser_fuzz::check(Path::new("terminal.mg"), source.clone())
            .unwrap_or_else(|errors| panic!("{errors}\n\n{source}"));
    }
}

#[test]
fn syntax_errors_are_not_silently_accepted() {
    assert!(parser_fuzz::check(Path::new("invalid.mg"), "fn broken(;".into()).is_err());
}
