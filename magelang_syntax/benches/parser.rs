use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use magelang_syntax::{ErrorManager, FileManager, parse};
use std::hint::black_box;
use std::path::PathBuf;
use std::time::Duration;

fn benchmark_fixture(
    c: &mut Criterion,
    group_name: &str,
    benchmark_name: &str,
    fixture_name: &str,
    expected_lines: Option<u64>,
    long_running: bool,
) {
    let source_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("benches/fixtures")
        .join(fixture_name);
    let source_len = std::fs::metadata(&source_path)
        .expect("failed to inspect benchmark input")
        .len();

    let mut group = c.benchmark_group(group_name);
    if let Some(lines) = expected_lines {
        group.throughput(Throughput::Elements(lines));
    } else {
        group.throughput(Throughput::Bytes(source_len));
    }
    if long_running {
        group.sample_size(10);
        group.measurement_time(Duration::from_secs(20));
    }
    group.bench_function(benchmark_name, move |b| {
        let source = std::fs::read_to_string(&source_path).expect("failed to read benchmark input");
        if let Some(expected_lines) = expected_lines {
            assert_eq!(
                source
                    .lines()
                    .filter(|line| !line.trim().is_empty())
                    .count(),
                expected_lines as usize
            );
        }

        {
            let mut files = FileManager::default();
            let file = files
                .add_file(source_path.clone(), source)
                .expect("failed to register benchmark input");
            let errors = ErrorManager::default();
            black_box(parse(&errors, &file));
            assert!(
                errors.is_empty(),
                "{fixture_name} must parse without errors"
            );
        }

        b.iter(|| {
            let mut files = FileManager::default();
            let file = files
                .open(black_box(source_path.clone()))
                .expect("failed to read benchmark input");
            let errors = ErrorManager::default();
            let ast = parse(&errors, &file);
            black_box((files, ast))
        });
    });
    group.finish();
}

fn benchmark_parser(c: &mut Criterion) {
    benchmark_fixture(
        c,
        "parser",
        "10k_loc_with_io",
        "parser_10k.mg",
        Some(10_000),
        false,
    );
    benchmark_fixture(
        c,
        "parser/general_1m_loc",
        "with_io",
        "general_1m.mg",
        Some(1_000_000),
        true,
    );
    benchmark_fixture(
        c,
        "parser/deep_scope_512",
        "with_io",
        "deep_scope.mg",
        None,
        false,
    );
    benchmark_fixture(
        c,
        "parser/deep_expression_512",
        "with_io",
        "deep_expression.mg",
        None,
        false,
    );
    benchmark_fixture(
        c,
        "parser/deep_generic_512",
        "with_io",
        "deep_generic.mg",
        None,
        false,
    );
}

criterion_group!(benches, benchmark_parser);
criterion_main!(benches);
