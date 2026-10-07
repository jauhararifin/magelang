# Parser fuzz testing

The grammar-based generator produces random **syntactically valid** programs and
passes them through the scanner and parser, including AST destruction. Any parser
panic, abort, or diagnostic is a failure. A diagnostic can indicate either a parser
bug or an incorrect generator rule; failing inputs are not discarded or retried.

This is parser testing, not whole-compiler testing: identifiers need not resolve,
expressions need not type-check, and generated programs are never executed. It is
not a coverage-guided fuzzer and does not automatically minimize failures.

## Quick deterministic checks

A fixed corpus of 1,024 seeds runs as part of `cargo test -p magelang_syntax`:

```bash
cargo test -p magelang_syntax --test generated_programs
```

## Longer randomized runs

From the repository root:

```bash
cargo run -p magelang_syntax --release --example fuzz_parser -- --cases 10000
```

Omitting `--seed` chooses a random starting seed and prints it. Each case uses the
next seed, so a particular input can be regenerated without replaying earlier cases.
For a repeatable run with explicit bounds:

```bash
cargo run -p magelang_syntax --release --example fuzz_parser -- \
  --seed 42 --cases 10000 --depth 6 --budget 256
```

- `--depth`: recursive grammar depth limit (default 6, maximum 64).
- `--budget`: shared recursive expansion budget per program (default 256).
  Once exhausted, the generator emits terminal forms. List lengths and the number
  of top-level items are also bounded, preventing exponential input growth.
- `--artifacts`: output directory (default `target/parser-fuzz`). Each run gets
  its own subdirectory.

The CLI writes `current.mg` **before every parse**, with the seed, depth, and budget
in its first comment. It stops at the first failure and keeps that input, even if
the parser aborts or overflows the stack. On success, the last input remains for
inspection. A parser hang also leaves the current input; this runner has no
per-case timeout, so terminate it manually or use an external timeout for long runs.

Replay the exact saved source (also works after the generator changes):

```bash
cargo run -p magelang_syntax --release --example fuzz_parser -- \
  --input target/parser-fuzz/<run>/current.mg
```

Or regenerate one case using all three values from its header:

```bash
cargo run -p magelang_syntax --release --example fuzz_parser -- \
  --seed 42 --cases 1 --depth 6 --budget 256
```

Seed reproducibility assumes the same generator and RNG version. Debug builds
can also be used to exercise assertions by omitting `--release`.

## Generated syntax

The generator is in `tests/support/parser_fuzz.rs`, shared by the CLI and tests.
It mixes:

- Imports, annotations, structs, globals, function declarations and definitions.
- Generic parameters, nested type arguments, selected types, pointer/array-pointer
  types, grouped types, and function types with named or unnamed parameters.
- Local declarations, assignments, compound assignments, expression statements,
  blocks, `if`/`else if`/`else`, `while`, `for`, `defer`, returns, and loop jumps.
- Literals, unary/binary operators, mixed-precedence chains, casts, calls, field
  access, indexing, dereferencing, struct literals, and generic instantiations.
- Trailing commas, empty lists where allowed, tight `>>=` / `>>==` / `.*=` token
  boundaries, Unicode identifiers/literals/comments, and LF/CRLF separators.

Nested functions are not emitted: function declarations belong at the top level.
Conditions are grouped to disambiguate struct literals from statement bodies.
Keep new grammar productions bounded and add fixed regression tests in
`tests/parse.rs` for any parser bugs found by fuzzing.
