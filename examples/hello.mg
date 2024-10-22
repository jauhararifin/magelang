import fmt "std/fmt";

@main()
@wasm_export("_start")
fn main() {
  fmt::print_str("Hello, world!\n");
}
