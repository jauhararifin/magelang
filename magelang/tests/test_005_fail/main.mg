import wasm "std/wasm";

struct WithOpaque {
  value: opaque,
}

fn generic_eq<T>(a: T, b: T): bool {
  return a == b;
}

fn generic_neq<T>(a: T, b: T): bool {
  return a != b;
}

@main()
fn main() {
  let a = WithOpaque { value: null };
  let b = WithOpaque { value: null };
  let c = generic_eq::<WithOpaque>(a, b);
  let d = generic_neq::<WithOpaque>(a, b);

  let x = wasm::table_get(0);
  let y = wasm::table_get(1);
  let z = generic_eq::<opaque>(x, y);
}
