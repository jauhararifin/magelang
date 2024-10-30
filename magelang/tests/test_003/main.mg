import wasm "std/wasm";

struct Foo {
  a: i32,
  b: Bar,
  c: i64,
}

struct Bar {
  a: i64,
  b: i64,
}

@main()
fn main() {
  let a: *Foo;
  assert_equal::<usize>(0, a as usize);
  assert_equal::<usize>(0, a.a as usize);
  assert_equal::<usize>(8, a.b as usize);
  assert_equal::<usize>(8, a.b.a as usize);
  assert_equal::<usize>(16, a.b.b as usize);
  assert_equal::<usize>(24, a.c as usize);
}

fn assert(cond: bool) {
  if !cond {
    wasm::unreachable();
  }
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
