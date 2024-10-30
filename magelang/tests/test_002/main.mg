import wasm "std/wasm";
import mem "std/mem";

struct A {
  a: i32,
  b: B,
  c: i32,
}

struct B {
  a: i64,
  b: i64,
}

let global_struct: A;

@main()
fn main() {
  global_struct = A{
    a: 10,
    b: returning_b_struct(),
    c: 10,
  };

  assert_equal::<i32>(10, global_struct.a);
  assert_equal::<i64>(21, global_struct.b.a);
  assert_equal::<i64>(22, global_struct.b.b);
  assert_equal::<i32>(10, global_struct.c);

  global_struct = returning_a_struct();
  assert_equal::<i32>(10, global_struct.a);
  assert_equal::<i64>(21, global_struct.b.a);
  assert_equal::<i64>(22, global_struct.b.b);
  assert_equal::<i32>(30, global_struct.c);

  let a = mem::alloc::<A>();
  a.* = returning_a_struct();
  assert_equal::<i32>(10, a.a.*);
  assert_equal::<i64>(21, a.b.a.*);
  assert_equal::<i64>(22, a.b.b.*);
  assert_equal::<i32>(30, a.c.*);
}

fn returning_b_struct(): B {
  return returning_a_struct().b;
}

fn returning_a_struct(): A {
  return A {
    a: 10,
    b: B {
      a: 21,
      b: 22,
    },
    c: 30,
  };
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
