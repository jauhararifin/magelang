import wasm "std/wasm";

// Non-void functions whose body ends with a statement that returns on every path but leaves
// nothing on the operand stack (an if/else where both branches return, a nested block, ...).
// These used to fail wasm validation because the function's `end` saw an empty stack.

struct Pair { a: i32, b: i64 }

let trace: i64 = 0;

@main()
fn main() {
  test_if_else();
  test_if_else_with_defer();
  test_nested_if_else();
  test_else_if_chain();
  test_nested_block();
  test_struct_and_float_returns();
  test_void_functions_do_not_trap();
}

fn mark(n: i64) {
  trace = trace * 10 + n;
}

fn if_else(c: bool): i64 {
  if c {
    return 1;
  } else {
    return 2;
  }
}

fn test_if_else() {
  assert_equal::<i64>(1, if_else(true));
  assert_equal::<i64>(2, if_else(false));
}

fn if_else_with_defer(c: bool): i64 {
  defer mark(3);
  if c {
    defer mark(1);
    return 1;
  } else {
    defer mark(2);
    return 2;
  }
}

fn test_if_else_with_defer() {
  trace = 0;
  assert_equal::<i64>(1, if_else_with_defer(true));
  assert_equal::<i64>(13, trace);
  trace = 0;
  assert_equal::<i64>(2, if_else_with_defer(false));
  assert_equal::<i64>(23, trace);
}

fn nested_if_else(c: bool, d: bool): i32 {
  if c {
    if d {
      return 1;
    } else {
      return 2;
    }
  } else {
    if d {
      return 3;
    } else {
      return 4;
    }
  }
}

fn test_nested_if_else() {
  assert_equal::<i32>(1, nested_if_else(true, true));
  assert_equal::<i32>(2, nested_if_else(true, false));
  assert_equal::<i32>(3, nested_if_else(false, true));
  assert_equal::<i32>(4, nested_if_else(false, false));
}

fn else_if_chain(n: i64): i64 {
  if n == 0 {
    return 10;
  } else if n == 1 {
    return 11;
  } else if n == 2 {
    return 12;
  } else {
    return 13;
  }
}

fn test_else_if_chain() {
  assert_equal::<i64>(10, else_if_chain(0));
  assert_equal::<i64>(11, else_if_chain(1));
  assert_equal::<i64>(12, else_if_chain(2));
  assert_equal::<i64>(13, else_if_chain(3));
}

fn nested_block(c: bool): i64 {
  {
    {
      if c {
        return 5;
      } else {
        return 6;
      }
    }
  }
}

fn test_nested_block() {
  assert_equal::<i64>(5, nested_block(true));
  assert_equal::<i64>(6, nested_block(false));
}

fn pair_if_else(c: bool): Pair {
  if c {
    return Pair{a: 1, b: 2};
  } else {
    return Pair{a: 3, b: 4};
  }
}

fn f64_if_else(c: bool): f64 {
  if c {
    return 1.5;
  } else {
    return 2.5;
  }
}

fn test_struct_and_float_returns() {
  let p = pair_if_else(true);
  assert_equal::<i32>(1, p.a);
  assert_equal::<i64>(2, p.b);
  let q = pair_if_else(false);
  assert_equal::<i32>(3, q.a);
  assert_equal::<i64>(4, q.b);
  assert_equal::<f64>(1.5, f64_if_else(true));
  assert_equal::<f64>(2.5, f64_if_else(false));
}

fn void_if_else(c: bool) {
  if c {
    mark(1);
    return;
  } else {
    mark(2);
    return;
  }
}

fn void_falls_off_the_end(c: bool) {
  if c {
    mark(3);
  }
  mark(4);
}

fn test_void_functions_do_not_trap() {
  trace = 0;
  void_if_else(true);
  void_if_else(false);
  void_falls_off_the_end(true);
  void_falls_off_the_end(false);
  assert_equal::<i64>(12344, trace);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
