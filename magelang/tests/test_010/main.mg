import wasm "std/wasm";

struct Pair { a: i32, b: i64 }

let trace: i64 = 0;
// `computed` is declared before `base`, which it only depends on through a deferred statement.
let computed: i32 = compute();
let base: i32 = 7;

@main()
fn main() {
  test_order();
  test_lifo();
  test_inner_scope();
  test_void_function_end();
  test_conditional();
  test_loop_body();
  test_loop_continue_and_break();
  test_while_defer_break();
  test_for_update_after_body_defers();
  test_return_runs_all_pending();
  test_return_value_evaluated_before_defers();
  test_deferred_block_with_locals();
  test_shadowing();
  test_by_reference();
  test_nested_defer();
  test_struct_call_and_struct_return();
  test_generic();
  test_string_literal_in_defer();
  test_global_init_order();
}

fn mark(n: i64) {
  trace = trace * 10 + n;
}

fn test_order() {
  trace = 0;
  {
    mark(1);
    defer mark(2);
    mark(3);
  }
  assert_equal::<i64>(132, trace);
}

fn test_lifo() {
  trace = 0;
  {
    defer mark(1);
    defer mark(2);
    defer mark(3);
    mark(4);
  }
  assert_equal::<i64>(4321, trace);
}

fn test_inner_scope() {
  trace = 0;
  {
    defer mark(1);
    {
      defer mark(2);
      mark(3);
    }
    mark(4);
  }
  assert_equal::<i64>(3241, trace);
}

fn void_with_defer() {
  defer mark(1);
  mark(2);
}

fn test_void_function_end() {
  trace = 0;
  void_with_defer();
  assert_equal::<i64>(21, trace);
}

fn test_conditional() {
  trace = 0;
  for let i: i64 = 0; i < 3; i = i + 1 {
    if i == 1 {
      defer mark(9);
    }
    mark(i);
  }
  assert_equal::<i64>(912, trace);
}

fn test_loop_body() {
  trace = 0;
  for let i: i64 = 1; i < 4; i = i + 1 {
    defer mark(i);
    mark(0);
  }
  assert_equal::<i64>(10203, trace);
}

fn test_loop_continue_and_break() {
  trace = 0;
  for let i: i64 = 1; i < 10; i = i + 1 {
    defer mark(i);
    if i == 2 {
      continue;
    }
    if i == 4 {
      break;
    }
    mark(0);
  }
  assert_equal::<i64>(12034, trace);
}

fn test_while_defer_break() {
  trace = 0;
  let i: i64 = 0;
  while i < 5 {
    i = i + 1;
    defer mark(i);
    if i == 3 {
      break;
    }
  }
  assert_equal::<i64>(123, trace);
}

fn test_for_update_after_body_defers() {
  trace = 0;
  for let i: i64 = 1; i < 3; mark(8) {
    defer mark(i);
    i = i + 1;
    if i == 2 {
      continue;
    }
    mark(0);
  }
  assert_equal::<i64>(28038, trace);
}

fn returns_with_defers(): i64 {
  defer mark(1);
  {
    defer mark(2);
    if trace == 0 {
      defer mark(3);
      mark(4);
      return 9;
    }
  }
  mark(5);
  return 8;
}

fn test_return_runs_all_pending() {
  trace = 0;
  assert_equal::<i64>(9, returns_with_defers());
  assert_equal::<i64>(4321, trace);
  trace = 7;
  assert_equal::<i64>(8, returns_with_defers());
  assert_equal::<i64>(7251, trace);
}

fn value_before_defer(): i32 {
  let x: i32 = 1;
  defer x = 2;
  return x;
}

fn test_return_value_evaluated_before_defers() {
  assert_equal::<i32>(1, value_before_defer());
}

fn deferred_block_with_locals(early: bool): i64 {
  trace = 0;
  defer {
    let a: i64 = 5;
    let b: i64 = a + 1;
    mark(b);
  }
  if early {
    return 1;
  }
  mark(2);
  return 3;
}

fn test_deferred_block_with_locals() {
  assert_equal::<i64>(1, deferred_block_with_locals(true));
  assert_equal::<i64>(6, trace);
  assert_equal::<i64>(3, deferred_block_with_locals(false));
  assert_equal::<i64>(26, trace);
}

fn test_shadowing() {
  trace = 0;
  {
    let a: i32 = 1;
    defer mark(a as i64);
    let a: i64 = 2;
    defer mark(a);
    mark(3);
  }
  assert_equal::<i64>(321, trace);
}

fn test_by_reference() {
  trace = 0;
  {
    let x: i64 = 1;
    defer mark(x);
    x = 5;
  }
  assert_equal::<i64>(5, trace);
}

fn test_nested_defer() {
  trace = 0;
  {
    defer {
      defer mark(1);
      mark(2);
    }
    mark(3);
  }
  assert_equal::<i64>(321, trace);
}

fn make_pair(): Pair {
  mark(6);
  return Pair{a: 1, b: 2};
}

fn pair_with_defers(): Pair {
  defer make_pair();
  defer mark(7);
  return Pair{a: 3, b: 4};
}

fn test_struct_call_and_struct_return() {
  trace = 0;
  let p = pair_with_defers();
  assert_equal::<i32>(3, p.a);
  assert_equal::<i64>(4, p.b);
  assert_equal::<i64>(76, trace);
}

fn generic_defer<T>(v: T, m: i64): T {
  defer mark(m);
  return v;
}

fn test_generic() {
  trace = 0;
  assert_equal::<i32>(5, generic_defer::<i32>(5, 1));
  assert_equal::<f64>(2.5, generic_defer::<f64>(2.5, 2));
  assert_equal::<i64>(12, trace);
}

fn test_string_literal_in_defer() {
  let s: [*]u8 = "zzz";
  {
    defer s = "deferred";
    assert_equal::<u8>(122, s[0].*);
  }
  assert_equal::<u8>(100, s[0].*);
}

fn compute(): i32 {
  let total: i32 = 0;
  {
    defer total = total + base;
  }
  return total;
}

fn test_global_init_order() {
  assert_equal::<i32>(7, computed);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
