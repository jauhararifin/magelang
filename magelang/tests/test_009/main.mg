import wasm "std/wasm";

struct Pair { a: i32, b: i64 }
struct Big { x: i32, i: Pair, f: f64, y: i32 }

var g: i32 = 0;
var calls: i32 = 0;
// `computed` is declared before the globals it depends on through the init and the condition.
var computed: i32 = compute();
var limit: i32 = 4;
var start: i32 = 1;

@main()
fn main() {
  test_struct_init_and_field_update();
  test_global_loop_var();
  test_cond_evaluated_once_per_iteration();
  test_cond_evaluated_once_per_iteration_with_update();
  test_call_init_and_update_returning_struct();
  test_empty_body();
  test_nested_shadowing();
  test_body_local_shadows_loop_var();
  test_type_only_init();
  test_generic_cond_and_update();
  test_deref_update();
  test_index_update();
  test_zero_iterations();
  test_triple_nesting();
  test_while_for_while();
  test_return_struct_from_nested_loops();
  test_cast_in_update();
  test_for_in_if_in_while();
  test_middle_field_in_cond_and_update();
  test_init_uses_outer_and_cond_uses_init();
  test_jumps_in_else_branches();
  test_sequential_loops_reuse_name();
  test_inner_init_reads_outer_var();
  test_global_init_order_via_init_and_cond();
  test_break_from_deep_nesting_with_updates();
  test_params_before_init_local();
  test_init_shadows_global();
}

fn test_struct_init_and_field_update() {
  var sum: i64 = 0;
  for var b = Big{x: 0, i: Pair{a: 1, b: 10}, f: 0.5, y: 3}; b.x < b.y; b.x = b.x + 1 {
    sum = sum + b.i.b + b.x as i64;
  }
  assert_equal[i64](33, sum);
}

fn test_global_loop_var() {
  var sum: i32 = 0;
  for g = 0; g < 4; g = g + 1 {
    sum = sum + g;
  }
  assert_equal[i32](6, sum);
  assert_equal[i32](4, g);
}

fn bump_calls(): bool {
  calls = calls + 1;
  return calls <= 5;
}

fn test_cond_evaluated_once_per_iteration() {
  calls = 0;
  var iterations: i32 = 0;
  for ; bump_calls(); {
    iterations = iterations + 1;
    if iterations % 2 == 0 {
      continue;
    }
  }
  assert_equal[i32](5, iterations);
  assert_equal[i32](6, calls);
}

fn test_cond_evaluated_once_per_iteration_with_update() {
  calls = 0;
  var iterations: i32 = 0;
  var updates: i32 = 0;
  for ; bump_calls(); updates = updates + 1 {
    iterations = iterations + 1;
    if iterations % 2 == 0 {
      continue;
    }
  }
  assert_equal[i32](5, iterations);
  assert_equal[i32](5, updates);
  assert_equal[i32](6, calls);
}

fn make_big(): Big {
  calls = calls + 1;
  return Big{x: 1, i: Pair{a: 2, b: 3}, f: 4.5, y: 6};
}

fn test_call_init_and_update_returning_struct() {
  calls = 0;
  var n: i32 = 0;
  for make_big(); n < 3; make_big() {
    n = n + 1;
  }
  assert_equal[i32](4, calls);
}

fn test_empty_body() {
  var i: i32 = 0;
  for ; i < 10; i = i + 1 {}
  assert_equal[i32](10, i);
}

fn test_nested_shadowing() {
  var count: i32 = 0;
  for var i: i32 = 0; i < 2; i = i + 1 {
    for var i: i32 = 0; i < 3; i = i + 1 {
      count = count + 1;
    }
  }
  assert_equal[i32](6, count);
}

fn test_body_local_shadows_loop_var() {
  var count: i32 = 0;
  for var i: i32 = 0; i < 3; i = i + 1 {
    var i: i32 = 100;
    count = count + 1;
    if count > 10 {
      wasm.unreachable();
    }
  }
  assert_equal[i32](3, count);
}

fn test_type_only_init() {
  var sum: i32 = 0;
  for var i: i32; i < 3; i = i + 1 {
    sum = sum + i;
  }
  assert_equal[i32](3, sum);
}

fn steps_until_equal[T](a: T, b: T): i32 {
  var n: i32 = 0;
  for ; a != b; a = b {
    n = n + 1;
  }
  return n;
}

fn test_generic_cond_and_update() {
  assert_equal[i32](1, steps_until_equal[i32](1, 2));
  assert_equal[i32](0, steps_until_equal[i64](7, 7));
  assert_equal[i32](1, steps_until_equal[f64](1.5, 2.5));
  assert_equal[i32](1, steps_until_equal[*i32](8 as *i32, 16 as *i32));
  assert_equal[i32](0, steps_until_equal[bool](true, true));
}

fn test_deref_update() {
  var p = 1024 as *i32;
  p.* = 0;
  var sum: i32 = 0;
  for ; p.* < 4; p.* = p.* + 1 {
    sum = sum + p.*;
  }
  assert_equal[i32](6, sum);
  assert_equal[i32](4, p.*);
}

fn test_index_update() {
  var arr = 2048 as [*]i32;
  for var k: i32 = 0; k < 3; k = k + 1 {
    arr[k].* = k * 2;
  }
  var sum: i32 = 0;
  for ; arr[0].* < 3; arr[0].* = arr[0].* + 1 {
    sum = sum + arr[1].* + arr[2].*;
  }
  assert_equal[i32](18, sum);
  assert_equal[i32](3, arr[0].*);
}

fn test_zero_iterations() {
  calls = 0;
  var updates: i32 = 0;
  var body: i32 = 0;
  for make_big(); false; updates = updates + 1 {
    body = body + 1;
  }
  assert_equal[i32](1, calls);
  assert_equal[i32](0, updates);
  assert_equal[i32](0, body);
}

fn test_triple_nesting() {
  var count: i32 = 0;
  for var i: i32 = 0; i < 3; i = i + 1 {
    for var j: i32 = 0; j < 3; j = j + 1 {
      if j == 1 {
        continue;
      }
      for var k: i32 = 0; k < 3; k = k + 1 {
        if k == 1 {
          break;
        }
        count = count + 1;
      }
      if i == 2 {
        break;
      }
    }
  }
  assert_equal[i32](5, count);
}

fn test_while_for_while() {
  var total: i32 = 0;
  var a: i32 = 0;
  while a < 2 {
    a = a + 1;
    for var b: i32 = 0; b < 2; b = b + 1 {
      var c: i32 = 0;
      while c < 3 {
        c = c + 1;
        if c == 2 { continue; }
        total = total + 1;
      }
      if b == 0 { continue; }
      total = total + 10;
    }
    if a == 1 { continue; }
    total = total + 100;
  }
  assert_equal[i32](128, total);
}

fn find_pair(target: i32): Pair {
  for var i: i32 = 0; i < 10; i = i + 1 {
    for var j: i32 = 0; j < 10; j = j + 1 {
      if i * j == target {
        return Pair{a: i, b: j as i64};
      }
    }
  }
  return Pair{a: -1, b: -1};
}

fn test_return_struct_from_nested_loops() {
  var p = find_pair(6);
  assert_equal[i32](1, p.a);
  assert_equal[i64](6, p.b);
  var q = find_pair(1000);
  assert_equal[i32](-1, q.a);
  assert_equal[i64](-1, q.b);
}

fn test_cast_in_update() {
  var sum: i64 = 0;
  for var i: i64 = 0; i < 3; i = (i as i32 + 1) as i64 {
    sum = sum + i;
  }
  assert_equal[i64](3, sum);
}

fn test_for_in_if_in_while() {
  var n: i32 = 0;
  var total: i32 = 0;
  while n < 3 {
    n = n + 1;
    if n != 2 {
      for var i: i32 = 0; i < 2; i = i + 1 {
        if i == 0 { continue; }
        total = total + 1;
      }
      if n == 3 { break; }
      continue;
    }
    total = total + 10;
  }
  assert_equal[i32](12, total);
}

fn test_middle_field_in_cond_and_update() {
  calls = 0;
  var sum: i64 = 0;
  for var i: i32 = 0; i < make_big().y - 4; i = i + make_big().x {
    sum = sum + make_big().i.b;
  }
  assert_equal[i64](6, sum);
  assert_equal[i32](7, calls);
}

fn test_init_uses_outer_and_cond_uses_init() {
  var start: i32 = 5;
  var seen: i32 = 0;
  for var i: i32 = start * 2; i < start * 3; i = i + 1 {
    seen = seen + 1;
  }
  assert_equal[i32](5, seen);
}

fn test_jumps_in_else_branches() {
  var sum: i32 = 0;
  for var i: i32 = 0; i < 10; i = i + 1 {
    if i % 3 == 0 {
      sum = sum + 100;
    } else if i == 7 {
      break;
    } else {
      continue;
    }
    sum = sum + 1;
  }
  assert_equal[i32](303, sum);
}

fn test_sequential_loops_reuse_name() {
  var a: i32 = 0;
  for var i: i32 = 0; i < 3; i = i + 1 { a = a + i; }
  for var i: i64 = 10; i < 13; i = i + 1 { a = a + i as i32; }
  for var i: i32 = 100; i < 102; i = i + 1 { a = a + i; }
  assert_equal[i32](3 + 33 + 201, a);
}

fn test_inner_init_reads_outer_var() {
  var count: i32 = 0;
  for var i: i32 = 0; i < 3; i = i + 1 {
    for var j: i32 = i; j < 3; j = j + 1 {
      count = count + 1;
    }
  }
  assert_equal[i32](6, count);
}

fn compute(): i32 {
  var total: i32 = 0;
  for var i: i32 = start; i < limit; i = i + 1 {
    total = total + i;
  }
  return total;
}

fn test_global_init_order_via_init_and_cond() {
  assert_equal[i32](6, computed);
}

fn test_break_from_deep_nesting_with_updates() {
  var hits: i32 = 0;
  var outer_updates: i32 = 0;
  for var i: i32 = 0; i < 3; outer_updates = outer_updates + 1 {
    i = i + 1;
    for var j: i32 = 0; j < 3; j = j + 1 {
      if j == 1 {
        if i == 2 {
          break;
        } else {
          continue;
        }
      }
      hits = hits + 1;
    }
  }
  assert_equal[i32](3, outer_updates);
  assert_equal[i32](5, hits);
}

fn first_multiple(a: i32, b: i32, m: i32): i32 {
  for var i: i32 = a; i < b; i = i + 1 {
    if i % m == 0 {
      return i;
    }
  }
  return -1;
}

fn test_whole_struct_update() {
  var sum: i64 = 0;
  for var p = Pair{a: 0, b: 0}; p.a < 3; p = (Pair{a: p.a + 1, b: p.b + 10}) {
    sum = sum + p.b + p.a as i64;
  }
  assert_equal[i64](33, sum);
}

fn test_params_before_init_local() {
  assert_equal[i32](12, first_multiple(10, 20, 6));
  assert_equal[i32](-1, first_multiple(10, 12, 7));
  test_whole_struct_update();
}

fn test_init_shadows_global() {
  calls = 1;
  var seen: i32 = 0;
  for var calls: i32 = 5; calls < 7; calls = calls + 1 {
    seen = seen + calls;
  }
  assert_equal[i32](11, seen);
  assert_equal[i32](1, calls);
}

fn assert_equal[T](expected: T, actual: T) {
  if expected != actual {
    wasm.unreachable();
  }
}
