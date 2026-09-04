import wasm "std/wasm";
import mem "std/mem";

// Defer edge cases around return paths, variable capture, and the value types that sit on the
// wasm operand stack while deferred code runs.

struct Pair { a: i32, b: i64 }
struct Triple { x: i32, y: f64, z: Pair }

let trace: i64 = 0;

@main()
fn main() {
  test_return_inside_nested_loops();
  test_pending_defers_restored_after_early_return();
  test_return_value_from_call_evaluated_before_defers();
  test_void_return_in_nested_blocks();
  test_struct_return_with_deferred_loops();
  test_return_expression_with_temporaries();
  test_return_from_if_chain_depths();
  test_recursion();
  test_return_in_loop_vs_normal_exit();
  test_return_as_last_statement_of_nested_block();
  test_deferred_assignment_visible_after_block();
  test_deferred_block_locals_in_loop();
  test_parameter_modified_by_defer();
  test_shadowing_inside_loop();
  test_deferred_block_locals_do_not_clobber_later_locals();
  test_deferred_pointer_store();
  test_deferred_struct_pointer_stores();
  test_struct_argument_evaluated_late();
  test_generic_call_in_deferred_expression();
  test_deferred_call_to_function_with_defers();
  test_many_defers_with_every_exit_kind();
  test_i32_return_with_deferred_loop();
  test_i64_return_with_deferred_memory_stores();
  test_f64_return_with_deferred_struct_locals();
  test_nested_struct_return_with_deferred_loops();
  test_pointer_return();
  test_defers_in_loops_inside_deferred_block_on_return();
  test_struct_local_field_deferred_after_return_value();
  test_bool_u8_f32_returns();
  test_middle_field_in_deferred_expression();
  test_recursive_chain_with_block_defers();
  test_generic_deferred_block_with_type_param_local();
}

fn mark(n: i64) {
  trace = trace * 10 + n;
}

fn return_inside_nested_loops(): i64 {
  defer mark(1);
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer mark(2);
    let j: i64 = 0;
    while true {
      j = j + 1;
      defer mark(3);
      if i == 1 && j == 2 {
        return i * 10 + j;
      }
      if j == 2 {
        break;
      }
    }
    mark(4);
  }
  return 99;
}

fn test_return_inside_nested_loops() {
  trace = 0;
  assert_equal::<i64>(12, return_inside_nested_loops());
  assert_equal::<i64>(33423321, trace);
}

fn early_or_late(early: bool): i64 {
  defer mark(1);
  if early {
    return 0;
  }
  defer mark(2);
  {
    defer mark(3);
  }
  return 5;
}

fn test_pending_defers_restored_after_early_return() {
  trace = 0;
  assert_equal::<i64>(0, early_or_late(true));
  assert_equal::<i64>(1, trace);
  trace = 0;
  assert_equal::<i64>(5, early_or_late(false));
  assert_equal::<i64>(321, trace);
}

fn current_trace(): i64 {
  return trace;
}

fn return_call_result(): i64 {
  defer mark(5);
  mark(1);
  return current_trace();
}

fn test_return_value_from_call_evaluated_before_defers() {
  trace = 0;
  assert_equal::<i64>(1, return_call_result());
  assert_equal::<i64>(15, trace);
}

fn void_return(c: bool) {
  defer mark(1);
  {
    defer mark(2);
    if c {
      defer mark(3);
      return;
    }
    mark(4);
  }
  mark(5);
}

fn test_void_return_in_nested_blocks() {
  trace = 0;
  void_return(true);
  assert_equal::<i64>(321, trace);
  trace = 0;
  void_return(false);
  assert_equal::<i64>(4251, trace);
}

fn struct_return_with_deferred_loops(): Pair {
  defer {
    let k: i64 = 0;
    while true {
      k = k + 1;
      if k == 3 {
        break;
      }
    }
    mark(k);
  }
  defer for let j: i64 = 0; j < 2; j = j + 1 {
    if j == 0 {
      continue;
    }
    mark(j);
  }
  return Pair{a: 7, b: 8};
}

fn test_struct_return_with_deferred_loops() {
  trace = 0;
  let p = struct_return_with_deferred_loops();
  assert_equal::<i32>(7, p.a);
  assert_equal::<i64>(8, p.b);
  assert_equal::<i64>(13, trace);
}

fn make_pair(): Pair {
  mark(6);
  return Pair{a: 1, b: 2};
}

fn make_triple(): Triple {
  mark(9);
  return Triple{x: 1, y: 2.0, z: Pair{a: 3, b: 4}};
}

fn return_field_of_call(): i64 {
  defer mark(7);
  return make_pair().b;
}

fn test_return_expression_with_temporaries() {
  trace = 0;
  assert_equal::<i64>(2, return_field_of_call());
  assert_equal::<i64>(67, trace);
}

fn if_chain(n: i64): i64 {
  defer mark(1);
  if n == 0 {
    defer mark(2);
    return 10;
  } else if n == 1 {
    defer mark(3);
    if true {
      defer mark(4);
      return 11;
    }
  } else {
    defer mark(5);
  }
  mark(6);
  return 12;
}

fn test_return_from_if_chain_depths() {
  trace = 0;
  assert_equal::<i64>(10, if_chain(0));
  assert_equal::<i64>(21, trace);
  trace = 0;
  assert_equal::<i64>(11, if_chain(1));
  assert_equal::<i64>(431, trace);
  trace = 0;
  assert_equal::<i64>(12, if_chain(2));
  assert_equal::<i64>(561, trace);
}

fn count_down(n: i64): i64 {
  defer mark(n);
  if n == 0 {
    return 0;
  }
  return count_down(n - 1) + 1;
}

fn test_recursion() {
  trace = 0;
  assert_equal::<i64>(3, count_down(3));
  assert_equal::<i64>(123, trace);
}

fn loop_return(stop: i64): i64 {
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer mark(1);
    if i == stop {
      return i;
    }
    defer mark(2);
    mark(3);
  }
  return 9;
}

fn test_return_in_loop_vs_normal_exit() {
  trace = 0;
  assert_equal::<i64>(1, loop_return(1));
  assert_equal::<i64>(3211, trace);
  trace = 0;
  assert_equal::<i64>(9, loop_return(5));
  assert_equal::<i64>(321321321, trace);
}

fn block_return(): i64 {
  {
    defer mark(1);
    mark(2);
    return 3;
  }
}

fn while_return(): i64 {
  let i: i64 = 0;
  while i < 10 {
    defer mark(4);
    i = i + 1;
    if i == 2 {
      return i;
    }
  }
  return 0;
}

fn test_return_as_last_statement_of_nested_block() {
  trace = 0;
  assert_equal::<i64>(3, block_return());
  assert_equal::<i64>(21, trace);
  trace = 0;
  assert_equal::<i64>(2, while_return());
  assert_equal::<i64>(44, trace);
}

fn test_deferred_assignment_visible_after_block() {
  let x: i64 = 1;
  {
    defer x = 5;
    x = 2;
  }
  assert_equal::<i64>(5, x);
}

fn test_deferred_block_locals_in_loop() {
  trace = 0;
  for let i: i64 = 1; i < 4; i = i + 1 {
    defer {
      let a: i64 = i * 2;
      let b: i64 = a + 1;
      mark(b);
    }
    if i == 2 {
      continue;
    }
    let c: i64 = i;
    mark(c);
  }
  assert_equal::<i64>(13537, trace);
}

fn bump_param(p: i64): i64 {
  defer mark(p);
  p = p + 1;
  return p;
}

fn test_parameter_modified_by_defer() {
  trace = 0;
  assert_equal::<i64>(2, bump_param(1));
  assert_equal::<i64>(2, trace);
}

fn test_shadowing_inside_loop() {
  trace = 0;
  for let i: i64 = 0; i < 2; i = i + 1 {
    let i: i64 = i + 10;
    defer mark(i);
    let i: i64 = i + 10;
    mark(i);
  }
  assert_equal::<i64>(21221, trace);
}

fn deferred_block_then_locals(): i64 {
  defer {
    let a: i64 = 100;
    let b: i64 = a + 1;
    mark(b);
  }
  let c: i64 = 6;
  {
    defer mark(c);
  }
  trace = 0;
  return c;
}

fn test_deferred_block_locals_do_not_clobber_later_locals() {
  trace = 0;
  assert_equal::<i64>(6, deferred_block_then_locals());
  assert_equal::<i64>(101, trace);
}

fn test_deferred_pointer_store() {
  let p: *i64 = mem::alloc::<i64>();
  p.* = 1;
  {
    defer p.* = 5;
    p.* = 2;
  }
  assert_equal::<i64>(5, p.*);
  mem::dealloc::<i64>(p);
}

fn test_deferred_struct_pointer_stores() {
  let p: *Pair = mem::alloc::<Pair>();
  p.a.* = 1;
  p.b.* = 2;
  {
    defer p.b.* = 9;
    defer p.a.* = 8;
  }
  assert_equal::<i32>(8, p.a.*);
  assert_equal::<i64>(9, p.b.*);
  mem::dealloc::<Pair>(p);
}

fn take_pair(p: Pair) {
  mark(p.a as i64);
  mark(p.b);
}

fn test_struct_argument_evaluated_late() {
  trace = 0;
  {
    let p = Pair{a: 3, b: 4};
    defer take_pair(p);
    p.a = 5;
    mark(1);
  }
  assert_equal::<i64>(154, trace);
}

fn identity<T>(v: T): T {
  defer mark(1);
  return v;
}

fn test_generic_call_in_deferred_expression() {
  trace = 0;
  {
    defer mark(identity::<i64>(2));
    mark(3);
  }
  assert_equal::<i64>(312, trace);
}

fn with_defers(early: bool): i64 {
  defer mark(1);
  if early {
    return 2;
  }
  return 9;
}

fn test_deferred_call_to_function_with_defers() {
  trace = 0;
  {
    defer mark(with_defers(true));
    mark(3);
  }
  assert_equal::<i64>(312, trace);
}

fn many_defers(n: i64): i64 {
  defer mark(1);
  defer mark(2);
  defer mark(3);
  defer mark(4);
  defer mark(5);
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer mark(6);
    if i == n {
      return i;
    }
    if i == n + 1 {
      break;
    }
    if i == n + 2 {
      continue;
    }
  }
  return 9;
}

fn test_many_defers_with_every_exit_kind() {
  trace = 0;
  assert_equal::<i64>(1, many_defers(1));
  assert_equal::<i64>(6654321, trace);
  trace = 0;
  assert_equal::<i64>(9, many_defers(7));
  assert_equal::<i64>(66654321, trace);
  trace = 0;
  assert_equal::<i64>(9, many_defers(-1));
  assert_equal::<i64>(654321, trace);
}

fn i32_with_deferred_loop(n: i32): i32 {
  defer {
    let k: i32 = 0;
    while true {
      k = k + 1;
      if k == n {
        break;
      }
    }
    mark(k as i64);
  }
  return n;
}

fn test_i32_return_with_deferred_loop() {
  trace = 0;
  assert_equal::<i32>(3, i32_with_deferred_loop(3));
  assert_equal::<i64>(3, trace);
}

fn i64_with_deferred_memory_stores(): i64 {
  let p: *i64 = mem::alloc::<i64>();
  p.* = 4;
  defer mem::dealloc::<i64>(p);
  defer mark(p.*);
  defer p.* = 5;
  return p.*;
}

fn test_i64_return_with_deferred_memory_stores() {
  trace = 0;
  assert_equal::<i64>(4, i64_with_deferred_memory_stores());
  assert_equal::<i64>(5, trace);
}

fn f64_with_deferred_struct_locals(early: bool): f64 {
  defer {
    let q = Pair{a: 1, b: 2};
    let r = make_pair();
    mark(q.b + r.b);
  }
  if early {
    return 1.5;
  }
  return 2.5;
}

fn test_f64_return_with_deferred_struct_locals() {
  trace = 0;
  assert_equal::<f64>(1.5, f64_with_deferred_struct_locals(true));
  assert_equal::<i64>(64, trace);
  trace = 0;
  assert_equal::<f64>(2.5, f64_with_deferred_struct_locals(false));
  assert_equal::<i64>(64, trace);
}

fn nested_struct_with_deferred_loops(): Triple {
  defer for let j: i64 = 0; j < 3; j = j + 1 {
    defer mark(j);
    if j == 1 {
      continue;
    }
  }
  defer {
    let tr = Triple{x: 1, y: 2.0, z: Pair{a: 3, b: 4}};
    mark(tr.z.b);
  }
  return Triple{x: 7, y: 8.5, z: Pair{a: 9, b: 10}};
}

fn test_nested_struct_return_with_deferred_loops() {
  trace = 0;
  let tr = nested_struct_with_deferred_loops();
  assert_equal::<i32>(7, tr.x);
  assert_equal::<f64>(8.5, tr.y);
  assert_equal::<i32>(9, tr.z.a);
  assert_equal::<i64>(10, tr.z.b);
  assert_equal::<i64>(4012, trace);
}

fn pointer_return(): *i64 {
  let p = mem::alloc::<i64>();
  defer p.* = 42;
  p.* = 1;
  return p;
}

fn test_pointer_return() {
  let p = pointer_return();
  assert_equal::<i64>(42, p.*);
  mem::dealloc::<i64>(p);
}

fn deep(n: i64): i64 {
  defer {
    for let i: i64 = 0; i < 2; i = i + 1 {
      defer {
        defer mark(1);
        mark(2);
      }
      mark(3);
    }
  }
  for let k: i64 = 0; k < 5; k = k + 1 {
    defer mark(4);
    if k == n {
      return k;
    }
  }
  return 9;
}

fn test_defers_in_loops_inside_deferred_block_on_return() {
  trace = 0;
  assert_equal::<i64>(1, deep(1));
  assert_equal::<i64>(44321321, trace);
}

fn struct_local_deferred_field(): Pair {
  let p = Pair{a: 1, b: 2};
  defer p.b = 99;
  defer mark(p.b);
  return p;
}

fn test_struct_local_field_deferred_after_return_value() {
  trace = 0;
  let p = struct_local_deferred_field();
  assert_equal::<i64>(2, p.b);
  assert_equal::<i64>(2, trace);
}

fn bool_return(): bool {
  defer mark(1);
  return true;
}

fn u8_return(): u8 {
  defer mark(2);
  return 200;
}

fn f32_return(): f32 {
  defer mark(3);
  return 5.0;
}

fn test_bool_u8_f32_returns() {
  trace = 0;
  if bool_return() {
    mark(7);
  }
  assert_equal::<u8>(200, u8_return());
  assert_equal::<f32>(5.0, f32_return());
  assert_equal::<i64>(1723, trace);
}

fn middle_field_in_deferred_expression(): Triple {
  defer mark(make_triple().z.a as i64);
  defer mark(make_pair().a as i64 + 5);
  return Triple{x: 7, y: 8.5, z: Pair{a: 9, b: 10}};
}

fn test_middle_field_in_deferred_expression() {
  trace = 0;
  let tr = middle_field_in_deferred_expression();
  assert_equal::<i64>(10, tr.z.b);
  assert_equal::<i64>(6693, trace);
}

fn chain(n: i64): i64 {
  defer mark(n);
  if n == 0 {
    return 0;
  }
  let r: i64 = 0;
  {
    defer r = r + 1;
    r = chain(n - 1);
  }
  return r + 1;
}

fn test_recursive_chain_with_block_defers() {
  trace = 0;
  assert_equal::<i64>(4, chain(2));
  assert_equal::<i64>(12, trace);
}

fn generic_block<T>(v: T, m: i64): T {
  defer {
    let x: T = v;
    let y: T = x;
    mark(m);
  }
  return v;
}

fn test_generic_deferred_block_with_type_param_local() {
  trace = 0;
  assert_equal::<i32>(5, generic_block::<i32>(5, 1));
  assert_equal::<i64>(1, trace);
  let p = generic_block::<Pair>(Pair{a: 2, b: 3}, 4);
  assert_equal::<i32>(2, p.a);
  assert_equal::<i64>(3, p.b);
  assert_equal::<i64>(14, trace);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
