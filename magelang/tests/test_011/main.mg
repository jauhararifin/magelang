import wasm "std/wasm";

struct Pair { a: i32, b: i64 }

let trace: i64 = 0;

@main()
fn main() {
  test_if_and_else_scopes();
  test_else_if_scopes();
  test_deferred_defer_and_empty_block();
  test_nested_scopes_inside_deferred_block();
  test_deferred_if_statement();
  test_three_nested_blocks();
  test_while_each_iteration();
  test_continue_skips_defers_registered_after_it();
  test_break_skips_defers_registered_after_it();
  test_inner_loop_break_keeps_outer_defers();
  test_inner_loop_continue_keeps_outer_defers();
  test_infinite_for_with_break();
  test_while_continue();
  test_break_through_nested_blocks_and_ifs();
  test_deferred_loop_on_continue();
  test_deferred_loop_on_break();
  test_defer_inside_deferred_loop_with_break();
  test_update_sees_deferred_assignment();
  test_inner_while_break_keeps_for_defers();
  test_deferred_while_with_continue();
  test_else_if_chain_with_jumps();
  test_deferred_block_locals_with_nested_defers_on_continue();
  test_loop_terminated_by_deferred_increment();
  test_multiple_defers_per_iteration_with_return();
  test_dropped_struct_result_on_break();
}

fn mark(n: i64) {
  trace = trace * 10 + n;
}

fn if_and_else(c: bool) {
  if c {
    defer mark(1);
    mark(2);
  } else {
    defer mark(3);
    mark(4);
  }
  mark(5);
}

fn test_if_and_else_scopes() {
  trace = 0;
  if_and_else(true);
  assert_equal::<i64>(215, trace);
  trace = 0;
  if_and_else(false);
  assert_equal::<i64>(435, trace);
}

fn else_if(n: i64) {
  if n == 0 {
    defer mark(1);
    mark(2);
  } else if n == 1 {
    defer mark(3);
    mark(4);
  } else {
    defer mark(5);
    mark(6);
  }
  mark(7);
}

fn test_else_if_scopes() {
  trace = 0;
  else_if(0);
  assert_equal::<i64>(217, trace);
  trace = 0;
  else_if(1);
  assert_equal::<i64>(437, trace);
  trace = 0;
  else_if(2);
  assert_equal::<i64>(657, trace);
}

fn test_deferred_defer_and_empty_block() {
  trace = 0;
  {
    defer {}
    defer mark(1);
    defer defer mark(2);
    defer mark(3);
    mark(4);
  }
  assert_equal::<i64>(4321, trace);
}

fn test_nested_scopes_inside_deferred_block() {
  trace = 0;
  {
    defer {
      defer mark(1);
      defer mark(2);
      mark(3);
      {
        defer mark(4);
        mark(5);
      }
      mark(6);
    }
    mark(7);
  }
  assert_equal::<i64>(7354621, trace);
}

fn deferred_if(c: bool) {
  defer if c { mark(1); } else { mark(2); }
  mark(3);
}

fn test_deferred_if_statement() {
  trace = 0;
  deferred_if(true);
  assert_equal::<i64>(31, trace);
  trace = 0;
  deferred_if(false);
  assert_equal::<i64>(32, trace);
}

fn test_three_nested_blocks() {
  trace = 0;
  {
    defer mark(1);
    {
      defer mark(2);
      {
        defer mark(3);
        mark(4);
      }
      mark(5);
    }
    mark(6);
  }
  assert_equal::<i64>(435261, trace);
}

fn test_while_each_iteration() {
  trace = 0;
  let i: i64 = 0;
  while i < 3 {
    defer mark(9);
    mark(i + 1);
    i = i + 1;
  }
  assert_equal::<i64>(192939, trace);
}

fn test_continue_skips_defers_registered_after_it() {
  trace = 0;
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer mark(1);
    if i == 1 {
      continue;
    }
    defer mark(2);
  }
  assert_equal::<i64>(21121, trace);
}

fn break_skips_later_defers() {
  defer mark(9);
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer mark(1);
    if i == 1 {
      break;
    }
    defer mark(2);
    mark(3);
  }
  mark(4);
}

fn test_break_skips_defers_registered_after_it() {
  trace = 0;
  break_skips_later_defers();
  assert_equal::<i64>(321149, trace);
}

fn test_inner_loop_break_keeps_outer_defers() {
  trace = 0;
  for let i: i64 = 0; i < 2; i = i + 1 {
    defer mark(1);
    for let j: i64 = 0; j < 3; j = j + 1 {
      defer mark(2);
      if j == 1 {
        break;
      }
      mark(3);
    }
    mark(4);
  }
  assert_equal::<i64>(3224132241, trace);
}

fn test_inner_loop_continue_keeps_outer_defers() {
  trace = 0;
  for let i: i64 = 0; i < 2; i = i + 1 {
    defer mark(1);
    for let j: i64 = 0; j < 2; j = j + 1 {
      defer mark(2);
      if j == 0 {
        continue;
      }
      mark(3);
    }
    mark(4);
  }
  assert_equal::<i64>(2324123241, trace);
}

fn test_infinite_for_with_break() {
  trace = 0;
  let n: i64 = 0;
  for ;; {
    defer mark(1);
    n = n + 1;
    if n == 3 {
      break;
    }
    mark(2);
  }
  mark(n);
  assert_equal::<i64>(212113, trace);
}

fn test_while_continue() {
  trace = 0;
  let i: i64 = 0;
  while i < 4 {
    i = i + 1;
    defer mark(i);
    if i % 2 == 0 {
      continue;
    }
    mark(9);
  }
  assert_equal::<i64>(912934, trace);
}

fn test_break_through_nested_blocks_and_ifs() {
  trace = 0;
  for let i: i64 = 0; i < 5; i = i + 1 {
    defer mark(1);
    {
      defer mark(2);
      if i >= 1 {
        defer mark(3);
        if true {
          defer mark(4);
          break;
        }
      }
      mark(5);
    }
  }
  mark(6);
  assert_equal::<i64>(52143216, trace);
}

fn test_deferred_loop_on_continue() {
  trace = 0;
  for let i: i64 = 0; i < 2; i = i + 1 {
    defer for let j: i64 = 0; j < 2; j = j + 1 { mark(j + 1); }
    if i == 0 {
      continue;
    }
    mark(5);
  }
  assert_equal::<i64>(12512, trace);
}

fn test_deferred_loop_on_break() {
  trace = 0;
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer for let j: i64 = 0; j < 3; j = j + 1 {
      if j == 1 {
        continue;
      }
      mark(j);
    }
    if i == 1 {
      break;
    }
    mark(9);
  }
  assert_equal::<i64>(90202, trace);
}

fn defer_inside_deferred_loop() {
  defer {
    for let j: i64 = 0; j < 5; j = j + 1 {
      defer mark(j);
      if j == 2 {
        break;
      }
      mark(9);
    }
  }
  mark(7);
}

fn test_defer_inside_deferred_loop_with_break() {
  trace = 0;
  defer_inside_deferred_loop();
  assert_equal::<i64>(790912, trace);
}

fn test_update_sees_deferred_assignment() {
  trace = 0;
  for let i: i64 = 0; i < 6; i = i + 1 {
    defer i = i + 1;
    mark(i + 1);
  }
  assert_equal::<i64>(135, trace);
}

fn test_inner_while_break_keeps_for_defers() {
  trace = 0;
  for let i: i64 = 0; i < 2; i = i + 1 {
    defer mark(1);
    let k: i64 = 0;
    while true {
      k = k + 1;
      defer mark(2);
      if k == 2 {
        break;
      }
    }
    mark(k);
  }
  assert_equal::<i64>(22212221, trace);
}

fn deferred_while_with_continue() {
  defer {
    let k: i64 = 0;
    while k < 3 {
      k = k + 1;
      if k == 2 {
        continue;
      }
      mark(k);
    }
  }
}

fn test_deferred_while_with_continue() {
  trace = 0;
  deferred_while_with_continue();
  assert_equal::<i64>(13, trace);
}

fn test_else_if_chain_with_jumps() {
  trace = 0;
  for let i: i64 = 0; i < 5; i = i + 1 {
    defer mark(1);
    if i == 0 {
      defer mark(2);
      continue;
    } else if i == 1 {
      defer mark(3);
      mark(4);
    } else if i == 2 {
      defer mark(5);
      if true {
        defer mark(6);
        break;
      }
    } else {
      defer mark(7);
    }
    mark(8);
  }
  mark(9);
  assert_equal::<i64>(2143816519, trace);
}

fn test_deferred_block_locals_with_nested_defers_on_continue() {
  trace = 0;
  for let i: i64 = 1; i < 4; i = i + 1 {
    defer {
      let a: i64 = i;
      defer mark(a);
      let b: i64 = a * 2;
      defer mark(b);
    }
    if i != 2 {
      continue;
    }
    mark(9);
  }
  assert_equal::<i64>(2194263, trace);
}

fn loop_terminated_by_deferred_increment(): i64 {
  let i: i64 = 0;
  while i < 3 {
    defer i = i + 1;
    mark(i + 1);
  }
  return i;
}

fn test_loop_terminated_by_deferred_increment() {
  trace = 0;
  assert_equal::<i64>(3, loop_terminated_by_deferred_increment());
  assert_equal::<i64>(123, trace);
}

fn multiple_defers_per_iteration(): i64 {
  for let i: i64 = 1; i < 10; i = i + 1 {
    defer mark(i);
    defer mark(i);
    if i == 3 {
      defer mark(i);
      return i;
    }
  }
  return 0;
}

fn test_multiple_defers_per_iteration_with_return() {
  trace = 0;
  assert_equal::<i64>(3, multiple_defers_per_iteration());
  assert_equal::<i64>(1122333, trace);
}

fn make_pair(): Pair {
  mark(6);
  return Pair{a: 1, b: 2};
}

fn test_dropped_struct_result_on_break() {
  trace = 0;
  for let i: i64 = 0; i < 3; i = i + 1 {
    defer make_pair();
    if i == 1 {
      break;
    }
  }
  assert_equal::<i64>(66, trace);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
