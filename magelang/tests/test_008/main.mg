import wasm "std/wasm";

// `computed` is declared before `offset`, so it can only be correct if the global initialization
// order takes the for-loop update statement into account.
let computed: i32 = compute_offset();
let offset: i32 = 100;
let counter: i32 = 0;

@main()
fn main() {
  test_basic();
  test_continue_runs_update();
  test_break();
  test_empty_init();
  test_empty_condition();
  test_empty_update();
  test_all_empty();
  test_nested_labels();
  test_nested_for();
  test_for_inside_while();
  test_while_inside_for();
  test_assign_init_and_call_update();
  test_expr_init();
  test_scoping();
  test_generic();
  test_unconditional_jump();
  test_string_literal_in_init_and_update();
  test_return_inside_for();
  test_global_init_order();
}

fn test_basic() {
  let sum: i32 = 0;
  for let i: i32 = 0; i < 5; i = i + 1 {
    sum = sum + i;
  }
  assert_equal::<i32>(10, sum);
}

fn test_continue_runs_update() {
  // continue has to jump to the update statement, otherwise this loop never ends.
  let sum: i32 = 0;
  let count: i32 = 0;
  for let i: i32 = 0; i < 10; i = i + 1 {
    count = count + 1;
    if i % 2 == 0 {
      continue;
    }
    sum = sum + i;
  }
  assert_equal::<i32>(10, count);
  assert_equal::<i32>(25, sum); // 1 + 3 + 5 + 7 + 9
}

fn test_break() {
  let last: i32 = -1;
  for let i: i32 = 0; i < 100; i = i + 1 {
    if i == 7 {
      break;
    }
    last = i;
  }
  assert_equal::<i32>(6, last);
}

fn test_empty_init() {
  let i: i32 = 0;
  let sum: i32 = 0;
  for ; i < 5; i = i + 1 {
    sum = sum + i;
  }
  assert_equal::<i32>(10, sum);
  assert_equal::<i32>(5, i);
}

fn test_empty_condition() {
  let sum: i32 = 0;
  for let i: i32 = 0;; i = i + 1 {
    if i == 5 {
      break;
    }
    sum = sum + i;
  }
  assert_equal::<i32>(10, sum);
}

fn test_empty_update() {
  let sum: i32 = 0;
  let count: i32 = 0;
  for let i: i32 = 0; i < 5; {
    i = i + 1;
    if i == 2 {
      continue;
    }
    count = count + 1;
    sum = sum + i;
  }
  assert_equal::<i32>(4, count);
  assert_equal::<i32>(13, sum); // 1 + 3 + 4 + 5
}

fn test_all_empty() {
  let count: i32 = 0;
  for ;; {
    count = count + 1;
    if count == 3 {
      break;
    }
  }
  assert_equal::<i32>(3, count);
}

fn test_nested_labels() {
  let sum: i32 = 0;
  for let i: i32 = 0; i < 6; i = i + 1 {
    if i == 1 {
      if true {
        continue;
      }
    } else if i == 4 {
      {
        break;
      }
    }
    sum = sum + i;
  }
  assert_equal::<i32>(5, sum); // 0 + 2 + 3
}

fn test_nested_for() {
  let total: i32 = 0;
  for let i: i32 = 0; i < 3; i = i + 1 {
    for let j: i32 = 0; j < 3; j = j + 1 {
      if j == 2 {
        continue;
      }
      if i == 2 {
        break;
      }
      total = total + 1;
    }
    total = total + 10;
  }
  assert_equal::<i32>(34, total);
}

fn test_for_inside_while() {
  let i: i32 = 0;
  let total: i32 = 0;
  while i < 3 {
    for let j: i32 = 0; j < 3; j = j + 1 {
      if j == 1 {
        continue;
      }
      total = total + 1;
    }
    i = i + 1;
    if i == 2 {
      continue;
    }
    total = total + 100;
  }
  assert_equal::<i32>(206, total);
}

fn test_while_inside_for() {
  let total: i32 = 0;
  for let i: i32 = 0; i < 3; i = i + 1 {
    let j: i32 = 0;
    while j < 5 {
      j = j + 1;
      if j == 2 {
        continue;
      }
      if j == 4 {
        break;
      }
      total = total + 1;
    }
    if i == 1 {
      continue;
    }
    total = total + 100;
  }
  assert_equal::<i32>(206, total);
}

fn test_assign_init_and_call_update() {
  counter = 0;
  let n: i32 = 100;
  for n = 0; n < 4; incr() {
    n = n + 1;
  }
  assert_equal::<i32>(4, n);
  assert_equal::<i32>(4, counter);
}

fn test_expr_init() {
  counter = 0;
  let sum: i32 = 0;
  for incr(); counter < 3; incr() {
    sum = sum + counter;
  }
  assert_equal::<i32>(3, counter);
  assert_equal::<i32>(3, sum); // 1 + 2
}

fn test_scoping() {
  let i: i32 = 42;
  let sum: i32 = 0;
  for let i: i32 = 0; i < 3; i = i + 1 {
    sum = sum + i;
  }
  assert_equal::<i32>(42, i);
  assert_equal::<i32>(3, sum);
}

fn test_generic() {
  assert_equal::<i32>(3, count_with::<i64>(10, 3));
  assert_equal::<i32>(5, count_with::<f64>(1.5, 5));
}

fn count_with<T>(val: T, n: i32): i32 {
  let total: i32 = 0;
  for let i: i32 = 0; i < n; i = i + 1 {
    total = total + 1;
  }
  return total;
}

fn test_unconditional_jump() {
  let count: i32 = 0;
  for let i: i32 = 0; i < 5; i = i + 1 {
    count = count + 1;
    continue;
  }
  assert_equal::<i32>(5, count);

  let count2: i32 = 0;
  for let i: i32 = 0; i < 5; i = i + 1 {
    count2 = count2 + 1;
    break;
  }
  assert_equal::<i32>(1, count2);
}

fn test_string_literal_in_init_and_update() {
  // the string literals below only appear in the init and the update statement, so they are only
  // put in the data segment if the data collector walks those statements.
  let s: [*]u8 = "zzz";
  let n: i32 = 0;
  assert_equal::<u8>(122, s[0].*);
  for let t: [*]u8 = "init"; n < 2; s = "post" {
    n = n + 1;
    assert_equal::<u8>(105, t[0].*);
  }
  assert_equal::<u8>(112, s[0].*);
}

fn test_return_inside_for() {
  assert_equal::<i32>(2, first_even(10));
  assert_equal::<i32>(-1, first_even(2));
}

fn first_even(n: i32): i32 {
  for let i: i32 = 1; i < n; i = i + 1 {
    if i % 2 == 0 {
      return i;
    }
  }
  return -1;
}

fn test_global_init_order() {
  assert_equal::<i32>(300, computed);
}

fn compute_offset(): i32 {
  let total: i32 = 0;
  for let i: i32 = 0; i < 3; total = total + offset {
    i = i + 1;
  }
  return total;
}

fn incr() {
  counter = counter + 1;
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
