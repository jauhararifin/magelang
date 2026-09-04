@main()
fn main() {}

fn return_in_loops_inside_defer(): i64 {
  defer {
    while true {
      return 1;
    }
  }
  defer for ;; {
    return 2;
  }
  return 3;
}

fn return_in_nested_deferred_block(): i64 {
  defer {
    defer {
      return 1;
    }
  }
  return 2;
}

fn jump_in_deferred_if_and_block() {
  for ;; {
    defer if true {
      break;
    }
    defer {
      continue;
    }
    break;
  }
}

fn deferred_locals_are_not_visible() {
  defer {
    let x: i64 = 1;
  }
  defer let y: i64 = 2;
  x = 3;
  y = 4;
}

fn defer_after_return() {
  return;
  defer mark(1);
}

fn only_defer_is_not_a_return(): i64 {
  defer mark(1);
}

fn mark(n: i64) {}
