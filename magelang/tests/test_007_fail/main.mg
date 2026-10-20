@main()
fn main() {}

fn use_before_definition() {
  var a: i32 = 10;
  defer mark(b);
  var b: i32 = a + 1;
}

fn jump_inside_defer() {
  for var i: i32 = 0; i < 3; i = i + 1 {
    defer break;
    defer continue;
  }
}

fn return_inside_defer(): i32 {
  defer return;
  defer {
    mark(1);
    return 5;
  }
  for var i: i32 = 0; i < 3; i = i + 1 {
    defer {
      if i == 1 {
        return 6;
      }
    }
  }
  return 1;
}

fn mark(n: i32) {}
