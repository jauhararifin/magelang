struct A {
  x: i32,
  y: i32,
}

struct B {
  x: i32,
  y: i32,
}

fn take_a(a: A): i32 {
  return a.x;
}

@main()
fn main() {
  let b = B{x: 1, y: 2};
  take_a(b);
  let a: A = b;
}
