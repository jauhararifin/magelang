struct A {
  a: opaque,
  b: *B,
}

struct B {
  a: *A,
  b: opaque,
}

fn main() {
  let x: *A;
  x.* = A{};

  let y = x.*;
}
