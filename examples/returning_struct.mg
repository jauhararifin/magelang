struct A {
  a: i32,
  b: B,
  c: i32,
}

struct B {
  a: i64,
  b: i64,
}

let global_struct: A;

fn main() {
  global_struct = A{
    a: 10,
    b: returning_b_struct(),
    c: 10,
  };
}

fn returning_b_struct(): B {
  return returning_a_struct().b;
}

fn returning_a_struct(): A {
  return A {
    a: 10,
    b: B {
      a: 21,
      b: 22,
    },
    c: 30,
  };
}

