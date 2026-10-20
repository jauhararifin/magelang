pub struct Pair[T] { pub value: T }
pub struct Plain { pub value: i32 }

pub var count: i32 = 3;
pub var bytes: string = "ab";
pub var current: Plain = Plain{value: 2};

pub fn make_pair[T](value: T): Pair[T] {
  return Pair[T]{value: value};
}

pub fn add(a: i32, b: i32): i32 {
  return a + b;
}
