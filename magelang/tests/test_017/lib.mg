pub struct Pair[T] { pub value: T }
pub struct Plain { pub value: i32 }

pub let count: i32 = 3;
pub let bytes: string = "ab";
pub let current: Plain = Plain{value: 2};

pub fn make_pair[T](value: T): Pair[T] {
  return Pair[T]{value: value};
}

pub fn add(a: i32, b: i32): i32 {
  return a + b;
}
