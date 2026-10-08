struct Pair[T] { value: T }
struct Plain { value: i32 }

let count: i32 = 3;
let bytes: [*]u8 = "ab";
let current: Plain = Plain{value: 2};

fn make_pair[T](value: T): Pair[T] {
  return Pair[T]{value: value};
}

fn add(a: i32, b: i32): i32 {
  return a + b;
}
