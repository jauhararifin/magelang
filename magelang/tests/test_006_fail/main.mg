fn generic_add<T, U>(a: T, b: U): U {
  return a + b;
}

fn generic_not<T>(a: T): bool {
  return !a;
}

fn generic_and<T, U>(a: T, b: U): bool {
  return a && b;
}

fn generic_lt<T, U>(a: T, b: U): bool {
  return a < b;
}

@main()
fn main() {
  let a = generic_add::<i32, f64>(1, 2.0);
  let b = generic_not::<i32>(1);
  let c = generic_and::<i32, bool>(1, true);
  let d = generic_lt::<bool, bool>(true, false);
}
