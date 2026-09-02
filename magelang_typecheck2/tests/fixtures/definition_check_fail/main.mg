struct Wrap<T> {
  value: T,
}

// Never instantiated, still checked.
fn add<T>(a: T, b: T): T {
  return a + b;
}

fn plain(): i32 {
  return 1;
}

@main()
fn main() {
  let a: Wrap;
  let b: Wrap<i32, i32>;
  let c = plain::<i32>();
  let d: i32 = wrong_count::<i32>();
  compare_opaque();
}

fn wrong_count<T, U>(): T {
  let x: T;
  return x;
}

fn compare_opaque() {
  let x: opaque;
  let y: opaque;
  assert_equal::<opaque>(x, y);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
  }
}
