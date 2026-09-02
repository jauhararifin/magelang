import wasm "std/wasm";
import mem "std/mem";

struct Pair<T, U> {
  first: T,
  second: U,
}

struct Wrap<T> {
  value: T,
}

@main()
fn main() {
  test_generic_struct();
  test_generic_func_chain();
  test_generic_func_as_value();
  test_size_of_instances();
  test_generic_struct_through_pointer();
}

fn test_generic_struct() {
  let p = Pair::<i32, bool>{first: 10, second: true};
  assert_equal::<i32>(10, p.first);
  assert_equal::<bool>(true, p.second);

  let q = make_pair::<i64, f64>(7, 2.5);
  assert_equal::<i64>(7, first::<i64, f64>(q));
  assert_equal::<f64>(2.5, second::<i64, f64>(q));

  let w = wrap::<Pair<i32, bool>>(p);
  assert_equal::<i32>(10, w.value.first);
}

fn make_pair<T, U>(first: T, second: U): Pair<T, U> {
  return Pair::<T, U>{first: first, second: second};
}

fn first<T, U>(p: Pair<T, U>): T {
  return p.first;
}

fn second<T, U>(p: Pair<T, U>): U {
  return p.second;
}

fn wrap<T>(value: T): Wrap<T> {
  return Wrap::<T>{value: value};
}

// Instantiating `outer` with i32 instantiates `inner` with Wrap<i32>.
fn test_generic_func_chain() {
  assert_equal::<i32>(42, outer::<i32>(42));
}

fn outer<T>(value: T): T {
  let w = inner::<Wrap<T>>(wrap::<T>(value));
  return w.value.value;
}

fn inner<T>(value: T): Wrap<T> {
  return wrap::<T>(value);
}

fn test_generic_func_as_value() {
  let f: fn(i32): i32 = identity::<i32>;
  assert_equal::<i32>(5, f(5));
  let g = identity::<i64>;
  assert_equal::<i64>(6, g(6));
}

fn identity<T>(value: T): T {
  return value;
}

fn test_size_of_instances() {
  assert_equal::<usize>(16, wasm::size_of::<Pair<i32, i64>>());
  assert_equal::<usize>(4, wasm::size_of::<Wrap<i32>>());
  assert_equal::<usize>(0, wasm::size_of::<Wrap<void>>());
}

fn test_generic_struct_through_pointer() {
  let p = mem::alloc::<Pair<i32, i64>>();
  p.first.* = 1;
  p.second.* = 2;
  p.*.first = 3;
  assert_equal::<i32>(3, p.first.*);
  assert_equal::<i64>(2, p.*.second);
}

fn assert_equal<T>(expected: T, actual: T) {
  if expected != actual {
    wasm::unreachable();
  }
}
