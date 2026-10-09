import dep "tests/test_031/dep";
import wasm "std/wasm";

struct Box[T] {
  value: T,
}

struct Pair[T] {
  left: T,
  right: T,
}

struct Wrapper[T] {
  box: Box[T],
}

struct Entry[K, V] {
  key: K,
  value: V,
}

fn identity[T](value: T): T {
  return value;
}

fn first[T](a: T, b: T): T {
  return a;
}

fn unbox[T](value: Box[T]): T {
  return value.value;
}

fn load[T](value: *T): T {
  return value.*;
}

fn pick[A, B](a: A, b: B): A {
  return a;
}

fn forward[T](value: T): T {
  return identity(value);
}

fn apply[T](function: fn(T): T, value: T): T {
  return function(value);
}

fn assert_equal[T](expected: T, actual: T) {
  if expected != actual {
    wasm.unreachable();
  }
}

@main()
fn main() {
  let a: i32 = 10;
  let x = first(a, a);
  assert_equal(a, x);

  let twelve: i32 = 12;
  assert_equal(twelve, first(12, a));

  let literal = identity(42);
  let expected_literal: isize = 42;
  assert_equal(expected_literal, literal);

  let boxed = Box[i64]{value: 33};
  let expected_boxed: i64 = 33;
  assert_equal(expected_boxed, unbox(boxed));

  let inferred_box = Box{value: expected_boxed};
  assert_equal(expected_boxed, inferred_box.value);

  let inferred_literal_box = Box{value: 42};
  assert_equal(expected_literal, inferred_literal_box.value);

  let inferred_pair = Pair{left: 12, right: a};
  assert_equal(twelve, inferred_pair.left);

  let inferred_wrapper = Wrapper{box: inferred_box};
  assert_equal(expected_boxed, inferred_wrapper.box.value);

  let imported_box = dep.Box{value: a};
  assert_equal(a, imported_box.value);

  let inferred_entry = Entry{key: a, value: true};
  assert_equal(a, inferred_entry.key);
  if !inferred_entry.value {
    wasm.unreachable();
  }

  let pointer = 4096 as *i16;
  pointer.* = 27;
  let expected_pointer: i16 = 27;
  assert_equal(expected_pointer, load(pointer));

  assert_equal(a, pick(a, true));
  assert_equal(a, forward(a));
  assert_equal(a, apply(identity[i32], a));
  assert_equal(a, dep.identity(a));
}
