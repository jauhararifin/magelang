import wasm "std/wasm";

struct Box[T] { value: T }
struct Pair[T] { left: T, right: T }
struct Mapper[A, B] { run: fn(A): B }
struct Marker[T] { value: i32 }
struct Node[T] { value: T, next: *Node[T] }
struct A { value: i32 }
struct B { value: i32 }

fn identity[T](value: T): T { return value; }
fn first[T](a: T, b: T): T { return a; }
fn unbox[T](box: Box[T]): T { return box.value; }
fn with_box[T](value: T, box: Box[T]): T { return value; }
fn head[T](values: [*]T): T { return values[0].*; }
fn call[A, B](f: fn(A): B, value: A): B { return f(value); }
fn widen(value: i32): i64 { return value as i64; }
fn second[A, B](a: A, b: B): B { return b; }

fn forward[A, T](unused: A, value: T): T {
  let box = Box{value: value};
  return identity(unbox(box));
}

fn swap[A, B](a: A, b: B): A {
  return second(b, a);
}

fn zero[T](): T {
  let value: T;
  return value;
}

fn assert_equal[T](expected: T, actual: T) {
  if expected != actual { wasm.unreachable(); }
}

@main()
fn main() {
  let i: i32 = 10;
  assert_equal[i32](10, first(i, 1));
  assert_equal[i32](1, first(1, i));
  assert_equal[i32](10, first(i, 2.5));
  assert_equal[i32](2, first(2.5, i));

  assert_equal[isize](1, first(1, 2));
  assert_equal[f64](1, first(1, 2.5));
  assert_equal[f64](2.5, first(2.5, 1));
  assert_equal[f64](2.5, identity(2.5));
  let f: f32 = 2.5;
  assert_equal[f32](1, first(1, f));

  let pair = Pair{right: i, left: 1};
  assert_equal[i32](1, pair.left);
  let floats = Pair{left: 1, right: 2.5};
  let reversed = Pair{right: 2.5, left: 1};
  assert_equal[f64](floats.left, reversed.left);
  assert_equal[f64](2.5, reversed.right);
  let partial = Pair{left: i};
  assert_equal[i32](0, partial.right);

  let box = Box{value: i};
  assert_equal[i32](7, with_box(7, box));
  let nested = Box{value: box};
  assert_equal[i32](10, unbox(unbox(nested)));
  assert_equal[u8](97, head("abc"));

  let node = Node{value: i, next: 0 as *Node[i32]};
  assert_equal[i32](10, node.value);
  let mapper = Mapper{run: widen};
  assert_equal[i64](12, mapper.run(12));
  assert_equal[i64](12, call(widen, 12));
  assert_equal[i32](12, call(identity[i32], 12));

  assert_equal[i32](10, forward(true, i));
  assert_equal[i32](10, swap(i, true));
  let wide: i64 = 23;
  assert_equal[i64](23, swap(wide, true));
  assert_equal[f32](2.5, swap(f, wide));
  assert_equal[i32](10, (identity)(i));

  let compatible = first(A{value: 1}, B{value: 2});
  assert_equal[i32](1, compatible.value);
  assert_equal[i32](0, zero[i32]());
  let explicit = Marker[bool]{value: 42};
  assert_equal[i32](42, explicit.value);
}
