struct Box[T] { value: T }
struct Pair[T] { left: T, right: T }
struct Marker[T] { value: i32 }
struct A { value: i32 }
struct B { value: i32 }
struct Other[T] { item: T }

fn identity[T](value: T): T { return value; }
fn first[T](a: T, b: T): T { return a; }
fn unbox[T](value: Box[T]): T { return value.value; }
fn boxes[T](a: Box[T], b: Box[T]) {}
fn pointers[T](a: *T, b: *T) {}
fn call[A, B](f: fn(A): B, value: A): B { return f(value); }
fn add(a: i32, b: i32): i32 { return a + b; }
fn zero[T](): T { let value: T; return value; }
fn unused[T](value: i32) {}

fn rigid[A, B](a: A, b: B) {
  first(a, b);
}

fn rigid_literal[T](value: T) {
  first(1, value);
}

fn test() {
  let a: i32 = 1;
  let b: i64 = 2;
  first(a, b);
  let pair = Pair{left: a, right: b};
  zero();
  unused(1);
  let marker = Marker{value: 1};
  let empty = Pair{};
  first(1, true);
  unbox(a);
  unbox(Other{item: a});
  boxes(Box{value: A{value: 1}}, Box{value: B{value: 2}});
  pointers(0 as *A, 0 as *B);
  call(add, a);
  first(a);
  identity(a, a);
  let missing = Box{other: a};
  let duplicate = Pair{left: a, left: b, right: a};
  let explicit = Marker[i32, i64]{value: 1};
}

fn partial[A, B](a: A) {}
fn check_partial() {
  partial(1);
}
