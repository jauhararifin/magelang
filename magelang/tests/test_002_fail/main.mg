struct A[T]{
  b: B[T],
}
struct B[T]{
  a: A[T],
}
fn main() {
  var a: A[i32];
}
