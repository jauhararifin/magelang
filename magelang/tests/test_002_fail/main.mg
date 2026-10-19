type A[T] struct {
  b: B[T],
}
type B[T] struct {
  a: A[T],
}
fn main() {
  let a: A[i32];
}
