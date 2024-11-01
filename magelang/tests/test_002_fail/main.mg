struct A<T>{
  b: B<T>,
}
struct B<T>{
  a: A<T>,
}
fn main() {
  let a: A<i32>;
}
