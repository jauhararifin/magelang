fn permute<A, B, C, D, E>() {
  if false {
    permute<B, A, D, E, C>();
  }
}

@main()
fn main() {
  permute<i8, i16, i32, i64, isize>();
}
