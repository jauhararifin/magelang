struct A<T> {
  x: T,
}

struct B {
  a: A<B>,
}

@main()
fn main() {
  let b: B;
}
