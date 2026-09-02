struct Wrap<T> {
  value: T,
}

fn deeper<T>(value: T) {
  deeper::<Wrap<T>>(Wrap::<T>{value: value});
}

@main()
fn main() {
  deeper::<i32>(1);
}
