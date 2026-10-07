fn repeats<T>() {
  if false {
    repeats<T>();
  }
}

fn settles<T>() {
  if false {
    settles<i32>();
  }
}

fn swaps<A, B>() {
  if false {
    swaps<B, A>();
  }
}

@main()
fn main() {
  repeats<i32>();
  settles<i64>();
  swaps<i32, i64>();
}
