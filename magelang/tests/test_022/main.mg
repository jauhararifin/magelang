struct Pair<T> {
  value: T,
}

fn id<T>(value: T): T {
  return value;
}

@main()
fn main() {
  let pair: Pair<i32>=Pair<i32>{value: 1};
  let same: bool = id<i32>==id<i32>;
  let nested: bool = id<Pair<i32>>==id<Pair<i32>>;
}
