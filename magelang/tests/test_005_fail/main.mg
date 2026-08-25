@main()
fn main() {
  instantiate_bad_ordered();
}

fn needs_ordered<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return b;
  }
  return a;
}

fn missing_ordered_in_caller<T>(a: T, b: T): T
  where T: @comparable<T>
{
  return needs_ordered::<T>(a, b);
}

fn less_with_only_comparable<T>(a: T, b: T): bool
  where T: @comparable<T>
{
  return a < b;
}

fn cast_without_constraint<T, U>(value: T): U {
  return value as U;
}

fn add_without_numeric<T>(a: T, b: T): T
  where T: @comparable<T>
{
  return a + b;
}

fn first_without_derefable<T>(values: [*]T): T {
  return values[0].*;
}

fn takes_same<T>(a: T, b: T): T
  where T: @ordered<T>
{
  return needs_ordered::<T>(a, b);
}

fn mismatched_generic_call<T, U>(a: T, b: U): T
  where T: @ordered<T>, U: @ordered<U>
{
  return takes_same::<T>(a, b);
}

fn instantiate_bad_ordered(): bool {
  return needs_ordered::<bool>(true, false);
}
