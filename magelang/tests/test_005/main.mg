import wasm "std/wasm";
import mem "std/mem";

@main()
fn main() {
  test_identity_and_choice();
  test_comparable_generics();
  test_ordered_generics();
  test_numeric_generics();
  test_integer_generics();
  test_castable_generics();
  test_generic_recursion();
  test_generic_arrays();
  test_generic_sort();
}

fn test_identity_and_choice() {
  assert_equal::<i32>(42, identity::<i32>(42));
  assert_equal::<bool>(true, identity::<bool>(true));
  assert_equal::<i64>(9, choose::<i64>(true, 9, 10));
  assert_equal::<i64>(10, choose::<i64>(false, 9, 10));
}

fn test_comparable_generics() {
  assert(eq::<i32>(12, 12));
  assert(!eq::<i32>(12, 13));
  assert(neq::<bool>(true, false));
  assert(!neq::<bool>(true, true));
  assert(ordered_is_comparable::<i32>(7, 7));
}

fn test_ordered_generics() {
  assert_equal::<i32>(9, max::<i32>(4, 9));
  assert_equal::<i32>(4, min::<i32>(4, 9));
  assert_equal::<i32>(10, clamp::<i32>(12, 0, 10));
  assert_equal::<i32>(0, clamp::<i32>(-1, 0, 10));
  assert_equal::<i32>(5, clamp::<i32>(5, 0, 10));
  assert_equal::<f64>(3.5, max::<f64>(1.25, 3.5));
  assert_equal::<f64>(1.25, min::<f64>(1.25, 3.5));
}

fn test_numeric_generics() {
  assert_equal::<i32>(0, zero::<i32>());
  assert_equal::<i64>(0, zero::<i64>());
  assert_equal::<f64>(0.0, zero::<f64>());
  assert_equal::<i32>(10, add3::<i32>(2, 3, 5));
  assert_equal::<f64>(6.5, add3::<f64>(1.0, 2.5, 3.0));
  assert_equal::<i32>(25, square::<i32>(5));
  assert_equal::<f64>(6.25, square::<f64>(2.5));
}

fn test_integer_generics() {
  assert_equal::<i32>(15, set_low_bits::<i32>(8));
  assert_equal::<u32>(15, set_low_bits::<u32>(8));
  assert_equal::<i64>(8, shift_left_one::<i64>(4));
  assert_equal::<u64>(8, shift_left_one::<u64>(4));
}

fn test_castable_generics() {
  assert_equal::<i64>(42, cast_to::<i32, i64>(42));
  assert_equal::<f64>(42.0, cast_to::<i32, f64>(42));

  let value = mem::alloc::<i32>();
  value.* = 99;
  let address = cast_to::<*i32, usize>(value);
  let value_again = cast_to::<usize, *i32>(address);
  assert_equal::<i32>(99, value_again.*);
}

fn test_generic_recursion() {
  assert_equal::<i32>(0, countdown_even::<i32>(8));
  assert_equal::<i64>(0, countdown_odd::<i64>(7));
  assert_equal::<i32>(21, recursive_sum::<i32>(6));
  assert_equal::<i64>(55, recursive_sum::<i64>(10));
}

fn test_generic_arrays() {
  let ints = mem::alloc_array::<i32>(5);
  ints[0].* = 5;
  ints[1].* = 4;
  ints[2].* = 3;
  ints[3].* = 2;
  ints[4].* = 1;

  assert_equal::<i32>(15, sum::<i32>(ints, 5));
  assert_equal::<i32>(5, max_array::<i32>(ints, 5));
  assert_equal::<i32>(1, min_array::<i32>(ints, 5));

  let floats = mem::alloc_array::<f64>(4);
  floats[0].* = 1.5;
  floats[1].* = 2.0;
  floats[2].* = 3.25;
  floats[3].* = 4.25;
  assert_equal::<f64>(11.0, sum::<f64>(floats, 4));
  assert_equal::<i64>(14, sum_cast::<i32, i64>(ints, 4));
}

fn test_generic_sort() {
  let ints = mem::alloc_array::<i32>(6);
  ints[0].* = 5;
  ints[1].* = -1;
  ints[2].* = 3;
  ints[3].* = 3;
  ints[4].* = 0;
  ints[5].* = 9;

  sort::<i32>(ints, 6);
  assert_equal::<i32>(-1, ints[0].*);
  assert_equal::<i32>(0, ints[1].*);
  assert_equal::<i32>(3, ints[2].*);
  assert_equal::<i32>(3, ints[3].*);
  assert_equal::<i32>(5, ints[4].*);
  assert_equal::<i32>(9, ints[5].*);

  let floats = mem::alloc_array::<f64>(5);
  floats[0].* = 4.5;
  floats[1].* = 1.25;
  floats[2].* = 3.0;
  floats[3].* = 3.0;
  floats[4].* = -2.5;

  sort::<f64>(floats, 5);
  assert_equal::<f64>(-2.5, floats[0].*);
  assert_equal::<f64>(1.25, floats[1].*);
  assert_equal::<f64>(3.0, floats[2].*);
  assert_equal::<f64>(3.0, floats[3].*);
  assert_equal::<f64>(4.5, floats[4].*);
}

fn identity<T>(value: T): T {
  return value;
}

fn choose<T>(cond: bool, a: T, b: T): T {
  if cond {
    return a;
  }
  return b;
}

fn eq<T>(a: T, b: T): bool
  where T: @comparable<T>
{
  return a == b;
}

fn neq<T>(a: T, b: T): bool
  where T: @comparable<T>
{
  return a != b;
}

fn ordered_is_comparable<T>(a: T, b: T): bool
  where T: @ordered<T>
{
  return a == b;
}

fn max<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return b;
  }
  return a;
}

fn min<T>(a: T, b: T): T
  where T: @ordered<T>
{
  if a < b {
    return a;
  }
  return b;
}

fn clamp<T>(value: T, low: T, high: T): T
  where T: @ordered<T>
{
  return min::<T>(max::<T>(value, low), high);
}

fn zero<T>(): T
  where T: @numeric
{
  return 0;
}

fn add3<T>(a: T, b: T, c: T): T
  where T: @numeric
{
  return a + b + c;
}

fn square<T>(value: T): T
  where T: @numeric
{
  return value * value;
}

fn set_low_bits<T>(value: T): T
  where T: @integer
{
  let mask: T = 7;
  return value | mask;
}

fn shift_left_one<T>(value: T): T
  where T: @integer
{
  let one: T = 1;
  return value << one;
}

fn cast_to<T, U>(value: T): U
  where T: @castable<U>
{
  return value as U;
}

fn countdown_even<T>(n: T): T
  where T: @integer, T: @comparable<T>
{
  let zero: T = 0;
  if n == zero {
    return zero;
  }
  let one: T = 1;
  return countdown_odd::<T>(n - one);
}

fn countdown_odd<T>(n: T): T
  where T: @integer, T: @comparable<T>
{
  let zero: T = 0;
  if n == zero {
    return zero;
  }
  let one: T = 1;
  return countdown_even::<T>(n - one);
}

fn recursive_sum<T>(n: T): T
  where T: @integer, T: @comparable<T>
{
  let zero: T = 0;
  if n == zero {
    return zero;
  }
  let one: T = 1;
  return n + recursive_sum::<T>(n - one);
}

fn sum<T>(values: [*]T, len: usize): T
  where T: @numeric, T: @derefable
{
  let result: T = 0;
  let i: usize = 0;
  while i < len {
    result = result + values[i].*;
    i = i + 1;
  }
  return result;
}

fn sum_cast<T, U>(values: [*]T, len: usize): U
  where T: @castable<U>, T: @derefable, U: @numeric
{
  let result: U = 0;
  let i: usize = 0;
  while i < len {
    result = result + values[i].* as U;
    i = i + 1;
  }
  return result;
}

fn max_array<T>(values: [*]T, len: usize): T
  where T: @ordered<T>, T: @derefable
{
  let result = values[0].*;
  let i: usize = 1;
  while i < len {
    result = max::<T>(result, values[i].*);
    i = i + 1;
  }
  return result;
}

fn min_array<T>(values: [*]T, len: usize): T
  where T: @ordered<T>, T: @derefable
{
  let result = values[0].*;
  let i: usize = 1;
  while i < len {
    result = min::<T>(result, values[i].*);
    i = i + 1;
  }
  return result;
}

fn sort<T>(values: [*]T, len: usize)
  where T: @ordered<T>, T: @derefable
{
  let i: usize = 0;
  while i < len {
    let j = i + 1;
    while j < len {
      if values[j].* < values[i].* {
        swap::<T>(values, i, j);
      }
      j = j + 1;
    }
    i = i + 1;
  }
}

fn swap<T>(values: [*]T, a: usize, b: usize)
  where T: @derefable
{
  let tmp = values[a].*;
  values[a].* = values[b].*;
  values[b].* = tmp;
}

fn assert(cond: bool) {
  if !cond {
    wasm::unreachable();
  }
}

fn assert_equal<T>(expected: T, actual: T)
  where T: @comparable<T>
{
  if expected != actual {
    wasm::unreachable();
  }
}
