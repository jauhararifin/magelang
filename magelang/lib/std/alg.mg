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

fn product<T>(values: [*]T, len: usize): T
  where T: @numeric, T: @derefable
{
  let result: T = 1;
  let i: usize = 0;
  while i < len {
    result = result * values[i].*;
    i = i + 1;
  }
  return result;
}

fn fill<T>(values: [*]T, len: usize, value: T)
  where T: @derefable
{
  let i: usize = 0;
  while i < len {
    values[i].* = value;
    i = i + 1;
  }
}

fn copy<T>(dst: [*]T, src: [*]T, len: usize)
  where T: @derefable
{
  let i: usize = 0;
  while i < len {
    dst[i].* = src[i].*;
    i = i + 1;
  }
}

fn reverse<T>(values: [*]T, len: usize)
  where T: @derefable
{
  if len == 0 {
    return;
  }

  let i: usize = 0;
  let j = len - 1;
  while i < j {
    let tmp = values[i].*;
    values[i].* = values[j].*;
    values[j].* = tmp;
    i = i + 1;
    j = j - 1;
  }
}

fn contains<T>(values: [*]T, len: usize, value: T): bool
  where T: @comparable<T>, T: @derefable
{
  let i: usize = 0;
  while i < len {
    if values[i].* == value {
      return true;
    }
    i = i + 1;
  }
  return false;
}

fn index_of<T>(values: [*]T, len: usize, value: T): usize
  where T: @comparable<T>, T: @derefable
{
  let i: usize = 0;
  while i < len {
    if values[i].* == value {
      return i;
    }
    i = i + 1;
  }
  return len;
}
