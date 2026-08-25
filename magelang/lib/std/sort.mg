fn swap<T>(values: [*]T, a: usize, b: usize)
  where T: @derefable
{
  let tmp = values[a].*;
  values[a].* = values[b].*;
  values[b].* = tmp;
}

fn is_sorted<T>(values: [*]T, len: usize): bool
  where T: @ordered<T>, T: @derefable
{
  if len < 2 {
    return true;
  }

  let i: usize = 1;
  while i < len {
    if values[i].* < values[i - 1].* {
      return false;
    }
    i = i + 1;
  }
  return true;
}

fn insertion_sort<T>(values: [*]T, len: usize)
  where T: @ordered<T>, T: @derefable
{
  let i: usize = 1;
  while i < len {
    let j = i;
    while j > 0 && values[j].* < values[j - 1].* {
      swap::<T>(values, j, j - 1);
      j = j - 1;
    }
    i = i + 1;
  }
}

fn sort<T>(values: [*]T, len: usize)
  where T: @ordered<T>, T: @derefable
{
  insertion_sort::<T>(values, len);
}
