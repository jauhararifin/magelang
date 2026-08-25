import mem "std/mem";

struct Vector<T>{
  arr: [*]T,
  cap: usize,
  len: usize,
}

fn init<T>(v: *Vector<T>) {
  v.* = Vector::<T>{
    cap: 0,
    len: 0,
    arr: 0 as [*]T,
  };
}

fn init_with_cap<T>(v: *Vector<T>, cap: usize)
  where T: @derefable
{
  v.* = Vector::<T>{
    cap: 0,
    len: 0,
    arr: 0 as [*]T,
  };
  reserve::<T>(v, cap);
}

fn deinit<T>(v: *Vector<T>) {
  if v.arr.* as usize != 0 {
    mem::dealloc_array::<T>(v.arr.*);
  }
  init::<T>(v);
}

fn clear<T>(v: *Vector<T>) {
  v.len.* = 0;
}

fn len<T>(v: *Vector<T>): usize {
  return v.len.*;
}

fn cap<T>(v: *Vector<T>): usize {
  return v.cap.*;
}

fn is_empty<T>(v: *Vector<T>): bool {
  return v.len.* == 0;
}

fn reserve<T>(v: *Vector<T>, new_cap: usize)
  where T: @derefable
{
  if new_cap <= v.cap.* {
    return;
  }

  let arr = mem::alloc_array::<T>(new_cap);
  let i: usize = 0;
  while i < v.len.* {
    arr[i].* = v.arr.*[i].*;
    i = i + 1;
  }

  if v.arr.* as usize != 0 {
    mem::dealloc_array::<T>(v.arr.*);
  }

  v.arr.* = arr;
  v.cap.* = new_cap;
}

fn resize<T>(v: *Vector<T>, new_len: usize, value: T)
  where T: @derefable
{
  reserve::<T>(v, new_len);
  while v.len.* < new_len {
    v.arr.*[v.len.*].* = value;
    v.len.* = v.len.* + 1;
  }
  if new_len < v.len.* {
    v.len.* = new_len;
  }
}

fn push<T>(v: *Vector<T>, item: T)
  where T: @derefable
{
  if v.len.* == v.cap.* {
    let new_cap: usize = 1;
    if v.cap.* != 0 {
      new_cap = v.cap.* * 2;
    }
    reserve::<T>(v, new_cap);
  }

  v.arr.*[v.len.*].* = item;
  v.len.* = v.len.* + 1;
}

fn pop<T>(v: *Vector<T>, out: *T): bool
  where T: @derefable
{
  if v.len.* == 0 {
    return false;
  }

  v.len.* = v.len.* - 1;
  out.* = v.arr.*[v.len.*].*;
  return true;
}

fn back<T>(v: *Vector<T>, out: *T): bool
  where T: @derefable
{
  if v.len.* == 0 {
    return false;
  }

  out.* = v.arr.*[v.len.* - 1].*;
  return true;
}

fn set<T>(v: *Vector<T>, i: usize, val: T)
  where T: @derefable
{
  v.arr.*[i].* = val;
}

fn get<T>(v: *Vector<T>, i: usize): T
  where T: @derefable
{
  return v.arr.*[i].*;
}

fn insert<T>(v: *Vector<T>, i: usize, val: T)
  where T: @derefable
{
  let idx = i;
  if idx > v.len.* {
    idx = v.len.*;
  }

  if v.len.* == v.cap.* {
    let new_cap: usize = 1;
    if v.cap.* != 0 {
      new_cap = v.cap.* * 2;
    }
    reserve::<T>(v, new_cap);
  }

  let j = v.len.*;
  while j > idx {
    v.arr.*[j].* = v.arr.*[j - 1].*;
    j = j - 1;
  }
  v.arr.*[idx].* = val;
  v.len.* = v.len.* + 1;
}

fn remove<T>(v: *Vector<T>, i: usize): T
  where T: @derefable
{
  let result = v.arr.*[i].*;
  let j = i;
  while j + 1 < v.len.* {
    v.arr.*[j].* = v.arr.*[j + 1].*;
    j = j + 1;
  }
  v.len.* = v.len.* - 1;
  return result;
}

fn swap<T>(v: *Vector<T>, a: usize, b: usize)
  where T: @derefable
{
  let tmp = v.arr.*[a].*;
  v.arr.*[a].* = v.arr.*[b].*;
  v.arr.*[b].* = tmp;
}
