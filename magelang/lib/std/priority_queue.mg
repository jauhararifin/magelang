import vector "std/vector";

struct PriorityQueue<T> {
  items: vector::Vector<T>,
  is_min: bool,
}

fn init_min<T>(queue: *PriorityQueue<T>) {
  vector::init::<T>(queue.items);
  queue.is_min.* = true;
}

fn init_max<T>(queue: *PriorityQueue<T>) {
  vector::init::<T>(queue.items);
  queue.is_min.* = false;
}

fn init_min_with_cap<T>(queue: *PriorityQueue<T>, cap: usize)
  where T: @derefable
{
  vector::init_with_cap::<T>(queue.items, cap);
  queue.is_min.* = true;
}

fn init_max_with_cap<T>(queue: *PriorityQueue<T>, cap: usize)
  where T: @derefable
{
  vector::init_with_cap::<T>(queue.items, cap);
  queue.is_min.* = false;
}

fn deinit<T>(queue: *PriorityQueue<T>) {
  vector::deinit::<T>(queue.items);
  queue.is_min.* = true;
}

fn len<T>(queue: *PriorityQueue<T>): usize {
  return vector::len::<T>(queue.items);
}

fn is_empty<T>(queue: *PriorityQueue<T>): bool {
  return vector::is_empty::<T>(queue.items);
}

fn push<T>(queue: *PriorityQueue<T>, value: T)
  where T: @ordered<T>, T: @derefable
{
  vector::push::<T>(queue.items, value);
  sift_up::<T>(queue, vector::len::<T>(queue.items) - 1);
}

fn peek<T>(queue: *PriorityQueue<T>): T
  where T: @derefable
{
  return queue.items.arr.*[0].*;
}

fn try_peek<T>(queue: *PriorityQueue<T>, out: *T): bool
  where T: @derefable
{
  if vector::len::<T>(queue.items) == 0 {
    return false;
  }
  out.* = queue.items.arr.*[0].*;
  return true;
}

fn pop<T>(queue: *PriorityQueue<T>): T
  where T: @ordered<T>, T: @derefable
{
  let result = queue.items.arr.*[0].*;
  let last = vector::remove::<T>(queue.items, vector::len::<T>(queue.items) - 1);
  if vector::len::<T>(queue.items) != 0 {
    queue.items.arr.*[0].* = last;
    sift_down::<T>(queue, 0);
  }
  return result;
}

fn try_pop<T>(queue: *PriorityQueue<T>, out: *T): bool
  where T: @ordered<T>, T: @derefable
{
  if vector::len::<T>(queue.items) == 0 {
    return false;
  }
  out.* = pop::<T>(queue);
  return true;
}

fn higher_priority<T>(queue: *PriorityQueue<T>, a: T, b: T): bool
  where T: @ordered<T>
{
  if queue.is_min.* {
    return a < b;
  }
  return b < a;
}

fn sift_up<T>(queue: *PriorityQueue<T>, idx: usize)
  where T: @ordered<T>, T: @derefable
{
  let i = idx;
  while i > 0 {
    let parent = (i - 1) / 2;
    if higher_priority::<T>(queue, queue.items.arr.*[i].*, queue.items.arr.*[parent].*) {
      vector::swap::<T>(queue.items, i, parent);
      i = parent;
    } else {
      return;
    }
  }
}

fn sift_down<T>(queue: *PriorityQueue<T>, idx: usize)
  where T: @ordered<T>, T: @derefable
{
  let i = idx;
  let n = vector::len::<T>(queue.items);
  while true {
    let best = i;
    let left = i * 2 + 1;
    let right = left + 1;

    if left < n && higher_priority::<T>(queue, queue.items.arr.*[left].*, queue.items.arr.*[best].*) {
      best = left;
    }
    if right < n && higher_priority::<T>(queue, queue.items.arr.*[right].*, queue.items.arr.*[best].*) {
      best = right;
    }

    if best == i {
      return;
    }

    vector::swap::<T>(queue.items, i, best);
    i = best;
  }
}
