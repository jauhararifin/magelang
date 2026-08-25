import wasm "std/wasm";
import mem "std/mem";
import vector "std/vector";
import alg "std/alg";
import sort "std/sort";
import hashmap "std/hashmap";
import hashset "std/hashset";
import pq "std/priority_queue";
import slab "std/slab";

@main()
fn main() {
  test_algorithms();
  test_sort();
  test_vector();
  test_hashmap();
  test_hashset();
  test_priority_queue();
  test_slab_allocator();
}

fn test_algorithms() {
  assert_equal::<i32>(7, alg::max::<i32>(7, -2));
  assert_equal::<i32>(-2, alg::min::<i32>(7, -2));
  assert_equal::<i32>(10, alg::clamp::<i32>(20, 0, 10));
  assert_equal::<i32>(0, alg::clamp::<i32>(-5, 0, 10));
  assert_equal::<i32>(5, alg::clamp::<i32>(5, 0, 10));

  let values = mem::alloc_array::<i32>(6);
  values[0].* = 1;
  values[1].* = 2;
  values[2].* = 3;
  values[3].* = 4;
  values[4].* = 5;
  values[5].* = 6;
  assert_equal::<i32>(21, alg::sum::<i32>(values, 6));
  assert_equal::<i32>(720, alg::product::<i32>(values, 6));
  assert(alg::contains::<i32>(values, 6, 4));
  assert(!alg::contains::<i32>(values, 6, 9));
  assert_equal::<usize>(3, alg::index_of::<i32>(values, 6, 4));
  assert_equal::<usize>(6, alg::index_of::<i32>(values, 6, 9));

  alg::reverse::<i32>(values, 6);
  assert_equal::<i32>(6, values[0].*);
  assert_equal::<i32>(1, values[5].*);

  alg::fill::<i32>(values, 6, 11);
  let i: usize = 0;
  while i < 6 {
    assert_equal::<i32>(11, values[i].*);
    i = i + 1;
  }

  let copy = mem::alloc_array::<i32>(6);
  alg::copy::<i32>(copy, values, 6);
  i = 0;
  while i < 6 {
    assert_equal::<i32>(11, copy[i].*);
    i = i + 1;
  }
}

fn test_sort() {
  let empty = 0 as [*]i32;
  sort::sort::<i32>(empty, 0);
  assert(sort::is_sorted::<i32>(empty, 0));

  let single = mem::alloc_array::<i32>(1);
  single[0].* = 42;
  sort::sort::<i32>(single, 1);
  assert(sort::is_sorted::<i32>(single, 1));
  assert_equal::<i32>(42, single[0].*);

  let values = mem::alloc_array::<i32>(12);
  values[0].* = 9;
  values[1].* = -3;
  values[2].* = 9;
  values[3].* = 0;
  values[4].* = 7;
  values[5].* = 7;
  values[6].* = -10;
  values[7].* = 5;
  values[8].* = 1;
  values[9].* = 4;
  values[10].* = 3;
  values[11].* = 2;

  assert(!sort::is_sorted::<i32>(values, 12));
  sort::sort::<i32>(values, 12);
  assert(sort::is_sorted::<i32>(values, 12));
  assert_equal::<i32>(-10, values[0].*);
  assert_equal::<i32>(-3, values[1].*);
  assert_equal::<i32>(0, values[2].*);
  assert_equal::<i32>(1, values[3].*);
  assert_equal::<i32>(2, values[4].*);
  assert_equal::<i32>(3, values[5].*);
  assert_equal::<i32>(4, values[6].*);
  assert_equal::<i32>(5, values[7].*);
  assert_equal::<i32>(7, values[8].*);
  assert_equal::<i32>(7, values[9].*);
  assert_equal::<i32>(9, values[10].*);
  assert_equal::<i32>(9, values[11].*);

  let floats = mem::alloc_array::<f64>(5);
  floats[0].* = 3.5;
  floats[1].* = -1.0;
  floats[2].* = 3.5;
  floats[3].* = 2.25;
  floats[4].* = 0.0;
  sort::sort::<f64>(floats, 5);
  assert(sort::is_sorted::<f64>(floats, 5));
  assert_equal::<f64>(-1.0, floats[0].*);
  assert_equal::<f64>(0.0, floats[1].*);
  assert_equal::<f64>(2.25, floats[2].*);
  assert_equal::<f64>(3.5, floats[3].*);
  assert_equal::<f64>(3.5, floats[4].*);
}

fn test_vector() {
  let v = mem::alloc::<vector::Vector<i32>>();
  vector::init::<i32>(v);
  assert(vector::is_empty::<i32>(v));
  assert_equal::<usize>(0, vector::len::<i32>(v));
  assert_equal::<usize>(0, vector::cap::<i32>(v));

  let i: i32 = 0;
  while i < 100 {
    vector::push::<i32>(v, i * 2);
    i = i + 1;
  }
  assert_equal::<usize>(100, vector::len::<i32>(v));
  assert(vector::cap::<i32>(v) >= 100);

  i = 0;
  while i < 100 {
    assert_equal::<i32>(i * 2, vector::get::<i32>(v, i as usize));
    i = i + 1;
  }

  vector::set::<i32>(v, 10, 999);
  assert_equal::<i32>(999, vector::get::<i32>(v, 10));

  vector::insert::<i32>(v, 0, -1);
  assert_equal::<i32>(-1, vector::get::<i32>(v, 0));
  vector::insert::<i32>(v, 50, -2);
  assert_equal::<i32>(-2, vector::get::<i32>(v, 50));
  vector::insert::<i32>(v, 10000, -3);
  assert_equal::<i32>(-3, vector::get::<i32>(v, vector::len::<i32>(v) - 1));

  assert_equal::<i32>(-1, vector::remove::<i32>(v, 0));
  assert_equal::<i32>(-2, vector::remove::<i32>(v, 49));

  let out = mem::alloc::<i32>();
  assert(vector::back::<i32>(v, out));
  assert_equal::<i32>(-3, out.*);
  assert(vector::pop::<i32>(v, out));
  assert_equal::<i32>(-3, out.*);
  assert_equal::<usize>(100, vector::len::<i32>(v));

  vector::clear::<i32>(v);
  assert(vector::is_empty::<i32>(v));
  assert(!vector::pop::<i32>(v, out));

  vector::resize::<i32>(v, 5, 7);
  assert_equal::<usize>(5, vector::len::<i32>(v));
  i = 0;
  while i < 5 {
    assert_equal::<i32>(7, vector::get::<i32>(v, i as usize));
    i = i + 1;
  }
  vector::resize::<i32>(v, 2, 0);
  assert_equal::<usize>(2, vector::len::<i32>(v));
}

fn test_hashmap() {
  let map = mem::alloc::<hashmap::HashMap<i32, i64>>();
  hashmap::init::<i32, i64>(map);
  assert(hashmap::is_empty::<i32, i64>(map));

  let out = mem::alloc::<i64>();
  assert(!hashmap::get::<i32, i64>(map, 123, out));
  assert(!hashmap::delete::<i32, i64>(map, 123));

  let i: i32 = 0;
  while i < 80 {
    assert(hashmap::set::<i32, i64>(map, i, (i as i64) * 10));
    i = i + 1;
  }
  assert_equal::<usize>(80, hashmap::len::<i32, i64>(map));

  i = 0;
  while i < 80 {
    assert(hashmap::contains::<i32, i64>(map, i));
    assert(hashmap::get::<i32, i64>(map, i, out));
    assert_equal::<i64>((i as i64) * 10, out.*);
    i = i + 1;
  }

  assert(!hashmap::set::<i32, i64>(map, 10, 777));
  assert(hashmap::get::<i32, i64>(map, 10, out));
  assert_equal::<i64>(777, out.*);
  assert_equal::<usize>(80, hashmap::len::<i32, i64>(map));

  i = 0;
  while i < 80 {
    if i % 2 == 0 {
      assert(hashmap::remove::<i32, i64>(map, i, out));
      if i == 10 {
        assert_equal::<i64>(777, out.*);
      } else {
        assert_equal::<i64>((i as i64) * 10, out.*);
      }
    }
    i = i + 1;
  }
  assert_equal::<usize>(40, hashmap::len::<i32, i64>(map));

  i = 0;
  while i < 80 {
    if i % 2 == 0 {
      assert(!hashmap::contains::<i32, i64>(map, i));
    } else {
      assert(hashmap::contains::<i32, i64>(map, i));
    }
    i = i + 1;
  }

  i = 100;
  while i < 160 {
    assert(hashmap::set::<i32, i64>(map, i, i as i64));
    i = i + 1;
  }
  assert_equal::<usize>(100, hashmap::len::<i32, i64>(map));

  hashmap::clear::<i32, i64>(map);
  assert(hashmap::is_empty::<i32, i64>(map));
  assert(!hashmap::get::<i32, i64>(map, 101, out));
}

fn test_hashset() {
  let set = mem::alloc::<hashset::HashSet<i32>>();
  hashset::init::<i32>(set);
  assert(hashset::is_empty::<i32>(set));

  let i: i32 = 0;
  while i < 120 {
    assert(hashset::insert::<i32>(set, i));
    assert(!hashset::insert::<i32>(set, i));
    i = i + 1;
  }
  assert_equal::<usize>(120, hashset::len::<i32>(set));

  i = 0;
  while i < 120 {
    assert(hashset::contains::<i32>(set, i));
    i = i + 1;
  }
  assert(!hashset::contains::<i32>(set, 999));

  i = 0;
  while i < 120 {
    if i % 3 == 0 {
      assert(hashset::remove::<i32>(set, i));
      assert(!hashset::remove::<i32>(set, i));
    }
    i = i + 1;
  }
  assert_equal::<usize>(80, hashset::len::<i32>(set));

  i = 0;
  while i < 120 {
    if i % 3 == 0 {
      assert(!hashset::contains::<i32>(set, i));
    } else {
      assert(hashset::contains::<i32>(set, i));
    }
    i = i + 1;
  }

  hashset::clear::<i32>(set);
  assert(hashset::is_empty::<i32>(set));
}

fn test_priority_queue() {
  let minq = mem::alloc::<pq::PriorityQueue<i32>>();
  pq::init_min::<i32>(minq);
  let out = mem::alloc::<i32>();
  assert(!pq::try_pop::<i32>(minq, out));
  assert(!pq::try_peek::<i32>(minq, out));

  pq::push::<i32>(minq, 5);
  pq::push::<i32>(minq, -1);
  pq::push::<i32>(minq, 7);
  pq::push::<i32>(minq, 7);
  pq::push::<i32>(minq, 0);
  pq::push::<i32>(minq, 3);
  assert_equal::<usize>(6, pq::len::<i32>(minq));
  assert(pq::try_peek::<i32>(minq, out));
  assert_equal::<i32>(-1, out.*);
  assert_equal::<i32>(-1, pq::pop::<i32>(minq));
  assert_equal::<i32>(0, pq::pop::<i32>(minq));
  assert_equal::<i32>(3, pq::pop::<i32>(minq));
  assert_equal::<i32>(5, pq::pop::<i32>(minq));
  assert_equal::<i32>(7, pq::pop::<i32>(minq));
  assert_equal::<i32>(7, pq::pop::<i32>(minq));
  assert(pq::is_empty::<i32>(minq));

  let maxq = mem::alloc::<pq::PriorityQueue<i32>>();
  pq::init_max::<i32>(maxq);
  let i: i32 = 0;
  while i < 50 {
    pq::push::<i32>(maxq, (i * 37) % 101);
    i = i + 1;
  }

  let prev = pq::pop::<i32>(maxq);
  while !pq::is_empty::<i32>(maxq) {
    let curr = pq::pop::<i32>(maxq);
    assert(curr <= prev);
    prev = curr;
  }
}

fn test_slab_allocator() {
  let allocator = mem::alloc::<slab::SlabAllocator>();
  slab::init(allocator);

  let a = slab::alloc::<i32>(allocator);
  a.* = 12345;
  assert_equal::<i32>(12345, a.*);
  let addr = a as usize;
  slab::dealloc::<i32>(allocator, a);
  let b = slab::alloc::<i32>(allocator);
  assert_equal::<usize>(addr, b as usize);
  b.* = -99;
  assert_equal::<i32>(-99, b.*);

  let bytes = slab::alloc_array::<u8>(allocator, 9);
  let byte_addr = bytes as usize;
  bytes[0].* = 1;
  bytes[8].* = 9;
  assert_equal::<u8>(1, bytes[0].*);
  assert_equal::<u8>(9, bytes[8].*);
  slab::dealloc_array::<u8>(allocator, bytes, 9);
  let same_class = slab::alloc_size(allocator, 16);
  assert_equal::<usize>(byte_addr, same_class);
  slab::dealloc_size(allocator, same_class, 16);

  let large = slab::alloc_size(allocator, 1000);
  assert(large != 0);
  slab::dealloc_size(allocator, large, 1000);
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
