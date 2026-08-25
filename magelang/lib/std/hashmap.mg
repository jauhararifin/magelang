import mem "std/mem";
import hash "std/hash";

struct HashMap<K, V> {
  states: [*]u8, // 0 = empty, 1 = full, 2 = deleted
  hashes: [*]usize,
  keys: [*]K,
  values: [*]V,
  cap: usize,
  len: usize,
  deleted: usize,
}

fn init<K, V>(map: *HashMap<K, V>) {
  map.* = HashMap::<K, V>{
    states: 0 as [*]u8,
    hashes: 0 as [*]usize,
    keys: 0 as [*]K,
    values: 0 as [*]V,
    cap: 0,
    len: 0,
    deleted: 0,
  };
}

fn init_with_cap<K, V>(map: *HashMap<K, V>, cap: usize)
  where K: @derefable, V: @derefable
{
  init::<K, V>(map);
  reserve::<K, V>(map, cap);
}

fn deinit<K, V>(map: *HashMap<K, V>) {
  if map.cap.* != 0 {
    mem::dealloc_array::<u8>(map.states.*);
    mem::dealloc_array::<usize>(map.hashes.*);
    mem::dealloc_array::<K>(map.keys.*);
    mem::dealloc_array::<V>(map.values.*);
  }
  init::<K, V>(map);
}

fn len<K, V>(map: *HashMap<K, V>): usize {
  return map.len.*;
}

fn cap<K, V>(map: *HashMap<K, V>): usize {
  return map.cap.*;
}

fn is_empty<K, V>(map: *HashMap<K, V>): bool {
  return map.len.* == 0;
}

fn clear<K, V>(map: *HashMap<K, V>) {
  let i: usize = 0;
  while i < map.cap.* {
    map.states.*[i].* = 0;
    i = i + 1;
  }
  map.len.* = 0;
  map.deleted.* = 0;
}

fn reserve<K, V>(map: *HashMap<K, V>, requested: usize)
  where K: @derefable, V: @derefable
{
  let new_cap: usize = 16;
  while new_cap < requested * 2 {
    new_cap = new_cap * 2;
  }

  if new_cap <= map.cap.* {
    return;
  }

  rehash::<K, V>(map, new_cap);
}

fn set<K, V>(map: *HashMap<K, V>, key: K, value: V): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable, V: @derefable
{
  if map.cap.* == 0 || (map.len.* + map.deleted.* + 1) * 4 >= map.cap.* * 3 {
    let new_cap: usize = 16;
    if map.cap.* != 0 {
      new_cap = map.cap.* * 2;
    }
    rehash::<K, V>(map, new_cap);
  }

  let h = hash_key::<K>(key);
  let idx = h % map.cap.*;
  let first_deleted = map.cap.*;

  while true {
    let state = map.states.*[idx].*;
    if state == 0 {
      let target = idx;
      if first_deleted != map.cap.* {
        target = first_deleted;
        map.deleted.* = map.deleted.* - 1;
      }

      map.states.*[target].* = 1;
      map.hashes.*[target].* = h;
      map.keys.*[target].* = key;
      map.values.*[target].* = value;
      map.len.* = map.len.* + 1;
      return true;
    }

    if state == 2 {
      if first_deleted == map.cap.* {
        first_deleted = idx;
      }
    } else if map.hashes.*[idx].* == h && map.keys.*[idx].* == key {
      map.values.*[idx].* = value;
      return false;
    }

    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  return false;
}

fn get<K, V>(map: *HashMap<K, V>, key: K, out: *V): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable, V: @derefable
{
  if map.cap.* == 0 {
    return false;
  }

  let h = hash_key::<K>(key);
  let idx = h % map.cap.*;

  while true {
    let state = map.states.*[idx].*;
    if state == 0 {
      return false;
    }

    if state == 1 && map.hashes.*[idx].* == h && map.keys.*[idx].* == key {
      out.* = map.values.*[idx].*;
      return true;
    }

    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  return false;
}

fn contains<K, V>(map: *HashMap<K, V>, key: K): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable, V: @derefable
{
  if map.cap.* == 0 {
    return false;
  }

  let h = hash_key::<K>(key);
  let idx = h % map.cap.*;

  while true {
    let state = map.states.*[idx].*;
    if state == 0 {
      return false;
    }

    if state == 1 && map.hashes.*[idx].* == h && map.keys.*[idx].* == key {
      return true;
    }

    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  return false;
}

fn remove<K, V>(map: *HashMap<K, V>, key: K, out: *V): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable, V: @derefable
{
  if map.cap.* == 0 {
    return false;
  }

  let h = hash_key::<K>(key);
  let idx = h % map.cap.*;

  while true {
    let state = map.states.*[idx].*;
    if state == 0 {
      return false;
    }

    if state == 1 && map.hashes.*[idx].* == h && map.keys.*[idx].* == key {
      out.* = map.values.*[idx].*;
      map.states.*[idx].* = 2;
      map.len.* = map.len.* - 1;
      map.deleted.* = map.deleted.* + 1;
      return true;
    }

    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  return false;
}

fn delete<K, V>(map: *HashMap<K, V>, key: K): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable, V: @derefable
{
  if map.cap.* == 0 {
    return false;
  }

  let h = hash_key::<K>(key);
  let idx = h % map.cap.*;

  while true {
    let state = map.states.*[idx].*;
    if state == 0 {
      return false;
    }

    if state == 1 && map.hashes.*[idx].* == h && map.keys.*[idx].* == key {
      map.states.*[idx].* = 2;
      map.len.* = map.len.* - 1;
      map.deleted.* = map.deleted.* + 1;
      return true;
    }

    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  return false;
}

fn hash_key<K>(key: K): usize
  where K: @castable<usize>
{
  return hash::mix(key as usize);
}

fn rehash<K, V>(map: *HashMap<K, V>, new_cap: usize)
  where K: @derefable, V: @derefable
{
  let old_states = map.states.*;
  let old_hashes = map.hashes.*;
  let old_keys = map.keys.*;
  let old_values = map.values.*;
  let old_cap = map.cap.*;

  let states = mem::alloc_array::<u8>(new_cap);
  let hashes = mem::alloc_array::<usize>(new_cap);
  let keys = mem::alloc_array::<K>(new_cap);
  let values = mem::alloc_array::<V>(new_cap);

  let i: usize = 0;
  while i < new_cap {
    states[i].* = 0;
    i = i + 1;
  }

  map.states.* = states;
  map.hashes.* = hashes;
  map.keys.* = keys;
  map.values.* = values;
  map.cap.* = new_cap;
  map.len.* = 0;
  map.deleted.* = 0;

  i = 0;
  while i < old_cap {
    if old_states[i].* == 1 {
      insert_existing::<K, V>(map, old_keys[i].*, old_values[i].*, old_hashes[i].*);
    }
    i = i + 1;
  }

  if old_cap != 0 {
    mem::dealloc_array::<u8>(old_states);
    mem::dealloc_array::<usize>(old_hashes);
    mem::dealloc_array::<K>(old_keys);
    mem::dealloc_array::<V>(old_values);
  }
}

fn insert_existing<K, V>(map: *HashMap<K, V>, key: K, value: V, h: usize)
  where K: @derefable, V: @derefable
{
  let idx = h % map.cap.*;
  while map.states.*[idx].* == 1 {
    idx = idx + 1;
    if idx == map.cap.* {
      idx = 0;
    }
  }

  map.states.*[idx].* = 1;
  map.hashes.*[idx].* = h;
  map.keys.*[idx].* = key;
  map.values.*[idx].* = value;
  map.len.* = map.len.* + 1;
}
