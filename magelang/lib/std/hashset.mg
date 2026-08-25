import hashmap "std/hashmap";

struct HashSet<K> {
  map: hashmap::HashMap<K, bool>,
}

fn init<K>(set: *HashSet<K>) {
  hashmap::init::<K, bool>(set.map);
}

fn init_with_cap<K>(set: *HashSet<K>, cap: usize)
  where K: @derefable
{
  hashmap::init_with_cap::<K, bool>(set.map, cap);
}

fn deinit<K>(set: *HashSet<K>) {
  hashmap::deinit::<K, bool>(set.map);
}

fn len<K>(set: *HashSet<K>): usize {
  return hashmap::len::<K, bool>(set.map);
}

fn is_empty<K>(set: *HashSet<K>): bool {
  return hashmap::is_empty::<K, bool>(set.map);
}

fn clear<K>(set: *HashSet<K>) {
  hashmap::clear::<K, bool>(set.map);
}

fn insert<K>(set: *HashSet<K>, key: K): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable
{
  return hashmap::set::<K, bool>(set.map, key, true);
}

fn contains<K>(set: *HashSet<K>, key: K): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable
{
  return hashmap::contains::<K, bool>(set.map, key);
}

fn remove<K>(set: *HashSet<K>, key: K): bool
  where K: @castable<usize>, K: @comparable<K>, K: @derefable
{
  return hashmap::delete::<K, bool>(set.map, key);
}
