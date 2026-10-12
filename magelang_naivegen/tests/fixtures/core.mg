@intrinsic("unreachable")
fn trap();

fn assert[T](expected: T, actual: T) {
  if expected != actual { trap(); }
}

struct Pair[T] { a: T, b: T }
struct Nested { tag: u8, pair: Pair[i64], empty: void }

let events: i64;
let initialized: i64 = initialize();
let base: i64 = 21;
let global_pair: Pair[i64] = pair(base);

fn initialize(): i64 { return base * 2; }
fn pair(n: i64): Pair[i64] { return Pair[i64]{a: n, b: n + 1}; }
fn id[T](value: T): T { return value; }
fn fib(n: i64): i64 {
  if n < 2 { return n; }
  return fib(n - 1) + fib(n - 2);
}
fn sum(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, g: i64, h: i64, i: i64, j: i64): i64 {
  a += 10;
  return a + b + c + d + e + f + g + h + i + j;
}
fn add_event(n: i64) { events = events * 10 + n; }
fn argument(): i64 { add_event(2); return 42; }
fn callee(): fn(i64): i64 { add_event(1); return id[i64]; }
fn side(): bool { add_event(3); return true; }
fn deferred_return(n: i64): Pair[i64] {
  defer add_event(1);
  defer { let value: i64 = 2; add_event(value); }
  if n == 0 { return pair(10); }
  return pair(n);
}
fn conditional_defer(flag: bool) {
  defer add_event(1);
  if flag { defer add_event(2); return; }
  { defer add_event(3); }
}

@main()
fn main() {
  assert[i64](42, initialized);
  assert[i64](22, global_pair.b);
  assert[i64](55, fib(10));
  let f: fn(i64): i64 = id[i64];
  assert[i64](42, f(42));
  assert[i64](65, sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10));
  assert[i64](42, callee()(argument()));
  assert[i64](12, events);
  let p = pair(5);
  p.a += pair(10).b;
  assert[i64](16, p.a);
  assert[i64](6, p.b);
  global_pair.a = 7;
  assert[Pair[i64]](Pair[i64]{a: 7, b: 22}, global_pair);
  let nested = Nested{tag: 9, pair: p};
  nested.pair.b *= 3;
  assert[i64](18, id[Nested](nested).pair.b);
  assert[u8](9, id[Nested](nested).tag);
  let empty: void;
  assert[void](empty, id[void](empty));

  events = 0;
  assert[bool](false, false && side());
  assert[bool](true, true || side());
  assert[i64](0, events);
  let flag: bool = false;
  flag &&= side();
  flag ||= side();
  assert[bool](true, flag);
  assert[i64](3, events);

  events = 0;
  assert[Pair[i64]](Pair[i64]{a: 10, b: 11}, deferred_return(0));
  assert[i64](21, events);
  events = 0;
  assert[i64](9, deferred_return(9).a);
  assert[i64](21, events);
  events = 0;
  conditional_defer(true);
  assert[i64](21, events);
  events = 0;
  conditional_defer(false);
  assert[i64](31, events);

  events = 0;
  for let i: i64 = 0; i < 5; i += 1 {
    defer add_event(i);
    if i == 1 { continue; }
    if i == 3 { break; }
  }
  assert[i64](123, events);
  let total: i64;
  let i: i64;
  while i < 10000 {
    i += 1;
    if i % 2 == 0 { continue; }
    total += id[i64](i);
  }
  assert[i64](25000000, total);
  let count: i64;
  for let outer = 0; outer < 3; outer += 1 {
    for let inner = 0; inner < 5; inner += 1 {
      if inner == 2 { break; }
      count += 1;
    }
  }
  assert[i64](6, count);
}
