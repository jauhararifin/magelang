struct Box[T] { value: T }

fn id[T](value: T): T { return value; }
fn plain(value: i32): i32 { return value; }

@main()
fn main() {
  let values = 0 as [*]i32;
  let a = values[];
  let b = values[0, 1];
  let c = values[true];
  let d = values[i32];
  let e = id[];
  let f = id[i32, u8];
  let g = id[0];
  let h = id[1 + 2];
  let i = id[values[0]];
  let j = plain[i32];
  let k = plain[];
  let l = (1)[0];
  let m: i32[];
  let n: Box[];
  let o: Box[0];
  let p: 1 + 2;
  let q = *i32;
  let r = [*]i32;
  let s = fn(i32): i32;
  let t = i32;
  let u = Box[i32];
  let v = 1{};
}
