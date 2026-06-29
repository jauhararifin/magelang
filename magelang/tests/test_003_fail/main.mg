let a: i32 = f();
let b: i32 = a;

fn f(): i32 {
  return b;
}
