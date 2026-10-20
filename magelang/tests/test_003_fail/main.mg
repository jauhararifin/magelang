var a: i32 = f();
var b: i32 = a;

fn f(): i32 {
  return b;
}
