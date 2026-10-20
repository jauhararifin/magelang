struct Box[T] { value: T }

fn id[T](value: T): T { return value; }
fn plain(value: i32): i32 { return value; }

@main()
fn main() {
  var values = 0 as [*]i32;
  var a = values[];
  var b = values[0, 1];
  var c = values[true];
  var d = values[i32];
  var e = id[];
  var f = id[i32, u8];
  var g = id[0];
  var h = id[1 + 2];
  var i = id[values[0]];
  var j = plain[i32];
  var k = plain[];
  var l = (1)[0];
  var m: i32[];
  var n: Box[];
  var o: Box[0];
  var p: 1 + 2;
  var q = *i32;
  var r = [*]i32;
  var s = fn(i32): i32;
  var t = i32;
  var u = Box[i32];
  var v = 1{};
}
