import dep "tests/test_028/dep";
import wasm "std/wasm";

struct Box[T] { value: T }
var initial: i32 = later();
var later_value: i32 = 17;

fn later(): i32 { return later_value; }
fn id[T](value: T): T { return value; }
fn import_as_type_param[dep](value: dep): dep { return value; }
fn function_as_type_param[id](value: id): id { return value; }
fn builtin_as_type_param[i32](value: i32): i32 { return value; }
fn type_as_parameter(Box: string): u8 { return Box[0].*; }
fn builtin_as_parameter(i32: string): u8 { return i32[1].*; }

@main()
fn main() {
  if initial != 17 { wasm.unreachable(); }
  if import_as_type_param[i32](3) != 3 { wasm.unreachable(); }
  if function_as_type_param[i32](4) != 4 { wasm.unreachable(); }
  if builtin_as_type_param[i64](5) != 5 { wasm.unreachable(); }
  if type_as_parameter("ab") != 97 { wasm.unreachable(); }
  if builtin_as_parameter("ab") != 98 { wasm.unreachable(); }

  var boxed: dep.Box[i32] = dep.Box[i32]{value: dep.id[i32](42)};
  if boxed.value != 42 { wasm.unreachable(); }
  if dep.bytes[0].* != 97 { wasm.unreachable(); }
  if ((dep)).id[i32](6) != 6 { wasm.unreachable(); }

  {
    var Box = "ab";
    if Box[0].* != 97 { wasm.unreachable(); }
    var id = Box;
    if id[1].* != 98 { wasm.unreachable(); }
    var i32: i32 = 1;
    if id[i32].* != 98 { wasm.unreachable(); }
  }
  var original: Box[i32] = Box[i32]{value: id[i32](7)};
  if original.value != 7 { wasm.unreachable(); }

  {
    var dep = dep.Box[string]{value: dep.bytes};
    if dep.value[1].* != 98 { wasm.unreachable(); }
  }
  if dep.id[i32](8) != 8 { wasm.unreachable(); }

  var x = 1;
  var x = x + 1;
  if x != 2 { wasm.unreachable(); }
}
