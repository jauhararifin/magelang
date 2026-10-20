struct Foo { value: i32 }
struct Empty {}

fn takes(value: (i32, bool)) {}
fn wrong_return(): (i32, bool) { return (1, 2); }
fn wrong_empty(): () { return; }
fn missing_return(): () {}

fn bad() {
  var tuple: (i32, bool) = (1, true);
  var index = tuple.2;
  var huge = tuple.999999999999999999999999999999999;
  var field = tuple.missing;
  var short: (i32, bool) = (1,);
  var long: (i32,) = (1, false);
  var wrong_element: (i32, bool) = (false, true);
  var wrong_nested: (i32, (bool,)) = (1, (2,));
  var not_tuple: (i32,) = 1;
  var not_scalar: i32 = (1,);
  var not_struct: Empty = ();
  var not_empty: () = Empty{};
  var not_type: (i32, true);
  var not_value = (i32, false);
  var numeric_struct = Foo{}.0;
  var numeric_scalar = false.0;
  var empty_index = ().0;
  var bracket = tuple[0];
  var pointer: *(i32, bool);
  pointer.0 = 1;
  pointer.0.* = false;
  var pointer_index = pointer.2;
  tuple.0 = false;
  tuple = (1,);
  (1, true).0 = 2;
  takes((1, 2));
  var equal = tuple == (1,);
  var cast = tuple as i32;
}

fn bad_tuple_selections() {
  var invalid_type: (i32, bool).0;
  var struct_ptr: *Foo;
  var struct_index = struct_ptr.0;
  var slice: *[i32];
  var slice_index = slice.0;
  var unknown_index = missing.0;
  var overflowing_index = (1,).18446744073709551616;
}

fn bad_single_tuple_types() {
  var scalar_value: (i32) = (1);
  var nested_value: ((i32)) = (1,);
  var empty_value: (()) = ();
}

fn wrong_single_return(): (i32) { return (1); }

fn bad_tuple_comparisons() {
  var wrong_type = (1,) == (true,);
  var not_struct = (1,) == Foo{value: 1};
  var not_empty_struct = () == Empty{};
  var unordered = (1, 2) < (1, 3);
}
