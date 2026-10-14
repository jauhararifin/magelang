struct Foo { value: i32 }
struct Empty {}

fn takes(value: (i32, bool)) {}
fn wrong_return(): (i32, bool) { return (1, 2); }
fn wrong_empty(): () { return; }
fn missing_return(): () {}

fn bad() {
  let tuple: (i32, bool) = (1, true);
  let index = tuple.2;
  let huge = tuple.999999999999999999999999999999999;
  let field = tuple.missing;
  let short: (i32, bool) = (1,);
  let long: (i32,) = (1, false);
  let wrong_element: (i32, bool) = (false, true);
  let wrong_nested: (i32, (bool,)) = (1, (2,));
  let not_tuple: (i32,) = 1;
  let not_scalar: i32 = (1,);
  let not_struct: Empty = ();
  let not_empty: () = Empty{};
  let not_type: (i32, true);
  let not_value = (i32, false);
  let numeric_struct = Foo{}.0;
  let numeric_scalar = false.0;
  let empty_index = ().0;
  let bracket = tuple[0];
  let pointer: *(i32, bool);
  pointer.0 = 1;
  pointer.0.* = false;
  let pointer_index = pointer.2;
  tuple.0 = false;
  tuple = (1,);
  (1, true).0 = 2;
  takes((1, 2));
  let equal = tuple == (1,);
  let cast = tuple as i32;
}

fn bad_tuple_selections() {
  let invalid_type: (i32, bool).0;
  let struct_ptr: *Foo;
  let struct_index = struct_ptr.0;
  let slice: *[i32];
  let slice_index = slice.0;
  let unknown_index = missing.0;
  let overflowing_index = (1,).18446744073709551616;
}

fn bad_single_tuple_types() {
  let scalar_value: (i32) = (1);
  let nested_value: ((i32)) = (1,);
  let empty_value: (()) = ();
}

fn wrong_single_return(): (i32) { return (1); }

fn bad_tuple_comparisons() {
  let wrong_type = (1,) == (true,);
  let not_struct = (1,) == Foo{value: 1};
  let not_empty_struct = () == Empty{};
  let unordered = (1, 2) < (1, 3);
}
