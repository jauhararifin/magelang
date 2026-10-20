import dep "tests/test_030_fail/dep";

struct Holder { value: i32 }
fn use_local(value: i32) {}
fn use_import(value: dep.i32) {}
var earlier: i32;
var i32: u32;

@main()
fn main() {}
