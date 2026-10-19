import dep "tests/test_030_fail/dep";

type Holder struct { value: i32 }
fn use_local(value: i32) {}
fn use_import(value: dep.i32) {}
let earlier: i32;
let i32: u32;

@main()
fn main() {}
