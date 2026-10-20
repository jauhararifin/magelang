import dep "tests/test_visibility_fail/dep";

var public_global: i32 = dep.public_global;
var private_global: i32 = dep.private_global;
var private_function: i32 = dep.private_function();
var private_generic: i32 = dep.private_generic[i32](1);
var private_type: dep.Private;
var public_alias: dep.PublicAlias;
var private_alias: dep.PrivateAlias;
var private_literal: dep.Public = dep.Public{hidden: 1};

fn inspect(): i32 {
  var value = dep.public_function();
  return value.visible + value.hidden;
}

var private_builtin: dep.i32;

@main()
fn main() {}
