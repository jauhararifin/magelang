import dep "tests/test_visibility_fail/dep";

let public_global: i32 = dep.public_global;
let private_global: i32 = dep.private_global;
let private_function: i32 = dep.private_function();
let private_generic: i32 = dep.private_generic[i32](1);
let private_type: dep.Private;
let public_alias: dep.PublicAlias;
let private_alias: dep.PrivateAlias;
let private_literal: dep.Public = dep.Public{hidden: 1};

fn inspect(): i32 {
  let value = dep.public_function();
  return value.visible + value.hidden;
}

let private_builtin: dep.i32;

@main()
fn main() {}
