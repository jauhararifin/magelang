import dep "tests/test_028/dep";

struct TypeFirst {}
fn TypeFirst() {}

fn FunctionFirst() {}
struct FunctionFirst {}

struct TypeBeforeGlobal {}
let TypeBeforeGlobal: i32;

let GlobalFirst: i32;
struct GlobalFirst {}

import ImportFirst "tests/test_028/dep";
struct ImportFirst {}

struct TypeBeforeImport {}
import TypeBeforeImport "tests/test_028/dep";

import ImportBeforeFunction "tests/test_028/dep";
fn ImportBeforeFunction() {}

fn FunctionBeforeImport() {}
import FunctionBeforeImport "tests/test_028/dep";

import ImportBeforeGlobal "tests/test_028/dep";
let ImportBeforeGlobal: i32;

let GlobalBeforeImport: i32;
import GlobalBeforeImport "tests/test_028/dep";

fn repeated_parameter[T](T: T) {}

struct Box[T] { value: T }
fn id[T](value: T): T { return value; }

fn shadow_type() {
  let Box = "ab";
  let bad: Box[i32];
}

fn shadow_type_argument[T](value: T) {
  let T = 1;
  let bad: T;
  let other = id[T];
}

fn shadow_import() {
  let dep = 0;
  let bad: dep.Box[i32];
  let other = dep.id[i32](1);
}

fn type_param_shadows_import[dep]() {
  let bad = dep.id[i32](1);
  let other: dep.Box[i32];
}

fn type_param_shadows_function[id]() {
  let bad = id[i32](1);
}

fn parameter_shadows_type(Box: i32) {
  let bad: Box[i32];
}

fn builtin_shadowing() {
  let i32 = 0;
  let bad: i32;
  let other = id[i32];
}

fn wrong_category() {
  let a = dep;
  let b: dep;
  let c = dep.Box[i32];
  let d: dep.id;
}

@main()
fn main() {}
