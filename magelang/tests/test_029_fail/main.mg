import dep "tests/test_028/dep";

struct TypeFirst {}
fn TypeFirst() {}

fn FunctionFirst() {}
struct FunctionFirst {}

struct TypeBeforeGlobal {}
var TypeBeforeGlobal: i32;

var GlobalFirst: i32;
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
var ImportBeforeGlobal: i32;

var GlobalBeforeImport: i32;
import GlobalBeforeImport "tests/test_028/dep";

fn repeated_parameter[T](T: T) {}

struct Box[T] { value: T }
fn id[T](value: T): T { return value; }

fn shadow_type() {
  var Box = "ab";
  var bad: Box[i32];
}

fn shadow_type_argument[T](value: T) {
  var T = 1;
  var bad: T;
  var other = id[T];
}

fn shadow_import() {
  var dep = 0;
  var bad: dep.Box[i32];
  var other = dep.id[i32](1);
}

fn type_param_shadows_import[dep]() {
  var bad = dep.id[i32](1);
  var other: dep.Box[i32];
}

fn type_param_shadows_function[id]() {
  var bad = id[i32](1);
}

fn parameter_shadows_type(Box: i32) {
  var bad: Box[i32];
}

fn builtin_shadowing() {
  var i32 = 0;
  var bad: i32;
  var other = id[i32];
}

fn wrong_category() {
  var a = dep;
  var b: dep;
  var c = dep.Box[i32];
  var d: dep.id;
}

@main()
fn main() {}
