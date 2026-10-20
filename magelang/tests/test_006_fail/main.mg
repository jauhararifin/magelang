@main()
fn main() {
  init_var_not_visible_after_loop();
  non_bool_cond();
  break_after_loop();
  body_local_not_visible_in_update();
}

fn init_var_not_visible_after_loop() {
  for var i: i32 = 0; i < 3; i = i + 1 {}
  i = 5;
}

fn non_bool_cond() {
  for var i: i32 = 0; i; i = i + 1 {}
}

fn break_after_loop() {
  for ;; { break; }
  break;
}

fn body_local_not_visible_in_update() {
  for var i: i32 = 0; i < 3; j = j + 1 {
    var j: i32 = 0;
  }
}
