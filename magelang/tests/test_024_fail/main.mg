fn missing_before_close() {
  defer
}

@main()
fn main() {
  defer ;
  var recovered_after_semicolon = 1;
  defer ,;
  var recovered_after_comma = 1;
  defer );
  var recovered_after_close_brac = 1;
  defer ];
  var recovered_after_close_square = 1;
  defer else;
  var recovered_after_else = 1;
  defer defer ,;
  var recovered_after_nested_defer = 1;
  defer if ;
  var recovered_after_invalid_if = 1;
}
