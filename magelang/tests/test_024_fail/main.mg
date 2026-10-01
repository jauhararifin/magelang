fn missing_before_close() {
  defer
}

@main()
fn main() {
  defer ;
  let recovered_after_semicolon = 1;
  defer ,;
  let recovered_after_comma = 1;
  defer );
  let recovered_after_close_brac = 1;
  defer ];
  let recovered_after_close_square = 1;
  defer else;
  let recovered_after_else = 1;
  defer defer ,;
  let recovered_after_nested_defer = 1;
  defer if ;
  let recovered_after_invalid_if = 1;
}
