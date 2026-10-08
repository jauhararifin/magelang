struct Box[T] { value: T }
let bytes: [*]u8 = "ab";

fn id[T](value: T): T {
  return value;
}
