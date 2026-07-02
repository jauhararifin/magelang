struct WithOpaque {
  value: opaque,
}

fn load<T>(p: *T): T {
  return p.*;
}

@main()
fn main() {
  let opaque_ptr = 0 as *opaque;
  let a = opaque_ptr.*;

  let box_ptr = 0 as *WithOpaque;
  let b = box_ptr.value;

  let opaque_array = 0 as [*]opaque;
  let c = opaque_array[0];

  let d = load::<opaque>(opaque_ptr);
}
