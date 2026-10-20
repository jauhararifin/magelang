pub struct Public {
  pub visible: i32,
  hidden: i32,
}

struct Private {
  pub visible: i32,
}

pub type PublicAlias = Public;
type PrivateAlias = Public;

pub let public_global: i32 = private_global;
let private_global: i32 = 2;

pub fn public_function(): Public {
  return Public{visible: private_function(), hidden: private_generic[i32](2)};
}

fn private_function(): i32 {
  return private_global;
}

fn private_generic[T](value: T): T {
  return value;
}
