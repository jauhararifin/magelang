type Direct struct { value: (i32, Direct) }
type Indirect struct { value: (bool, (Other,)) }
type Other struct { value: Indirect }
type Generic[T] struct { value: (T, Generic[T]) }

let a: (i32,) = (b.0,);
let b: (i32,) = (a.0,);

type Grouped struct { value: (Grouped) }
