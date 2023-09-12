@intrinsic("data.end")
fn data_end(): usize;

@intrinsic("size_of")
fn size_of<T>(): usize;

@intrinsic("align_of")
fn align_of<T>(): usize;

@intrinsic("memory.size")
fn memory_size(): usize;

@intrinsic("memory.grow")
fn memory_grow(sz: usize): usize;

@intrinsic("table.get")
fn table_get(id: usize): opaque;

@intrinsic("table.set")
fn table_set(id: usize, val: opaque);
