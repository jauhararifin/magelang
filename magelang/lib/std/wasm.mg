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

