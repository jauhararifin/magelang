use std::alloc::LayoutError;

#[derive(Debug)]
pub enum Error {
    AllocateError(LayoutError),
}

impl From<LayoutError> for Error {
    fn from(err: LayoutError) -> Error {
        Error::AllocateError(err)
    }
}
