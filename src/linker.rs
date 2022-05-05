use crate::{
    bytecode::{Object, Program},
    errors::Error,
};

pub struct Linker();

impl Linker {
    pub fn new() -> Self {
        Self {}
    }
}

impl Linker {
    pub fn link(&self, objects: &[Object]) -> Result<Program, Error> {
        if objects.len() != 1 {
            todo!();
        }

        let main_func = objects[0].functions.iter().enumerate().find(|func| func.1.name == "main");
        if main_func.is_none() {
            return Err(Error::MissingMain);
        }

        Ok(Program {
            functions: objects[0].functions.iter().map(|f| f.clone()).collect(),
            entry_point: main_func.unwrap().0,
        })
    }
}
