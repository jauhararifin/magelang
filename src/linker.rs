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

        let main_func = objects[0].symbol_table.get("main");
        if main_func.is_none() {
            return Err(Error::MissingMain);
        }

        Ok(Program {
            values: objects[0].values.iter().map(|v| v.clone()).collect(),
            entry_point: main_func.unwrap().clone(),
        })
    }
}
