use std::io::Result;

fn main() -> Result<()> {
    prost_build::compile_protos(&["src/bytecode/model.proto"], &["src/bytecode"])?;
    Ok(())
}
