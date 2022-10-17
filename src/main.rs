use ape::code_gen::Generator;
use ape::parser::Parser;
use std::{fs::File, io, io::Write};

fn main() -> io::Result<()> {
    let mut parser = Parser::new("test.ape")?;
    let mut generator = Generator::default();

    let ir = parser.parse_to_ir().unwrap();
    generator.apply(ir).unwrap();

    let mut file = File::create("out.asm")?;
    file.write_all(&generator.export())?;

    Ok(())
}
