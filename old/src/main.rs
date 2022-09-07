#![feature(iter_intersperse)]

pub mod code_gen;
pub mod parser;

use code_gen::Program;
use parser::Parser;

fn main() -> std::io::Result<()> {
    let mut parser = Parser::new("test.ape")?;

    let result = parser.parse();
    let mut program = Program::new("out.asm")?;

    program.generate(result.unwrap());
    program.write()?;

    Ok(())
}
