use ape::code_gen::Generator;
use ape::lexer::Lexer;
use ape::parser::parse;
use std::{fs::File, io, io::Write, path::Path};

fn main() -> io::Result<()> {
    let mut lexer = Lexer::try_from(Path::new("test.ape"))?;
    let mut generator = Generator::default();

    let tokens = lexer.tokenise();

    println!("{:?}", &tokens);

    generator.apply(parse(tokens).unwrap()).unwrap();

    let mut file = File::create("out.asm")?;
    file.write_all(&generator.export())?;

    Ok(())
}
