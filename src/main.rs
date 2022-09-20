pub mod asm;
pub mod code_gen;
pub mod parser;

use code_gen::Generator;
use code_gen::Node::*;
use code_gen::Type::*;
use code_gen::IR::*;

use std::{fs::File, io, io::Write};

fn main() -> io::Result<()> {
    let mut generator = Generator::default();
    generator.apply(vec![
        Define("y".to_string(), Literal(Array(vec![Int(123), Int(345)]))),
        Define("z".to_string(), Ident("y".to_string())),
        CCall(
            "printf".to_string(),
            vec![Literal(Str(r"%d\n".to_string())), Ident("z".to_string())],
        ),
    ]);

    let mut file = File::create("out.asm")?;
    file.write_all(&generator.export())?;

    Ok(())
}
