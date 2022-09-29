pub mod asm;
pub mod code_gen;
pub mod parser;

use code_gen::Generator;
use code_gen::Node::*;
use code_gen::ReferenceType;
use code_gen::Type::*;

use std::{fs::File, io, io::Write};

fn main() -> io::Result<()> {
    let mut generator = Generator::default();
    generator.apply(vec![
        Extern(
            "printf".to_string(),
            vec![ReferenceType::Int, ReferenceType::Str, ReferenceType::Int],
        ),
        Extern(
            "addOne".to_string(),
            vec![ReferenceType::Int, ReferenceType::Int],
        ),
        Define(
            "y".to_string(),
            Box::new(Literal(Array(vec![Int(123), Int(345)]))),
        ),
        Define("z".to_string(), Box::new(Ident("y".to_string()))),
        Define(
            "w".to_string(),
            Box::new(CCall("addOne".to_string(), vec![Literal(Int(5))])),
        ),
        CCall(
            "printf".to_string(),
            vec![Literal(Str(r"%d\n".to_string())), Ident("w".to_string())],
        ),
    ]);

    let mut file = File::create("out.asm")?;
    file.write_all(&generator.export())?;

    Ok(())
}
