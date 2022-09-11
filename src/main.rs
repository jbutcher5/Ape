pub mod asm;
pub mod code_gen;
pub mod parser;

use code_gen::Generator;
use code_gen::InterRep::*;
use code_gen::Type::*;

fn main() {
    let mut generator = Generator::new();
    generator.apply(vec![
        Define("x".to_string(), Int(2)),
        CCall(
            "print_value".to_string(),
            vec![generator.get_variable("x".to_string())],
        ),
    ]);

    println!("{:?}", generator.functions);
}
