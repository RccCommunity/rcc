pub mod codegen;
pub mod grammer;
pub mod parser;

extern crate llvm_sys as llvm;
extern crate nom;

use std::fs::File;
use std::io::Read;

use codegen::codegen::CodeGenerator;
use parser::parser::RccParser;

fn main() {
    let mut input = String::new();
    let mut f = File::open("in.rcc").unwrap();
    f.read_to_string(&mut input).unwrap();

    // let parsed_input = parser::program(&input).unwrap();

    let r = RccParser::parse(&input);
    if r.is_err() {
        panic!("failed to parse file");
    }
    let parsed_input = r.unwrap();
    unsafe {
        CodeGenerator::codegen(parsed_input);
    }
}
