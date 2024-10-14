//! Implements a simple compiler for c in rust.
//! To run: clone the project and type
//! cargo run --manifest-path=/*path to directory*/c-compiler/Cargo.toml /*path to source file*/src.c

use std::env;
use std::fs::File;
use std::io::{Read, Write};

use ast::lexer::Lexer;
use ast::parser::Parser;
use generator::Generator;


pub mod ast;
pub mod generator;

fn main(){
    let mut args=env::args();
    args.next();
    let path= args.next().expect("No source file specified.");
    println!("{}",&path);
    let mut file=File::open(&path).expect("Can't open file specified.");
    let mut s=String::new();
    file.read_to_string(&mut s).expect("Can't read from file specified.");
    let mut parser = Parser::new(Lexer::new(&s));
    
    if let Some(asm)=parser.get_ast(){
        let generator=Generator::new();
        let s=generator.generate_assembly(asm);
        let mut path2=path.split(".");
        let path2=path2.next().unwrap().to_string()+&".s".to_string();
        let mut file=File::create(&path2).expect("Couldn't create file.");
        file.write_all(s.as_bytes()).expect("Couldn't write to file");
    }
    else{
        println!("Compilation Failed!")
    }
}