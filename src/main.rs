//! Implements a simple compiler for c in rust.
//! To run: clone the project and type
//! cargo run --manifest-path=/*path to directory*/c-compiler/Cargo.toml /*path to source file*/src.c

use std::env;
use std::fs::File;
use std::io::{Read, Write};

use ast::lexer::Lexer;
use ast::parser::Parser;
use generator::Generator;
use tac::TacGenerator;
use optimizer::Optimizer;

pub mod ast;
pub mod generator;
pub mod tac;
pub mod optimizer;

/*
Program flags:
--optimize - enable all optimizations.
--fold-constants - enable constant folding optimization.
--eliminate-unreachable-code - enable unreachable code elimination.
*/

fn main(){

    let mut args:Vec<String>=env::args().collect();
    if args.len()<2{
        println!("No input file.");
        return;
    }
    let temp= args.pop().unwrap();
    let path;
    let mut path2;
    if temp.ends_with(".c"){
        path = temp;
        path2 = path.clone();
        path2.pop();
        path2.pop();
    }
    else{
        path2 = temp;
        path = args.pop().unwrap();
        if !path.ends_with(".c"){
            println!("Invalid input.")
        }
    }
    let mut fold_constants = true;
    let mut dead_code_elimination = true;
    for arg in args{
        if arg.ends_with("c-compiler.exe"){
            continue;
        }
        match arg.as_str() {
            "--optimize" => {
                fold_constants=true;
                dead_code_elimination=true;
            },
            "--fold-constants" => fold_constants=true,
            "--eliminate-unreachable-code" => dead_code_elimination=true,
            _ => {
                println!("Command not recognized.");
                return;
            }
        }
    }
    let mut file=File::open(&path).expect("Can't open file specified.");
    let mut s=String::new();
    file.read_to_string(&mut s).expect("Can't read from file specified.");
    let mut parser = Parser::new(Lexer::new(&s));
    
    if let Some(ast)=parser.get_ast(){
        let mut tac = TacGenerator::new();
        tac.convert_ast_to_tac(ast);
        let commands = tac.get_command_lists_and_consume();
        let opt = Optimizer::new(fold_constants, dead_code_elimination);
        let commands = opt.optimize(commands);
        let generator=Generator::new();
        let asm=generator.generate_assembly(commands);
        
        //let mut path2=path.clone();
        let path2=path2+&".s".to_string();
        let mut file=File::create(&path2).expect("Couldn't create file.");
        file.write_all(asm.as_bytes()).expect("Couldn't write to file");
    }
    else{
        println!("Compilation Failed!")
    }
}