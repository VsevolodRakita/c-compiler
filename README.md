# c-compiler
A simple windows c compiler implemented in rust for educational purposes. Follows Windows x86 conventions. 
Currently supports only programs consisting of functions with integer local variables, blocks, conditional statements, loops, constants, arithmetical, logical, and bit opperation.

# To run
Clone the repository to your machine, and type
cargo run --manifest-path=/*path to directory*/c-compiler/Cargo.toml /*path to source file*/src.c (optional: /*path to destination*/)
this will create an asm file src.s in /*path to destination*/ if provided or /*path to source file*/ if not.
