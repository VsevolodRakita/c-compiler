# c-compiler
A simple windows c compiler implemented in rust for educational purposes.
Currently supports only programs consisting of one function, with a single return statement and expression consisting of an arithmetic/logical expression.

# To run
Clone the repository to your machine, and type
cargo run --manifest-path=/*path to directory*/c-compiler/Cargo.toml /*path to source file*/src.c (optional: /*path to destination*/)
this will create an asm file src.s in /*path to destination*/ if provided or /*path to source file*/ if not.
