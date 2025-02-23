# c-compiler
A simple windows c compiler implemented in rust for educational purposes. Follows Windows x86 conventions. 
Currently supports only programs consisting of functions with integer local variables, blocks, conditional statements, loops, constants, arithmetical, logical, and bit opperation. Includes constant folding and dead code elimination optimizations.

# To run
Clone the repository to your machine, and type
cargo run (optional flags) --manifest-path=/*path to directory*/c-compiler/Cargo.toml /*path to source file*/src.c (optional: /*path to destination*/)
this will create an asm file src.s in /*path to destination*/ if provided or /*path to source file*/ if not.

optional flags:
--optimize - enable all optimizations.
--fold-constants - enable constant folding optimization.
--eliminate-unreachable-code - enable unreachable code elimination.
