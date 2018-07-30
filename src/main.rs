// A small compiler for the 65816 CPU architecture that also supports patches
#![feature(io, if_while_or_patterns, type_ascription, slice_patterns, iterator_flatten, try_trait, nll, specialization)]

// while development happens - don't wanna miss the important warns
#![allow(unused_imports, dead_code, unused_variables, unreachable_code)]
extern crate byteorder;
extern crate linked_hash_map;

use std::fs::File;
use std::error::Error;
use std::io::BufWriter;
use std::env;
use std::process;


mod lexer;
mod parser;
mod addrmodes;
mod instructions;
mod compiler;
mod linker;
mod expression;
mod attributes;
mod colors;
mod n_peek;
mod lls;

use compiler::Compiler;

fn run() -> Result<(),Box<Error>> {
    let args = env::args().collect::<Vec<_>>();
    // TODO: arg parsing
    let filename = args.get(1).ok_or("arguments pls")?;
    let output_filename = args.get(2).map(|c| &**c).unwrap_or("out.sfc");
    // Pipeline: chars -> lexer -> (macro expander) -> parser -> compiler -> linker
    let compiled = Compiler::new(filename)?;
    let mut output = BufWriter::new(File::create(output_filename)?);
    linker::link(&mut output, compiled);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        println!("\x1B[1;31mError\x1B[0m: {}", e);
        process::exit(1);
    }
}
