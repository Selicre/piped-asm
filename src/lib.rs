// A small compiler for the 65816 CPU architecture that also supports patches
#![feature(if_while_or_patterns, type_ascription, slice_patterns, try_trait, nll, specialization, generators)]

// while development happens - don't wanna miss the important warns
#![allow(unused_imports, dead_code, unused_variables, unreachable_code)]
extern crate byteorder;
extern crate linked_hash_map;

use std::fs::File;
use std::error::Error;
use std::io::BufWriter;
use std::env;
use std::process;


pub mod lexer;
pub mod parser;
mod addrmodes;
mod instructions;
pub mod compiler;
pub mod linker;
mod expression;
mod attributes;
mod colors;
mod n_peek;
mod lls;

use compiler::Compiler;

pub fn run() -> Result<(),Box<Error>> {
    let args = env::args().collect::<Vec<_>>();
    // TODO: arg parsing
    let filename = args.get(1).ok_or("arguments pls")?;
    let output_filename = args.get(2).map(|c| &**c).unwrap_or("out.sfc");
    let compiled = Compiler::new(filename)?;
    let mut output = BufWriter::new(File::create(output_filename)?);
    linker::link(&mut output, compiled);
    Ok(())
}
