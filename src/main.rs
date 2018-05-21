// A small compiler for the 65816 CPU architecture that also supports patches
#![feature(io, if_while_or_patterns, type_ascription, slice_patterns, iterator_flatten)]

// while development happens - don't wanna miss the important warns

#![allow(unused_imports, dead_code, unused_variables)]

extern crate byteorder;
extern crate linked_hash_map;

use std::fmt::Debug;
use std::fs::File;
use std::error::Error;
use std::io;
use std::io::Read;
use std::io::BufWriter;
use std::env;
use std::process;

use linked_hash_map::LinkedHashMap;

mod lexer;
use lexer::Lexer;
use lexer::Span;

mod parser;
use parser::Parser;
use parser::Argument;
use parser::Statement;

mod addrmodes;
use addrmodes::AddressingMode;

mod instructions;
use instructions::Instruction;

mod compiler;
use compiler::Compiler;

mod linker;

fn run() -> Result<(),Box<Error>> {
    let args = env::args().collect::<Vec<_>>();
    // TODO: arg parsing
    let filename = args.get(1).ok_or("arguments pls")?;
    let file = match &**filename {
        "-" => Box::new(io::stdin()) as Box<Read>,
        c => Box::new(File::open(filename)?)
    };
    // Pipeline: chars -> lexer -> (macro expander) -> parser -> compiler -> linker
    let lexed = Lexer::new(filename.clone(), file.chars().map(|c| c.unwrap()));
    let parsed = Parser::new(lexed).map(|c| c.unwrap());
    let compiled = Compiler::new(parsed).map(|c| c.unwrap());
    let mut output = BufWriter::new(File::create("out.sfc")?);
    linker::link(&mut output, compiled);
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        println!("\x1B[1;31mError\x1B[0m: {}", e);
        process::exit(1);
    }
}
