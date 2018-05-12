// A small compiler for the 65816 CPU architecture that also supports patches
#![feature(io, if_while_or_patterns, type_ascription, slice_patterns)]

extern crate byteorder;

use std::fs::File;
use std::error::Error;
use std::io::Read;
use std::env;
use std::process;

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

fn run() -> Result<(),Box<Error>> {
    let args = env::args().collect::<Vec<_>>();
    // TODO: arg parsing
    let filename = args.get(1).ok_or("gib arguments pls")?;
    let file = File::open(filename)?;
    let lexed = Lexer::new(filename.clone(), file.chars().map(|c| c.unwrap()));
    let parsed = Parser::new(lexed);
    let mut output = File::create("out.bin")?;
    for l in parsed {
        let mut err = false;
        println!("{}", l.as_ref().map(|c| c.to_string()).unwrap_or_else(|c| format!("{:?}", c)));
        if let Ok(Statement::Instruction { name, size, mut arg }) = l {
            if !err {
                //if let ArgumentKind::Label(_) | ArgumentKind::AnonLabel(_) = arg.kind { continue; }
                if let Span::Ident(c) = arg.span {
                    arg.span = Span::number(0, 2, c.start);
                }
                if let Span::AnonLabel(c) = arg.span {
                    arg.span = Span::number(0, 2, c.start);
                }
                if let Err(e) = instructions::write_instr(&mut output, name.as_ident().unwrap(), AddressingMode::parse(arg).map_err(|_| "oof")?) {
                    err = true;
                    println!("{:?}",e);
                }
            }
        } else {
            //err = true;
            println!("Some error lel");
        }
    }
    Ok(())
}

fn main() {
    if let Err(e) = run() {
        println!("\x1B[1;31mError\x1B[0m: {}", e);
        process::exit(1);
    }
}
