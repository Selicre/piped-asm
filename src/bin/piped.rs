extern crate piped_asm;

use std::process;

fn main() {
    if let Err(e) = piped_asm::run() {
        println!("\x1B[1;31mError\x1B[0m: {}", e);
        process::exit(1);
    }
}
