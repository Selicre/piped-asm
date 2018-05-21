Hyper's assembler supreme (probably not actual name)

# How to run this

Step 1: install [Rust](https://www.rust-lang.org/)

Step 2: `cargo run -- <filename>`

# About

This is a multi-architecture pluggable assembler written in Rust, primarily targeted at development of Super Mario World hacks.

# Syntax

The assembler supports standard syntax for all commands, as well as some non-standard shorthands for constructs that are clunky enough to warrant extra syntax. The attribute system allows you to enforce strict rules on how to compile code (e.g. after a `SEP #$30` it is invalid to use `LDA #$2345` as this will always end up in the CPU misreading the instruction), as well as automatically generate instructions for the caller.

# Plugins

Plugins can be used at any point in the pipeline (lexer, parser, compiler). You can use anything that can read json from stdin and write to stdout, as well as dynamic libraries.

# Patches

To increase usability for people who don't know assembly, as well as enable non-destructive editing, the program can dynamically replace any code chunks from the source with 3rd party code.

Internally, after the first compilation pass, the resulting binary is stored as an ordered map of labeled code chunks, which is then linked into a rom. If the source file hasn't changed, the assembler may reuse the object file.
