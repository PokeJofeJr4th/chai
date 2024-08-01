#![warn(clippy::pedantic, clippy::nursery)]

use std::{fs, path::PathBuf};

use clap::Parser;
use interpreter::context::Context;

pub mod compiler;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod types;

#[derive(Parser)]
struct Args {
    filename: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(format!("{}.chai", args.filename.display())).unwrap();

    let toks = lexer::tokenize(&file).unwrap();

    println!("{toks:?}");

    let syn = parser::parse(toks).unwrap();

    println!("{syn:#?}");

    let mut std_context = load_standard_lib()
        .map_err(|err| format!("While loading standard library: {err}"))
        .unwrap();

    println!("{std_context:#?}");

    let interpreted = interpreter::interpret(syn, &mut std_context).unwrap();

    println!("{interpreted:#?}");

    let mut compiled = compiler::compile(interpreted).unwrap();

    println!("{compiled:#?}");

    let mut contents = Vec::new();
    compiled.write(&mut contents).unwrap();
    fs::write(format!("{}.class", args.filename.display()), contents).unwrap();
}

fn load_standard_lib() -> Result<Context, String> {
    interpreter::get_global_context(&parser::parse(lexer::tokenize(include_str!(
        "stdlib.chai"
    ))?)?)
}
