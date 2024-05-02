#![warn(clippy::pedantic, clippy::nursery)]

use std::{fs, path::PathBuf};

use clap::Parser;

pub mod lexer;
pub mod parser;
pub mod types;

#[derive(Parser)]
struct Args {
    filename: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.filename).unwrap();

    let toks = lexer::tokenize(&file).unwrap();

    println!("{toks:?}");

    let syn = parser::parse(toks).unwrap();

    println!("{syn:#?}");
}
