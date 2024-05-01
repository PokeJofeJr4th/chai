#![warn(clippy::pedantic, clippy::nursery)]

use std::{fs, path::PathBuf};

use clap::Parser;
pub mod lexer;

#[derive(Parser)]
struct Args {
    filename: PathBuf,
}

fn main() {
    let args = Args::parse();

    let file = fs::read_to_string(args.filename).unwrap();

    let toks = lexer::tokenize(&file).unwrap();

    println!("{toks:?}");
}
