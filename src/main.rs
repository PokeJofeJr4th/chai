#![warn(clippy::pedantic, clippy::nursery)]

use std::{fs, path::PathBuf};

use clap::Parser;
use interpreter::context::Context;
use parser::syntax::TopLevel;

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

    let arg_display = args.filename.display().to_string();
    let class_name = arg_display.split('/').last().unwrap();

    let toks = lexer::tokenize(&file).unwrap();

    println!("{toks:?}");

    let mut syn = parser::parse(toks).unwrap();

    if syn.iter().any(|tl| matches!(tl, TopLevel::Function { .. })) {
        syn = vec![TopLevel::Class(class_name.into(), syn)];
    }

    println!("{syn:#?}");

    let mut std_context = load_standard_lib()
        .map_err(|err| format!("While loading standard library: {err}"))
        .unwrap();

    println!("{std_context:#?}");

    let interpreted = interpreter::interpret(syn, &mut std_context).unwrap();

    println!("{interpreted:#?}");

    let base_path = args.filename.parent().unwrap();

    for (class, interpreted) in interpreted {
        let mut compiled = compiler::compile(interpreted, class.clone()).unwrap();

        println!("{compiled:#?}");

        let mut contents = Vec::new();
        compiled.write(&mut contents).unwrap();
        fs::write(format!("{}/{class}.class", base_path.display()), contents).unwrap();
    }
}

fn load_standard_lib() -> Result<Context, String> {
    interpreter::get_global_context(&parser::parse(lexer::tokenize(include_str!(
        "stdlib.chai"
    ))?)?)
}
