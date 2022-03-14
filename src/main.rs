mod ast;
mod parser;
mod lexer;
mod utilities;

use parser::Parser;
use utilities::*;

use std::{
    env,
    path::Path
};

fn main() {
    let mut args = env::args();
    let name = args.next().unwrap();

    match args.next() {
        Some(arg) => match arg.as_str() {
            "--repl" | "-r" => repl(),
            "--file" | "-f" => {
                let path = args.next().unwrap();
                let path = Path::new(&path);
                file(path);
            }
            _ => help(&name),
        },
        _ => help(&name),
    }
}
