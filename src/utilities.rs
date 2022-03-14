use crate::Parser;

use std::{
    io::{self, Write},
    path::Path,
    fs
};

pub fn help(name: &str) {
    println!(
        "Usage: {name} --repl or {name} --file path/to/file",
        name = name
    );
}

pub fn repl() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        // println!("{:#?}", Lexer::new(&input).collect::<Vec<_>>());
        parse(&input);
    }
}

pub fn file(path: &Path) {
    let contents = fs::read_to_string(path).unwrap();
    parse(&contents);
}

pub fn parse(input: &str) {
    let mut parser = Parser::new(input);
    match parser.expr() {
        Ok(expr) => println!("{expr}"),
        Err(err) => eprintln!("{err}"),
    }
}
