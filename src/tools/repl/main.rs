use rant::*;
use std::io::{self, Write};
use embedded_triple;
use line_col::*;

fn main() {
    println!("Rant {} ({})", RANT_VERSION, embedded_triple::get());

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let mut lexer = rant::compiler::lexer::lex(input.as_str());
                loop {
                    if let (Some(token), slice) = (lexer.next(), lexer.slice()) {
                        let lookup = LineColLookup::new(input.as_str());
                        let (line, col) = lookup.get(lexer.span().start);
                        println!("{:?}: \"{}\" @ {},{}", token, slice, line, col);
                    } else {
                        break
                    }
                }
            },
            Err(_) => println!("Failed to read input")
        }
    }
}