use rant::*;
use rant::compiler::reader::RantTokenReader;
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
                let source = input.as_str();
                let lexer = RantTokenReader::new(source);
                let lookup = LineColLookup::new(source);
                for (token, range) in lexer {
                    let (line, col) = lookup.get(range.start);
                    println!("{:?}: \"{}\" @ {},{}", token, &source[range], line, col);
                }
            },
            Err(_) => println!("Failed to read input")
        }
    }
}