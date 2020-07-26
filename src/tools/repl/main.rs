use rant::*;
use rant::compiler::parser::*;
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
                let mut parser = RantParser::new(source);
                let lookup = LineColLookup::new(source);
                match parser.parse() {
                    Ok(rst) => println!("RST: {:#?}", rst),
                    Err(errors) => {
                        println!("{} error(s) found:", errors.len());
                        for error in errors.iter() {
                            let span = error.span();
                            let (line_start, col_start) = lookup.get(span.start);
                            let (line_end, col_end) = lookup.get(span.end - 1);
                            println!("  - ({},{} - {},{}): {:?}", line_start, col_start, line_end, col_end, error.info());
                        }
                    }
                }
            },
            Err(_) => println!("Failed to read input")
        }
    }
}