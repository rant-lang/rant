use rant::*;
use rant::compiler::parser::*;
use std::io::{self, Write};
use std::time::{Instant, Duration};
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
                let start_time = Instant::now();
                let result = parser.parse();
                let parse_time = start_time.elapsed();
                match result {
                    Ok(rst) => println!("RST: {:#?}", rst),
                    Err(errors) => {
                        println!("{} error(s) found:", errors.len());
                        for error in errors.iter() {
                            let span = error.span();
                            let (line_start, col_start) = lookup.get(span.start);
                            let (line_end, col_end) = lookup.get(span.end - 1);
                            println!("  - ({},{} - {},{}): {}", line_start, col_start, line_end, col_end, error.info());
                        }
                    }
                }
                println!("(Parsed in {:?})", parse_time);
            },
            Err(_) => println!("Failed to read input")
        }
    }
}