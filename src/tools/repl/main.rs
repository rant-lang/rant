use rant::*;
use rant::stdlib::print_stdlib;
use std::io::{self, Write};
use std::time::{Instant, Duration};
use compiler::RantCompiler;

fn main() {
    println!("Rant {} ({})", RANT_VERSION, embedded_triple::get());
    print_stdlib();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let source = input.as_str();
                let start_time = Instant::now();
                let result = RantCompiler::compile_string(source);
                let parse_time = start_time.elapsed();
                match result {
                    Ok(prog) => println!("{:#?}", prog),
                    Err(errors) => {
                        println!("{} error(s) found:", errors.len());
                        for (index, error) in errors.iter().enumerate() {
                            println!("  #{}: ({}) {}", index + 1, error.first_line_col, error.info);
                        }
                    }
                }
                println!("(Parsed in {:?})", parse_time);
            },
            Err(_) => println!("Failed to read input")
        }
    }
}