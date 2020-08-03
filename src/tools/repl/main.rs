use rant::*;
use rand;
use rand::Rng;
use std::io::{self, Write};
use std::time::{Instant};
use compiler::RantCompiler;

fn main() {
    println!("Rant {} ({})", RANT_VERSION, embedded_triple::get());
    
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        let mut rant = Rant::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let source = input.as_str();
                let start_time = Instant::now();
                let compile_result = RantCompiler::compile_string(source);
                let parse_time = start_time.elapsed();

                // Make sure it compiled successfully
                match &compile_result {
                    Ok(_) => println!("(Compiled in {:?})", parse_time),
                    Err(errors) => {
                        eprintln!("{} error(s) found:", errors.len());
                        for (index, error) in errors.iter().enumerate() {
                            println!("  #{}: ({}) {}", index + 1, error.first_line_col, error.info);
                        }
                        continue
                    }
                }

                // Try to run it
                let program = compile_result.unwrap();
                let seed = rand::thread_rng().gen();
                let start_time = Instant::now();
                let run_result = rant.run(&program, seed);
                let run_time = start_time.elapsed();

                // Display results
                match run_result {
                    Ok(output) => println!("<< {}", output),
                    Err(err) => eprintln!("Runtime Error: {:?}", err)
                }
                println!("(Ran in {:?}, seed = {:x})", run_time, seed);
            },
            Err(_) => println!("Failed to read input")
        }
    }
}