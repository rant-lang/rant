use argh::FromArgs;
use colored::*;
use rant::*;
use rand;
use rand::Rng;
use std::io::{self, Write};
use std::time::{Instant};
use compiler::RantCompiler;
use codemap::{Span, CodeMap};
use codemap_diagnostic::{ColorConfig, Emitter, SpanLabel, SpanStyle, Diagnostic, Level};

/// Run Rant code from your terminal.
#[derive(FromArgs)]
struct CliArgs {
    /// display build version and exit
    #[argh(switch, short = 'v')]
    version: bool,

    /// optional seed to run programs with (defaults to random seed)
    #[argh(option, short = 's')]
    seed: Option<u64>,

    /// run this code and exit
    #[argh(option, short = 'r')]
    run: Option<String>,

    /// run this file and exit
    #[argh(option, short = 'o')]
    open: Option<String>,

    /// only print program output and nothing else
    #[argh(switch, short = 'q')]
    quiet: bool
}

fn main() {
    let args: CliArgs = argh::from_env();

    if args.version {
        println!("{}", RANT_VERSION);
        return
    }

    if !args.quiet && args.run.is_none() {
        println!("Rant {} ({})", RANT_VERSION, embedded_triple::get());
    }

    let mut rant = Rant::new();

    // Run inline code from cmdline args
    if let Some(code) = &args.run {
        run_rant(&mut rant, code.as_str(), &args);
        return
    }
    
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();
        

        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                run_rant(&mut rant, input.as_str(), &args);
            },
            Err(_) => println!("Failed to read input")
        }
    }
}

fn run_rant(ctx: &mut Rant, source: &str, args: &CliArgs) {
    let show_stats = !args.quiet;
    let start_time = Instant::now();
    let compile_result = RantCompiler::compile_string(source);
    let parse_time = start_time.elapsed();

    // Make sure it compiled successfully
    match &compile_result {
        Ok(_) => {
            if show_stats {
                println!("{} in {:?}", "Compiled".bright_green().bold(), parse_time) 
            }
        },
        Err(errors) => {
            let errc = errors.len();            
            let mut codemap = CodeMap::new();
            let file_span = codemap.add_file("(stdin)".to_owned(), source.to_owned()).span;
            let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));

            for error in errors.iter() {
                let label = SpanLabel {
                    span: file_span.subspan(error.span.start as u64, error.span.end as u64),
                    label: Some(error.inline_message()),
                    style: SpanStyle::Primary
                };

                let d = Diagnostic {
                    level: Level::Error,
                    message: error.message(),
                    code: Some(error.code().to_owned()),
                    spans: vec![label]
                };

                emitter.emit(&[d]);
            }

            eprintln!("\n{}\n", format!("{} ({} {} found)", "Compile failed".red(), errc, if errc == 1 { "error" } else { "errors" }).bold());
            return
        }
    }

    // Run it
    let program = compile_result.unwrap();
    let seed = args.seed.unwrap_or_else(|| rand::thread_rng().gen());
    let start_time = Instant::now();
    let run_result = ctx.run(&program, seed);
    let run_time = start_time.elapsed();

    // Display results
    match run_result {
        Ok(output) => {
            println!("{}", output);
            if show_stats {
                println!("{} in {:?} (seed = {:x})", "Executed".bright_green().bold(), run_time, seed);
            }
        },
        Err(err) => {
            eprintln!("{}: {:?}", "runtime error".red().bold(), err);
            if show_stats {
                eprintln!("{} in {:?} (seed = {:x})", "Crashed".red().bold(), run_time, seed);
            }
        }
    }
}