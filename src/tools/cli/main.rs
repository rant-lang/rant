use argh::FromArgs;
use colored::*;
use rant::*;
use exitcode;
use rand::Rng;
use std::io::{self, Write};
use std::{path::Path, time::Instant};
use std::process;
use codemap::CodeMap;
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

    /// run this code and exit (overrides -i)
    #[argh(option, short = 'r', long = "run")]
    run_code: Option<String>,

    /// run this file and exit
    #[argh(option, short = 'i')]
    in_file: Option<String>,

    /// only print program output and nothing else
    #[argh(switch, short = 'q')]
    quiet: bool
}

enum ProgramSource {
    Inline(String),
    Stdin(String),
    FilePath(String)
}

macro_rules! log_error {
    ($fmt:expr $(, $arg:expr),*) => {
        eprintln!("{}: {}", "error".bright_red().bold(), format!($fmt $(, $arg)*))
    }
}

fn main() {
    let args: CliArgs = argh::from_env();

    if args.version {
        println!("{}", BUILD_VERSION);
        return
    }

    if !args.quiet && args.run_code.is_none() && args.in_file.is_none() {
        println!("Rant {} ({})", BUILD_VERSION, embedded_triple::get());
    }

    let seed = args.seed.unwrap_or_else(|| rand::thread_rng().gen());
    let mut rant = Rant::with_seed(seed);

    // Run inline code from cmdline args
    if let Some(code) = &args.run_code {
        run_rant(&mut rant, ProgramSource::Inline(code.to_owned()), &args);
        return
    // Run input file from cmdline args
    } else if let Some(path) = &args.in_file {
        // Make sure it exists
        if !Path::new(path).exists() {
            log_error!("file not found: {}", path);
            process::exit(exitcode::NOINPUT);
        }
        run_rant(&mut rant, ProgramSource::FilePath(path.clone()), &args);
        return
    }
    
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let mut input = String::new();

        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                run_rant(&mut rant, ProgramSource::Stdin(input.to_owned()), &args);
            },
            Err(_) => log_error!("failed to read input")
        }
    }
}

fn run_rant(ctx: &mut Rant, source: ProgramSource, args: &CliArgs) {
    let show_stats = !args.quiet;
    let start_time = Instant::now();
    let compile_result = match &source {
        ProgramSource::Inline(source) => ctx.compile_str(source),
        ProgramSource::Stdin(source) => ctx.compile_str(source),
        ProgramSource::FilePath(path) => ctx.compile_file(path)
    };

    let parse_time = start_time.elapsed();

    // Make sure it compiled successfully
    match &compile_result {
        Ok(_) => {
            if show_stats {
                println!("{} in {:?}", "Compiled".bright_green().bold(), parse_time) 
            }
        },
        Err(errors) => {
            let code = match &source {
                ProgramSource::Inline(s) => s.to_owned(),
                ProgramSource::Stdin(s) => s.to_owned(),
                ProgramSource::FilePath(path) => std::fs::read_to_string(path).expect("can't open file for error reporting")
            };
            let errc = errors.len();            
            let mut codemap = CodeMap::new();
            let file_span = codemap.add_file(match &source {
                ProgramSource::Inline(_) => "(cmdline)",
                ProgramSource::Stdin(_) => "(stdin)",
                ProgramSource::FilePath(path) => path
            }.to_owned(), code).span;
            let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));

            for error in errors.iter() {
                let d = 
                if let Some(pos) = &error.pos {
                    let label = SpanLabel {
                        span: file_span.subspan(pos.span.start as u64, pos.span.end as u64),
                        label: Some(error.inline_message()),
                        style: SpanStyle::Primary
                    };
    
                    Diagnostic {
                        level: Level::Error,
                        message: error.message(),
                        code: Some(error.code().to_owned()),
                        spans: vec![label]
                    }
                } else {
                    Diagnostic {
                        level: Level::Error,
                        message: error.message(),
                        code: None,
                        spans: vec![]
                    }
                };
                emitter.emit(&[d]);
            }

            eprintln!("\n{}\n", format!("{} ({} {} found)", "Compile failed".bright_red(), errc, if errc == 1 { "error" } else { "errors" }).bold());
            return
        }
    }

    // Run it
    let program = compile_result.unwrap();
    let seed = args.seed.unwrap_or_else(|| rand::thread_rng().gen());
    ctx.set_seed(seed);
    let start_time = Instant::now();
    let run_result = ctx.run(&program);
    let run_time = start_time.elapsed();

    // Display results
    match run_result {
        Ok(output) => {
            println!("{}", output);
            if show_stats {
                println!("{} in {:?} (seed = {:016x})", "Executed".bright_green().bold(), run_time, seed);
            }
        },
        Err(err) => {
            eprintln!("{}: {:?}", "runtime error".bright_red().bold(), err);
            if show_stats {
                eprintln!("{} in {:?} (seed = {:016x})", "Crashed".bright_red().bold(), run_time, seed);
            }
        }
    }
}