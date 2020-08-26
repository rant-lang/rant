#![allow(clippy::single_component_path_imports)]

use argh::FromArgs;
use codemap_diagnostic::{ColorConfig, Emitter, SpanLabel, SpanStyle, Diagnostic, Level};
use codemap::CodeMap;
use colored::*;
use compiler::Severity;
use ctrlc;
use exitcode::{self, ExitCode};
use rand::Rng;
use rant::*;
use rant::compiler::CompilerMessage;
use std::{path::Path, time::Instant};
use std::io::{self, Write};
use std::process;
use std::sync::mpsc;

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
  quiet: bool,

  /// don't emit debug symbols
  #[argh(switch, short = 'n')]
  no_debug: bool,
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
  // Signal handling
  let (sig_tx, sig_rx) = mpsc::channel::<()>();
  ctrlc::set_handler(move || {
    sig_tx.send(()).unwrap();
  }).expect("failed to create signal handler");

  macro_rules! check_ctrl_c {
    () => {
      if sig_rx.try_recv().is_ok() {
        process::exit(exitcode::OK)
      }
    }
  }

  // Read cmdline args
  let args: CliArgs = argh::from_env();
  
  if args.version {
    println!("{}", BUILD_VERSION);
    return
  }
  
  if !args.quiet && args.run_code.is_none() && args.in_file.is_none() {
    println!("Rant {} ({})", BUILD_VERSION, embedded_triple::get());
    println!("Run this tool with --help for available options.");
  }
  
  let seed = args.seed.unwrap_or_else(|| rand::thread_rng().gen());
  let mut rant = Rant::with_options(RantOptions {
    use_stdlib: true,
    debug_mode: !args.no_debug,
    seed,
  });

  check_ctrl_c!();
  
  // Run inline code from cmdline args
  if let Some(code) = &args.run_code {
    let code = run_rant(&mut rant, ProgramSource::Inline(code.to_owned()), &args);
    process::exit(code);
    // Run input file from cmdline args
  } else if let Some(path) = &args.in_file {
    // Make sure it exists
    if !Path::new(path).exists() {
      log_error!("file not found: {}", path);
      process::exit(exitcode::NOINPUT);
    }
    let code = run_rant(&mut rant, ProgramSource::FilePath(path.clone()), &args);
    process::exit(code);
  }
  
  loop {
    check_ctrl_c!();
    print!("{} ", ">>".cyan());
    io::stdout().flush().unwrap();
    let mut input = String::new();
    
    match io::stdin().read_line(&mut input) {
      Ok(_) => {
        check_ctrl_c!();
        run_rant(&mut rant, ProgramSource::Stdin(input.to_owned()), &args);
      },
      Err(_) => log_error!("failed to read input")
    }
  }
}

fn run_rant(ctx: &mut Rant, source: ProgramSource, args: &CliArgs) -> ExitCode {
  let show_stats = !args.quiet;
  let start_time = Instant::now();
  let mut problems: Vec<CompilerMessage> = vec![];

  let compile_result = match &source {
    ProgramSource::Inline(source) => ctx.compile(source, &mut problems).map(|p| p.with_name("cmdline")),
    ProgramSource::Stdin(source) => ctx.compile(source, &mut problems).map(|p| p.with_name("stdin")),
    ProgramSource::FilePath(path) => ctx.compile_file(path, &mut problems)
  };
  
  let parse_time = start_time.elapsed();
  
  // Make sure it compiled successfully
  match &compile_result {
    Ok(_) => {
      if show_stats {
        println!("{} in {:?}", "Compiled".bright_green().bold(), parse_time) 
      }
    },
    Err(_) => {
      let code = match &source {
        ProgramSource::Inline(s) => s.to_owned(),
        ProgramSource::Stdin(s) => s.to_owned(),
        ProgramSource::FilePath(path) => std::fs::read_to_string(path).expect("can't open file for error reporting")
      };     

      let mut codemap = CodeMap::new();

      let file_span = codemap.add_file(match &source {
        ProgramSource::Inline(_) => "(cmdline)",
        ProgramSource::Stdin(_) => "(stdin)",
        ProgramSource::FilePath(path) => path
      }.to_owned(), code).span;

      let mut emitter = Emitter::stderr(ColorConfig::Always, Some(&codemap));
      
      // Print errors/warnings
      for msg in problems.iter() {
        let d = Diagnostic {
          level: match msg.severity() {
            Severity::Warning => Level::Warning,
            Severity::Error => Level::Error,
          },
          message: msg.message(),
          code: Some(msg.code().to_owned()),
          spans: if let Some(pos) = &msg.pos() {
            let span = pos.span();
            let label = SpanLabel {
              span: file_span.subspan(span.start as u64, span.end as u64),
              label: msg.inline_message(),
              style: SpanStyle::Primary
            };
            vec![label]
          } else {
            vec![]
          }
        };
        emitter.emit(&[d]);
      }

      let errc = problems.iter().filter(|msg| msg.is_error()).count();
      
      eprintln!("\n{}\n", format!("{} ({} {} found)", "Compile failed".bright_red(), errc, if errc == 1 { "error" } else { "errors" }).bold());
      return exitcode::DATAERR
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
      if output.len() > 0 {
        println!("{}", output);
      }
      if show_stats {
        println!("{} in {:?} (seed = {:016x})", "Executed".bright_green().bold(), run_time, seed);
      }
      exitcode::OK
    },
    Err(err) => {
      eprintln!("{}: {}\n\nstack trace:\n{}", "Runtime error".bright_red().bold(), err.description, err.stack_trace.unwrap_or("(no trace available)".to_owned()));
      if show_stats {
        eprintln!("{} in {:?} (seed = {:016x})", "Crashed".bright_red().bold(), run_time, seed);
      }
      exitcode::SOFTWARE
    }
  }
}