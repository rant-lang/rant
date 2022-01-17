#![allow(clippy::single_component_path_imports)]

use clap::{App, Arg};
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

struct RantCliOptions {
  no_debug: bool,
  no_warn: bool,
  bench_mode: bool,
  seed: Option<u64>,
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
  let version_long = format!("{} [{}]", BUILD_VERSION, embedded_triple::get());

  let arg_matches = App::new("Rant CLI")
    .version(BUILD_VERSION)
    .about("Command-line interface for Rant 4.x")
    .long_version(version_long.as_str())
    .arg(Arg::with_name("seed")
      .help("Specifies the initial 64-bit hex seed")
      .short("s")
      .value_name("SEED")
    )
    .arg(Arg::with_name("eval")
      .help("Specifies a string to run if no file is specified")
      .short("e")
      .long("eval")
      .value_name("PROGRAM_STRING")
    )
    .arg(Arg::with_name("bench-mode")
      .help("Enables benchmarking")
      .short("b")
      .long("bench-mode")
    )
    .arg(Arg::with_name("no-warnings")
      .help("Disables compiler warnings")
      .short("W")
      .long("no-warnings")
    )
    .arg(Arg::with_name("no-debug")
      .help("Disable emitting debug synbols (may improve performance)")
      .short("D")
      .long("no-debug")
    )
    .arg(Arg::with_name("FILE")
      .help("Specifies a Rant file to run")
      .index(1)
    )
    .get_matches();

  // Signal handling
  let (sig_tx, sig_rx) = mpsc::channel::<()>();
  ctrlc::set_handler(move || {
    sig_tx.send(()).unwrap();
  }).expect("failed to create signal handler");

  std::thread::spawn(move || {
    if sig_rx.recv().is_ok() {
      process::exit(exitcode::OK)
    }
  });

  let opts = RantCliOptions {
    bench_mode: arg_matches.is_present("bench-mode"),
    no_debug: arg_matches.is_present("no-debug"),
    no_warn: arg_matches.is_present("no-warnings"),
    seed: arg_matches.value_of("seed").map(|seed_str| u64::from_str_radix(seed_str, 16).ok()).flatten(),
  };
  
  let in_str = arg_matches.value_of("eval");
  let in_file = arg_matches.value_of("FILE");
  
  let mut rant = Rant::with_options(RantOptions {
    use_stdlib: true,
    debug_mode: !opts.no_debug,
    seed: opts.seed.unwrap_or_else(|| rand::thread_rng().gen()),
    .. Default::default()
  });
  
  // Check if the user supplied a source to run
  if let Some(code) = in_str {
    // Run inline code from cmdline args
    let code = run_rant(&mut rant, ProgramSource::Inline(code.to_owned()), &opts);
    process::exit(code);
    
  } else if let Some(path) = in_file {
    // Run input file from cmdline args
    if !Path::new(path).exists() {
      log_error!("file not found: {}", path);
      process::exit(exitcode::NOINPUT);
    }
    let code = run_rant(&mut rant, ProgramSource::FilePath(path.to_owned()), &opts);
    process::exit(code);
  }

  repl(&mut rant, &opts);
}

fn repl(rant: &mut Rant, opts: &RantCliOptions) {
  loop {
    print!("{} ", ">>".cyan());
    io::stdout().flush().unwrap();
    let mut input = String::new();
    
    match io::stdin().read_line(&mut input) {
      Ok(_) => {
        run_rant(rant, ProgramSource::Stdin(input.trim_end().to_owned()), opts);
      },
      Err(_) => log_error!("failed to read input")
    }
  }
}

fn run_rant(ctx: &mut Rant, source: ProgramSource, opts: &RantCliOptions) -> ExitCode {
  let show_stats = opts.bench_mode;
  let start_time = Instant::now();
  let mut problems: Vec<CompilerMessage> = vec![];

  let compile_result = match &source {
    ProgramSource::Inline(source) => ctx.compile_named(source, &mut problems, "cmdline"),
    ProgramSource::Stdin(source) => ctx.compile_named(source, &mut problems, "stdin"),
    ProgramSource::FilePath(path) => ctx.compile_file(path, &mut problems)
  };
  
  let parse_time = start_time.elapsed();

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
    if opts.no_warn && msg.is_warning() { continue }
    
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
    eprintln!(); // extra line to separate code from errors
    emitter.emit(&[d]);
  }

  let errc = problems.iter().filter(|msg| msg.is_error()).count();
  
  // Make sure it compiled successfully
  match &compile_result {
    Ok(_) => {
      if show_stats {
        eprintln!("{} in {:?}", "Compiled".bright_green().bold(), parse_time) 
      }
    },
    Err(_) => {
      eprintln!("\n{}\n", format!("{} ({} {} found)", "Compile failed".bright_red(), errc, if errc == 1 { "error" } else { "errors" }).bold());
      return exitcode::DATAERR
    }
  }
  
  // Run it
  let program = compile_result.unwrap();
  let seed = opts.seed.unwrap_or_else(|| rand::thread_rng().gen());
  ctx.set_seed(seed);
  let start_time = Instant::now();
  let run_result = ctx.run(&program).map(|output| output.to_string());
  let run_time = start_time.elapsed();
  
  // Display results
  match run_result {
    Ok(output) => {
      if !output.is_empty() {
        println!("{}", output);
      }
      if show_stats {
        eprintln!("{} in {:?} (seed = {:016x})", "Executed".bright_green().bold(), run_time, seed);
      }
      exitcode::OK
    },
    Err(err) => {
      eprintln!("{}: {}\n\nstack trace:\n{}", "Runtime error".bright_red().bold(), &err, &err.stack_trace.as_deref().unwrap_or("(no trace available)"));
      if show_stats {
        eprintln!("{} in {:?} (seed = {:016x})", "Crashed".bright_red().bold(), run_time, seed);
      }
      exitcode::SOFTWARE
    }
  }
}