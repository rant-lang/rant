use crate::{RantProgram, RantProgramInfo};
use self::parser::RantParser;
use std::{error::Error, fs};
use std::{fmt::Display, path::Path, rc::Rc};
use std::io::ErrorKind as IOErrorKind;

pub(crate) mod lexer;
pub(crate) mod reader;
pub(crate) mod parser;
pub(crate) mod message;

pub use message::*;

/// Type alias for `Result<RantProgram, CompilerErrorKind>`
pub type CompileResult = Result<RantProgram, CompilerErrorKind>;

/// Describes why a compilation failed.
#[derive(Debug)]
pub enum CompilerErrorKind {
  /// Compilation failed due to one or more syntax errors.
  SyntaxError,
  /// Compilation failed due to a file I/O error.
  IOError(IOErrorKind),
}

impl Display for CompilerErrorKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        CompilerErrorKind::SyntaxError => write!(f, "syntax error"),
        CompilerErrorKind::IOError(_) => write!(f, "I/O error"),
    }
  }
}

impl Error for CompilerErrorKind {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    None
  }
  fn cause(&self) -> Option<&dyn Error> {
    self.source()
  }  
}

/// Provides an interface through which the compiler can report errors and warnings.
pub trait Reporter {
  /// Passes a compiler message to the implementor for processing.
  fn report(&mut self, msg: CompilerMessage);
}

impl Reporter for () {
  fn report(&mut self, _msg: CompilerMessage) {}
}

impl Reporter for Vec<CompilerMessage> {
  fn report(&mut self, msg: CompilerMessage) {
    self.push(msg);
  }
}

pub(crate) fn compile_string<R: Reporter>(source: &str, reporter: &mut R, debug_enabled: bool, info: RantProgramInfo) -> CompileResult {
  let info = Rc::new(info);

  let mut parser = RantParser::new(source, reporter, debug_enabled, &info);

  // Return compilation result
  match parser.parse() {
    Ok(seq) => Ok(RantProgram::new(seq, info)),
    Err(()) => Err(CompilerErrorKind::SyntaxError),
  }
}

pub(crate) fn compile_file<P: AsRef<Path>, R: Reporter>(path: P, reporter: &mut R, debug_enabled: bool) -> CompileResult {
  let source_name = path.as_ref().canonicalize().unwrap_or_else(|_| path.as_ref().to_path_buf()).to_string_lossy().to_string();
  let file_read_result = fs::read_to_string(path);
  match file_read_result {
    Ok(source) => {
      compile_string(&source, reporter, debug_enabled, RantProgramInfo {
        name: None,
        path: Some(source_name)
      })
    },
    // Something went wrong with reading the file
    Err(err) => {
      // Report file IO issue
      let problem = match err.kind() {
          IOErrorKind::NotFound => Problem::FileNotFound(source_name),
          _ => Problem::FileSystemError(err.to_string())
      };
      reporter.report(CompilerMessage::new(problem, Severity::Error, None));
      Err(CompilerErrorKind::IOError(err.kind()))
    }
  }
}