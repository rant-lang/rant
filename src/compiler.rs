use crate::{syntax::{RST, Sequence}, RantProgram};
use parser::RantParser;
use std::{fmt::Display, rc::Rc, path::Path};
use std::io::ErrorKind as IOErrorKind;
use std::fs;

pub(crate) mod lexer;
pub(crate) mod reader;
pub(crate) mod parser;
pub(crate) mod message;

pub use message::*;

pub type CompileResult = Result<RantProgram, ()>;

/// Provides an interface through which the compiler reports errors and warnings.
pub trait Reporter {
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

pub struct RantCompiler {
  // TODO: Add compiler options
}

impl RantCompiler {
  pub fn compile_string<R: Reporter>(name: Option<&str>, source: &str, reporter: &mut R) -> CompileResult {
    let mut parser = RantParser::new(source, reporter);
    let result = parser.parse();

    // Return compilation result
    match result {
      Ok(rst) => Ok(RantProgram::new(match rst {
        RST::Sequence(seq) => seq,
        other => Rc::new(Sequence::new(vec![Rc::new(other)]))
      })),
      Err(()) => Err(()),
    }
  }
  
  pub fn compile_file<P: AsRef<Path>, R: Reporter>(path: P, reporter: &mut R) -> CompileResult {
    let source_name = path.as_ref().to_string_lossy().to_string();
    let file_read_result = fs::read_to_string(path);
    match file_read_result {
      Ok(source) => {
        Self::compile_string(Some(&source_name), &source, reporter)
      },
      // Something went wrong with reading the file
      Err(err) => {
        // Report file IO issue
        let problem = match err.kind() {
            IOErrorKind::NotFound => Problem::FileNotFound(source_name),
            _ => Problem::FileIOError(err.to_string())
        };
        reporter.report(CompilerMessage::new(problem, Severity::Error, None));
        Err(())
      }
    }
  }
}

/// Describes a problem (warning/error) encountered when building a source.
#[derive(Debug)]
pub enum Problem {
  UnexpectedToken(String),
  ExpectedToken(String),
  UnclosedBlock,
  UnclosedFunctionCall,
  UnclosedFunctionSignature,
  UnclosedStringLiteral,
  MultipleVariadicParams,
  MissingFunctionBody,
  UnclosedFunctionBody,
  InvalidParamOrder(String, String),
  InvalidParameter(String),
  MissingIdentifier,
  InvalidIdentifier(String),
  DuplicateParameter(String),
  LocalPathStartsWithIndex,
  InvalidSinkOn(&'static str),
  InvalidHintOn(&'static str),
  InvalidSink,
  InvalidHint,
  UnusedVariable(String),
  UnusedParameter(String),
  EmptyFunctionBody(String),
  FileNotFound(String),
  FileIOError(String),
}

impl Problem {
  fn code(&self) -> &'static str {
    match self {
      // Common errors (0000 - 0019)
      Problem::UnexpectedToken(_) =>                              "R-0000",
      Problem::ExpectedToken(_) =>                                "R-0001",
      Problem::UnclosedBlock =>                                   "R-0002",
      Problem::UnclosedFunctionCall =>                            "R-0003",
      Problem::UnclosedFunctionSignature =>                       "R-0004",
      Problem::InvalidParamOrder(..) =>                           "R-0005",
      Problem::MissingFunctionBody =>                             "R-0006",
      Problem::UnclosedFunctionBody =>                            "R-0007",
      Problem::InvalidParameter(_) =>                             "R-0008",
      Problem::DuplicateParameter(_) =>                           "R-0009",
      Problem::UnclosedStringLiteral =>                           "R-0010",
      Problem::MultipleVariadicParams =>                          "R-0011",
      
      // Access path errors (0020 - 0029)
      Problem::MissingIdentifier =>                               "R-0020",
      Problem::InvalidIdentifier(_) =>                            "R-0021",
      Problem::LocalPathStartsWithIndex =>                        "R-0022",
      
      // Hint/sink errors (0030 - 0039)
      Problem::InvalidSink | Problem::InvalidSinkOn(_) =>         "R-0030",
      Problem::InvalidHint | Problem::InvalidHintOn(_) =>         "R-0031",

      // File access errors (0100 - 0109)
      Problem::FileNotFound(_) =>                                 "R-0100",
      Problem::FileIOError(_) =>                                  "R-0101",

      // Common warnings (1000 - 1019)
      Problem::UnusedVariable(_) =>                               "R-1000",
      Problem::UnusedParameter(_) =>                              "R-1001",
      Problem::EmptyFunctionBody(_) =>                            "R-1002",
    }
  }
  
  fn message(&self) -> String {
    match self {
      Problem::UnclosedBlock => "unclosed block; expected '}'".to_owned(),
      Problem::UnclosedFunctionCall => "unclosed function call; expected ']'".to_owned(),
      Problem::UnclosedStringLiteral => "unclosed string literal".to_owned(),
      Problem::ExpectedToken(token) => format!("expected token: '{}'", token),
      Problem::UnexpectedToken(token) => format!("unexpected token: '{}'", token),
      Problem::MissingIdentifier => "missing identifier".to_owned(),
      Problem::LocalPathStartsWithIndex => "variable access path cannot start with an index".to_owned(),
      Problem::InvalidSinkOn(elname) => format!("sink is not valid on {}", elname),
      Problem::InvalidHintOn(elname) => format!("hint is not valid on {}", elname),
      Problem::InvalidSink => "sink is not valid".to_owned(),
      Problem::InvalidHint => "hint is not valid".to_owned(),
      Problem::InvalidIdentifier(idname) => format!("'{}' is not a valid identifier; identifiers may only contain alphanumeric characters, underscores, and hyphens and must contain at least one non-digit", idname),
      Problem::UnclosedFunctionSignature => "unclosed function signature; expected ']' followed by body block".to_owned(),
      Problem::InvalidParamOrder(first, second) => format!("{} is not allowed after {}", second, first),
      Problem::MissingFunctionBody => "missing body in function definition".to_owned(),
      Problem::UnclosedFunctionBody => "unclosed function body; expected '}'".to_owned(),
      Problem::InvalidParameter(pname) => format!("invalid parameter '{}'; must be a valid identifier or '*'", pname),
      Problem::DuplicateParameter(pname) => format!("duplicate parameter '{}' in function signature", pname),
      Problem::MultipleVariadicParams => "multiple variadic parameters are not allowed".to_owned(),
      Problem::UnusedVariable(vname) => format!("variable '{}' is not used", vname),
      Problem::UnusedParameter(pname) => format!("parameter '{}' is not used", pname),
      Problem::EmptyFunctionBody(fname) => format!("function '{}' is empty", fname),
      Problem::FileNotFound(file) => format!("file not found: '{}'", file),
      Problem::FileIOError(err) => format!("filesystem error: {}", err),
    }
  }
  
  fn inline_message(&self) -> Option<String> {
    Some(match self {
      Problem::UnclosedBlock => "no matching '}' found".to_owned(),
      Problem::UnclosedStringLiteral => "string literal needs closing delimiter".to_owned(),
      Problem::ExpectedToken(token) => format!("expected '{}'", token),
      Problem::UnexpectedToken(_) => "this probably shouldn't be here".to_owned(),
      Problem::MissingIdentifier => "missing identifier".to_owned(),
      Problem::LocalPathStartsWithIndex => "use a variable name here".to_owned(),
      Problem::InvalidSink | Problem::InvalidSinkOn(_) => "sink not allowed here".to_owned(),
      Problem::InvalidHint | Problem::InvalidHintOn(_) => "hint not allowed here".to_owned(),
      Problem::InvalidIdentifier(_) => "invalid identifier".to_owned(),
      Problem::UnclosedFunctionCall => "no matching ']' found".to_owned(),
      Problem::UnclosedFunctionSignature => "no matching ']' found".to_owned(),
      Problem::InvalidParamOrder(_, second) => format!("{} is not valid in this position", second),
      Problem::MissingFunctionBody => "function body should follow".to_owned(),
      Problem::UnclosedFunctionBody => "no matching '}' found".to_owned(),
      Problem::InvalidParameter(_) => "invalid parameter".to_owned(),
      Problem::DuplicateParameter(_) => "rename parameter to something unique".to_owned(),
      Problem::MultipleVariadicParams => "remove extra variadic parameter".to_owned(),
      _ => return None
    })
  }

  fn hint(&self) -> Option<String> {
    None
  }
}

impl Display for Problem {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.message())
  }
}