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

// TODO: Seriously consider replacing this struct with just a set of functions unless configuration is needed
pub(crate) struct RantCompiler {
}

impl RantCompiler {
  pub fn compile<R: Reporter>(source: &str, reporter: &mut R, debug_enabled: bool, info: RantProgramInfo) -> CompileResult {
    let info = Rc::new(info);

    let mut parser = RantParser::new(source, reporter, debug_enabled, &info);

    // Return compilation result
    match parser.parse() {
      Ok(seq) => Ok(RantProgram::new(seq, info)),
      Err(()) => Err(CompilerErrorKind::SyntaxError),
    }
  }
  
  pub fn compile_file<P: AsRef<Path>, R: Reporter>(path: P, reporter: &mut R, debug_enabled: bool) -> CompileResult {
    let source_name = path.as_ref().canonicalize().unwrap_or_else(|_| path.as_ref().to_path_buf()).to_string_lossy().to_string();
    let file_read_result = fs::read_to_string(path);
    match file_read_result {
      Ok(source) => {
        Self::compile(&source, reporter, debug_enabled, RantProgramInfo {
          name: None,
          path: Some(source_name)
        })
      },
      // Something went wrong with reading the file
      Err(err) => {
        // Report file IO issue
        let problem = match err.kind() {
            IOErrorKind::NotFound => Problem::FileNotFound(source_name),
            _ => Problem::FileIOError(err.to_string())
        };
        reporter.report(CompilerMessage::new(problem, Severity::Error, None));
        Err(CompilerErrorKind::IOError(err.kind()))
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
  DynamicKeyBlockMultiElement,
  FunctionBodyBlockMultiElement,
  UnclosedFunctionCall,
  UnclosedFunctionSignature,
  UnclosedStringLiteral,
  UnclosedVariableAccess,
  UnclosedList,
  UnclosedMap,
  AnonValueAssignment,
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
  ComposeValueReused,
  NothingToCompose,
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
      Problem::UnclosedVariableAccess =>                          "R-0012",
      Problem::UnclosedList =>                                    "R-0013",
      Problem::UnclosedMap =>                                     "R-0014",
      Problem::DynamicKeyBlockMultiElement =>                     "R-0015",
      Problem::FunctionBodyBlockMultiElement =>                   "R-0016",
      Problem::AnonValueAssignment =>                             "R-0017",
      Problem::ComposeValueReused =>                              "R-0018",
      Problem::NothingToCompose =>                                "R-0019",
      
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
      Problem::MissingIdentifier => "identifier required but is missing".to_owned(),
      Problem::LocalPathStartsWithIndex => "access path cannot start with an index; consider using a variable name or anonymous value here instead".to_owned(),
      Problem::InvalidSinkOn(elname) => format!("sink is not valid on {}", elname),
      Problem::InvalidHintOn(elname) => format!("hint is not valid on {}", elname),
      Problem::InvalidSink => "sink is not valid".to_owned(),
      Problem::InvalidHint => "hint is not valid".to_owned(),
      Problem::InvalidIdentifier(idname) => format!("'{}' is not a valid identifier; identifiers may only use alphanumeric characters, underscores, and hyphens but must also contain at least one non-digit", idname),
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
      Problem::UnclosedVariableAccess => "unclosed accessor; expected '>'".to_owned(),
      Problem::UnclosedList => "unclosed list initializer; expected ')'".to_owned(),
      Problem::UnclosedMap => "unclosed map initializer; expected ')'".to_owned(),
      Problem::DynamicKeyBlockMultiElement => "dynamic key blocks can't have more than one element; if branching is desired, create an inner block".to_owned(),
      Problem::FunctionBodyBlockMultiElement => "function body blocks can't have more than one element; if branching is desired, create an inner block".to_owned(),
      Problem::AnonValueAssignment => "can't assign directly to anonymous value; try assigning to a key or index instead".to_owned(),
      Problem::ComposeValueReused => "composition output consumed more than once within the same function call".to_owned(),
      Problem::NothingToCompose => "first function in composition cannot consume output".to_owned(),
    }
  }
  
  fn inline_message(&self) -> Option<String> {
    Some(match self {
      Problem::UnclosedBlock => "no matching '}' found".to_owned(),
      Problem::UnclosedStringLiteral => "string literal needs closing delimiter".to_owned(),
      Problem::ExpectedToken(token) => format!("expected '{}'", token),
      Problem::MissingIdentifier => "missing identifier".to_owned(),
      Problem::LocalPathStartsWithIndex => "replace with variable name or anonymous value".to_owned(),
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
      Problem::UnclosedVariableAccess => "no matching '>' found".to_owned(),
      Problem::UnclosedList => "no matching ')' found".to_owned(),
      Problem::UnclosedMap => "no matching ')' found".to_owned(),
      Problem::FunctionBodyBlockMultiElement => "multiple elements not allowed here".to_owned(),
      Problem::DynamicKeyBlockMultiElement => "multiple elements not allowed here".to_owned(),
      Problem::AnonValueAssignment => "direct assignment impossible".to_owned(),
      Problem::ComposeValueReused => "composition value reused here".to_owned(),
      Problem::NothingToCompose => "no previous output to consume".to_owned(),
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