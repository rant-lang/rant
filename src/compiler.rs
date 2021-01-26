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
          _ => Problem::FileIOError(err.to_string())
      };
      reporter.report(CompilerMessage::new(problem, Severity::Error, None));
      Err(CompilerErrorKind::IOError(err.kind()))
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
  AccessPathStartsWithIndex,
  AccessPathStartsWithSlice,
  InvalidSliceBound(String),
  InvalidSinkOn(&'static str),
  InvalidHintOn(&'static str),
  InvalidSink,
  InvalidHint,
  NothingToCompose,
  UnusedVariable(String),
  UnusedParameter(String),
  UnusedFunction(String),
  EmptyFunctionBody(String),
  NestedFunctionDefMarkedConstant,
  ConstantReassignment(String),
  ConstantRedefinition(String),
  FileNotFound(String),
  FileIOError(String),
}

impl Problem {
  fn code(&self) -> &'static str {
    match self {
      // Syntax errors (0000 - 0099)
      // Tokens
      Problem::UnexpectedToken(_) =>                              "R-0000",
      Problem::ExpectedToken(_) =>                                "R-0001",
      // Unclosed
      Problem::UnclosedBlock =>                                   "R-0002",
      Problem::UnclosedFunctionCall =>                            "R-0003",
      Problem::UnclosedFunctionSignature =>                       "R-0004",
      Problem::UnclosedVariableAccess =>                          "R-0005",
      Problem::UnclosedStringLiteral =>                           "R-0006",
      Problem::UnclosedList =>                                    "R-0007",
      Problem::UnclosedMap =>                                     "R-0008",
      // Functions
      Problem::InvalidParamOrder(..) =>                           "R-0009",
      Problem::MissingFunctionBody =>                             "R-0010",
      Problem::UnclosedFunctionBody =>                            "R-0011",
      Problem::InvalidParameter(_) =>                             "R-0012",
      Problem::DuplicateParameter(_) =>                           "R-0013",
      Problem::MultipleVariadicParams =>                          "R-0014",
      // Blocks
      Problem::DynamicKeyBlockMultiElement =>                     "R-0015",
      Problem::FunctionBodyBlockMultiElement =>                   "R-0016",
      // Accessors
      Problem::AnonValueAssignment =>                             "R-0017",
      Problem::MissingIdentifier =>                               "R-0018",
      Problem::InvalidIdentifier(_) =>                            "R-0019",
      Problem::AccessPathStartsWithIndex =>                       "R-0020",
      Problem::AccessPathStartsWithSlice =>                       "R-0021",
      Problem::InvalidSliceBound(_) =>                            "R-0022",
      
      // Composition
      Problem::NothingToCompose =>                                "R-0023",
      
      // Static analysis errors (0100 - 0199)
      Problem::ConstantReassignment(_) =>                         "R-0100",
      Problem::ConstantRedefinition(_) =>                         "R-0101",

      // Hint/sink errors (0130 - 0139)
      Problem::InvalidSink | Problem::InvalidSinkOn(_) =>         "R-0130",
      Problem::InvalidHint | Problem::InvalidHintOn(_) =>         "R-0131",

      // Common warnings (1000 - 1099)
      Problem::UnusedVariable(_) =>                               "R-1000",
      Problem::UnusedParameter(_) =>                              "R-1001",
      Problem::UnusedFunction(_) =>                               "R-1002",
      Problem::EmptyFunctionBody(_) =>                            "R-1003",
      Problem::NestedFunctionDefMarkedConstant =>                 "R-1004",

      // File access errors (0100 - 0109)
      Problem::FileNotFound(_) =>                                 "R-2100",
      Problem::FileIOError(_) =>                                  "R-2101",
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
      Problem::AccessPathStartsWithIndex => "access paths cannot start with an index; consider using a variable or anonymous value here instead".to_owned(),
      Problem::AccessPathStartsWithSlice => "access paths cannot start with a slice; consider using a variable or anonymous value here instead".to_owned(),
      Problem::InvalidSliceBound(token) => format!("invalid slice bound: '{}'", token),
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
      Problem::UnusedVariable(vname) => format!("variable '{}' is defined but never used", vname),
      Problem::UnusedParameter(pname) => format!("parameter '{}' is never used", pname),
      Problem::UnusedFunction(fname) => format!("function '{}' is defined but never used", fname),
      Problem::EmptyFunctionBody(fname) => format!("function '{}' is empty", fname),
      Problem::NestedFunctionDefMarkedConstant => "nested function definition can't be made constant; function will be mutable".to_owned(),
      Problem::FileNotFound(file) => format!("file not found: '{}'", file),
      Problem::FileIOError(err) => format!("filesystem error: {}", err),
      Problem::UnclosedVariableAccess => "unclosed accessor; expected '>'".to_owned(),
      Problem::UnclosedList => "unclosed list initializer; expected ')'".to_owned(),
      Problem::UnclosedMap => "unclosed map initializer; expected ')'".to_owned(),
      Problem::DynamicKeyBlockMultiElement => "dynamic key blocks can't have more than one element; if branching is desired, create an inner block".to_owned(),
      Problem::FunctionBodyBlockMultiElement => "function body blocks can't have more than one element; if branching is desired, create an inner block".to_owned(),
      Problem::AnonValueAssignment => "can't assign directly to anonymous value; try assigning to a key or index instead".to_owned(),
      Problem::NothingToCompose => "no compose value is available in this scope".to_owned(),
      Problem::ConstantReassignment(cname) => format!("reassignment of known constant '{}'", cname),
      Problem::ConstantRedefinition(cname) => format!("redefinition of known constant '{}'", cname),
    }
  }
  
  fn inline_message(&self) -> Option<String> {
    Some(match self {
      Problem::UnclosedBlock => "no matching '}' found".to_owned(),
      Problem::UnclosedStringLiteral => "string literal needs closing delimiter".to_owned(),
      Problem::ExpectedToken(token) => format!("expected '{}'", token),
      Problem::MissingIdentifier => "missing identifier".to_owned(),
      Problem::AccessPathStartsWithIndex => "nothing to index".to_owned(),
      Problem::AccessPathStartsWithSlice => "nothing to slice".to_owned(),
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
      Problem::NothingToCompose => "no previous output to consume".to_owned(),
      Problem::NestedFunctionDefMarkedConstant => "use '$' here instead".to_owned(),
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