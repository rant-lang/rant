use crate::{syntax::{RST, Sequence}, RantProgram};
use message::SyntaxError;
use parser::RantParser;
use line_col::LineColLookup;
use std::{fmt::Display, ops::Range, rc::Rc, path::Path};
use std::io::Error as IOError;
use std::fs;

pub(crate) mod lexer;
pub(crate) mod reader;
pub(crate) mod parser;
pub(crate) mod message;

pub type CompileResult = Result<RantProgram, Vec<CompilerError>>;

#[derive(Debug)]
pub struct LineCol {
  line: usize,
  col: usize
}

impl LineCol {
  pub fn new((line, col): (usize, usize)) -> Self {
    Self {
      line, col
    }
  }
  
  pub fn line(&self) -> usize {
    self.line
  }
  
  pub fn col(&self) -> usize {
    self.col
  }
}

impl Display for LineCol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{},{}", self.line, self.col)
  }
}

/// Represents a compiler error as an error type and optional position.
#[derive(Debug)]
pub struct CompilerError {
  pub info: CompilerErrorType,
  pub pos: Option<CompilerErrorPos>,
}

/// Source position data for compiler errors
#[derive(Debug)]
pub struct CompilerErrorPos {
  pub span: Range<usize>,
  pub first_line_col: LineCol,
  pub last_line_col: LineCol,
}

impl CompilerError {
  pub fn message(&self) -> String {
    self.info.message()
  }
  
  pub fn inline_message(&self) -> String {
    self.info.inline_message()
  }
  
  pub fn code(&self) -> &'static str {
    self.info.code()
  }
}

#[derive(Debug)]
pub enum CompilerErrorType {
  SyntaxError(SyntaxError),
  FileError(IOError),
  Other
}

impl CompilerErrorType {
  pub fn code(&self) -> &'static str {
    match self {
      CompilerErrorType::SyntaxError(err) => err.code(),
      _ => "TODO", // TODO
    }
  }
  
  pub fn message(&self) -> String {
    match self {
      CompilerErrorType::SyntaxError(err) => err.message(),
      _ => "unknown error".to_owned() // TODO
    }
  }
  
  pub fn inline_message(&self) -> String {
    match self {
      CompilerErrorType::SyntaxError(err) => err.inline_message(),
      _ => "unknown error".to_owned() // TODO
    }
  }
}

impl Display for CompilerErrorType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      CompilerErrorType::SyntaxError(err) => err.fmt(f),
      CompilerErrorType::FileError(err) => err.fmt(f),
      CompilerErrorType::Other => write!(f, "(other error)"),
    }
  }
}

pub struct RantCompiler {
  // TODO: Add compiler options
}

impl RantCompiler {
  pub fn compile_string(source: &str) -> CompileResult {
    let mut parser = RantParser::new(source);
    match parser.parse() {
      Ok(rst) => Ok(RantProgram::new(match rst {
        RST::Sequence(seq) => seq,
        other => Rc::new(Sequence::new(vec![Rc::new(other)]))
      })),
      Err(mut errors) => {
        let lookup = LineColLookup::new(source);
        Err(errors.drain(..).map(|err| {
          let (span, info) = err.consume();
          let (line_start, col_start) = lookup.get(span.start);
          let (line_end, col_end) = lookup.get(span.end.saturating_sub(1));
          CompilerError {
            info: CompilerErrorType::SyntaxError(info),
            pos: Some(CompilerErrorPos {
              span,
              first_line_col: LineCol::new((line_start, col_start)),
              last_line_col: LineCol::new((line_end, col_end)),
            })
          }
        }).collect())
      }
    }
  }
  
  pub fn compile_file<P: AsRef<Path>>(path: P) -> CompileResult {
    let file_read_result = fs::read_to_string(path);
    match file_read_result {
      Ok(source) => {
        Self::compile_string(&source)
      },
      Err(err) => {
        let ioerr = CompilerError {
          info: CompilerErrorType::FileError(err),
          pos: None
        };
        Err(vec![ioerr])
      }
    }
  }
}