use std::{fmt::Display, ops::Range};
use super::Problem;

/// Describes where in a source file a message was triggered.
#[derive(Debug, Clone)]
pub struct Position {
  line: usize,
  col: usize,
  span: Range<usize>
}

impl Position {
  pub(crate) fn new(line: usize, col: usize, span: Range<usize>) -> Self {
    Self {
      line,
      col, 
      span,
    }
  }
  
  /// Gets the line number of the position.
  pub fn line(&self) -> usize {
    self.line
  }
  
  /// Gets the column number of the position.
  pub fn col(&self) -> usize {
    self.col
  }

  /// Gets the span associated with the position.
  pub fn span(&self) -> Range<usize> {
    self.span.clone()
  }
}

impl Display for Position {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{},{}", self.line, self.col)
  }
}

/// Describes the severity of a compiler message.
#[derive(Debug, Copy, Clone)]
pub enum Severity {
  /// Advises the user of a potential problem, but still allows compilation to finish.
  Warning,
  /// Advises the user of a problem that prevents the source from compiling.
  Error
}

impl Display for Severity {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", match self {
      Severity::Warning => "warning",
      Severity::Error => "error",
    })
  }
}

/// Describes the location and nature of a compiler message.
#[derive(Debug)]
pub struct CompilerMessage {
  pos: Option<Position>,
  severity: Severity,
  info: Problem
}

impl CompilerMessage {
  pub(crate) fn new(info: Problem, severity: Severity, pos: Option<Position>) -> Self {
    Self {
      info,
      severity,
      pos
    }
  }

  /// Gets the position in the source where the message was triggered.
  pub fn pos(&self) -> Option<Position> {
    self.pos.clone()
  }

  /// Gets the severity of the message.
  pub fn severity(&self) -> Severity {
    self.severity
  }
  
  /// Gets a reference to the problem variant triggering the message.
  pub fn info(&self) -> &Problem {
    &self.info
  }
  
  /// Consumes the `CompilerMessage` and returns its position and info as a tuple.
  pub fn consume(self) -> (Option<Position>, Severity, Problem) {
    (self.pos, self.severity, self.info)
  }

  /// Gets the message code associated with the message.
  pub fn code(&self) -> &'static str {
    self.info.code()
  }

  /// Gets a message describing the error.
  pub fn message(&self) -> String {
    self.info.message()
  }

  /// Gets the inline message text, usually used to annotate the span.
  pub fn inline_message(&self) -> Option<String> {
    self.info.inline_message()
  }

  /// Gets the hint text associated with the message.
  pub fn hint(&self) -> Option<String> {
    self.info.hint()
  }

  /// Returns true if the message is an error.
  pub fn is_error(&self) -> bool {
    matches!(self.severity, Severity::Error)
  }

  /// Returns true if the message is a warning.
  pub fn is_warning(&self) -> bool {
    matches!(self.severity, Severity::Warning)
  }
}