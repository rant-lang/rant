use std::{fmt::Display, ops::Range};

/// Describes the location and nature of a compiler message.
#[derive(Debug)]
pub struct Message<T> {
  span: Range<usize>,
  info: T
}

impl<T> Message<T> {
  pub(crate) fn new(info: T, span: Range<usize>) -> Self {
    Self {
      info,
      span
    }
  }
  
  pub fn span(&self) -> Range<usize> {
    self.span.clone()
  }
  
  pub fn info(&self) -> &T {
    &self.info
  }
  
  pub fn consume(self) -> (Range<usize>, T) {
    (self.span, self.info)
  }
}

/// The information describing a syntax error as seen by the parser.
#[derive(Debug)]
pub enum SyntaxError {
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
}

impl SyntaxError {
  pub fn code(&self) -> &'static str {
    match self {
      // Common errors (0000 - 0019)
      SyntaxError::UnexpectedToken(_) =>                              "RE-0000",
      SyntaxError::ExpectedToken(_) =>                                "RE-0001",
      SyntaxError::UnclosedBlock =>                                   "RE-0002",
      SyntaxError::UnclosedFunctionCall =>                            "RE-0003",
      SyntaxError::UnclosedFunctionSignature =>                       "RE-0004",
      SyntaxError::InvalidParamOrder(..) =>                           "RE-0005",
      SyntaxError::MissingFunctionBody =>                             "RE-0006",
      SyntaxError::UnclosedFunctionBody =>                            "RE-0007",
      SyntaxError::InvalidParameter(_) =>                             "RE-0008",
      SyntaxError::DuplicateParameter(_) =>                           "RE-0009",
      SyntaxError::UnclosedStringLiteral =>                           "RE-0010",
      SyntaxError::MultipleVariadicParams =>                          "RE-0011",
      
      // Access path errors (0020 - 0029)
      SyntaxError::MissingIdentifier =>                               "RE-0020",
      SyntaxError::InvalidIdentifier(_) =>                            "RE-0021",
      SyntaxError::LocalPathStartsWithIndex =>                        "RE-0022",
      
      // Hint/sink errors (0030 - 0039)
      SyntaxError::InvalidSink | SyntaxError::InvalidSinkOn(_) => "RE-0030",
      SyntaxError::InvalidHint | SyntaxError::InvalidHintOn(_) => "RE-0031",
    }
  }
  
  /// Gets a message describing the error.
  pub fn message(&self) -> String {
    match self {
      SyntaxError::UnclosedBlock => "unclosed block; expected '}'".to_owned(),
      SyntaxError::UnclosedFunctionCall => "unclosed function call; expected ']'".to_owned(),
      SyntaxError::UnclosedStringLiteral => "unclosed string literal".to_owned(),
      SyntaxError::ExpectedToken(token) => format!("expected token: '{}'", token),
      SyntaxError::UnexpectedToken(token) => format!("unexpected token: '{}'", token),
      SyntaxError::MissingIdentifier => "missing identifier".to_owned(),
      SyntaxError::LocalPathStartsWithIndex => "variable access path cannot start with an index".to_owned(),
      SyntaxError::InvalidSinkOn(elname) => format!("sink is not valid on {}", elname),
      SyntaxError::InvalidHintOn(elname) => format!("hint is not valid on {}", elname),
      SyntaxError::InvalidSink => "sink is not valid".to_owned(),
      SyntaxError::InvalidHint => "hint is not valid".to_owned(),
      SyntaxError::InvalidIdentifier(idname) => format!("'{}' is not a valid identifier; identifiers may only contain alphanumeric characters, underscores, and hyphens and must contain at least one non-digit", idname),
      SyntaxError::UnclosedFunctionSignature => "unclosed function signature; expected ']' followed by body block".to_owned(),
      SyntaxError::InvalidParamOrder(first, second) => format!("{} is not allowed after {}", second, first),
      SyntaxError::MissingFunctionBody => "missing body in function definition".to_owned(),
      SyntaxError::UnclosedFunctionBody => "unclosed function body; expected '}'".to_owned(),
      SyntaxError::InvalidParameter(pname) => format!("invalid parameter '{}'; must be a valid identifier or '*'", pname),
      SyntaxError::DuplicateParameter(pname) => format!("duplicate parameter '{}' in function signature", pname),
      SyntaxError::MultipleVariadicParams => "multiple variadic parameters are not allowed".to_owned(),
    }
  }
  
  /// Gets a message, usually a brief suggestion, for directly annotating the code causing the error.
  pub fn inline_message(&self) -> String {
    match self {
      SyntaxError::UnclosedBlock => "no matching '}' found".to_owned(),
      SyntaxError::UnclosedStringLiteral => "string literal needs closing delimiter".to_owned(),
      SyntaxError::ExpectedToken(token) => format!("expected '{}'", token),
      SyntaxError::UnexpectedToken(_) => "this probably shouldn't be here".to_owned(),
      SyntaxError::MissingIdentifier => "missing identifier".to_owned(),
      SyntaxError::LocalPathStartsWithIndex => "use a variable name here".to_owned(),
      SyntaxError::InvalidSink | SyntaxError::InvalidSinkOn(_) => "sink not allowed here".to_owned(),
      SyntaxError::InvalidHint | SyntaxError::InvalidHintOn(_) => "hint not allowed here".to_owned(),
      SyntaxError::InvalidIdentifier(_) => "invalid identifier".to_owned(),
      SyntaxError::UnclosedFunctionCall => "no matching ']' found".to_owned(),
      SyntaxError::UnclosedFunctionSignature => "no matching ']' found".to_owned(),
      SyntaxError::InvalidParamOrder(_, second) => format!("{} is not valid in this position", second),
      SyntaxError::MissingFunctionBody => "function body should follow".to_owned(),
      SyntaxError::UnclosedFunctionBody => "no matching '}' found".to_owned(),
      SyntaxError::InvalidParameter(_) => "invalid parameter".to_owned(),
      SyntaxError::DuplicateParameter(_) => "rename parameter to something unique".to_owned(),
      SyntaxError::MultipleVariadicParams => "remove extra variadic parameter".to_owned(),
    }
  }
}

impl Display for SyntaxError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.message())
  }
}