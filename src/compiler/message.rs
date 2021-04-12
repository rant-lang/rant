use std::{fmt::Display, ops::Range};

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
  InvalidKeyword(String),
  WeightNotAllowed,
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

      // Charms (0200 - 0249)
      Problem::InvalidKeyword(_) =>                               "R-0200",
      Problem::WeightNotAllowed =>                                "R-0201",

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
      Problem::InvalidKeyword(kw) => format!("invalid keyword: '@{}'", kw),
      Problem::WeightNotAllowed => "@weight is not allowed in this context".to_owned(),
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