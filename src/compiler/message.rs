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
  NothingToPipe,
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
  FileSystemError(String),
  DynamicDepth,
  InvalidDepthUsage,
  DepthAssignment,
  FallibleOptionalArgAccess(String),
}

macro_rules! rmsg {
  ($msg:literal) => {
    $msg.to_string()
  };
  ($format:literal, $($args:expr),+) => {
    format!($format, $($args,)+)
  };
}

impl Problem {
  fn code(&self) -> &'static str {
    /// Formats Rant error code constants.
    macro_rules! rcode {
      ($code:literal) => {
        concat!("R", stringify!($code))
      }
    }

    match self {
      // Syntax errors (0000 - 0500)
      // Tokens
      Self::UnexpectedToken(_) =>                               rcode!(0000),
      Self::ExpectedToken(_) =>                                 rcode!(0001),

      // Unclosed
      Self::UnclosedBlock =>                                    rcode!(0002),
      Self::UnclosedFunctionCall =>                             rcode!(0003),
      Self::UnclosedFunctionSignature =>                        rcode!(0004),
      Self::UnclosedVariableAccess =>                           rcode!(0005),
      Self::UnclosedStringLiteral =>                            rcode!(0006),
      Self::UnclosedList =>                                     rcode!(0007),
      Self::UnclosedMap =>                                      rcode!(0008),

      // Functions
      Self::InvalidParamOrder(..) =>                            rcode!(0009),
      Self::MissingFunctionBody =>                              rcode!(0010),
      Self::UnclosedFunctionBody =>                             rcode!(0011),
      Self::InvalidParameter(_) =>                              rcode!(0012),
      Self::DuplicateParameter(_) =>                            rcode!(0013),
      Self::MultipleVariadicParams =>                           rcode!(0014),
      
      // Blocks
      Self::DynamicKeyBlockMultiElement =>                      rcode!(0016),
      Self::FunctionBodyBlockMultiElement =>                    rcode!(0017),

      // Accessors
      Self::AnonValueAssignment =>                              rcode!(0018),
      Self::MissingIdentifier =>                                rcode!(0019),
      Self::InvalidIdentifier(_) =>                             rcode!(0020),
      Self::AccessPathStartsWithIndex =>                        rcode!(0021),
      Self::AccessPathStartsWithSlice =>                        rcode!(0022),
      Self::InvalidSliceBound(_) =>                             rcode!(0023),
      Self::NothingToPipe =>                                 rcode!(0024),
      Self::DynamicDepth =>                                     rcode!(0025),
      Self::DepthAssignment =>                                  rcode!(0026),
      Self::InvalidDepthUsage =>                                rcode!(0027),
      
      // Static analysis errors (0100 - 0199)
      Self::ConstantReassignment(_) =>                          rcode!(0100),
      Self::ConstantRedefinition(_) =>                          rcode!(0101),

      // Hint/sink errors (0130 - 0139)
      Self::InvalidSink | Self::InvalidSinkOn(_) =>             rcode!(0130),
      Self::InvalidHint | Self::InvalidHintOn(_) =>             rcode!(0131),

      // Keywords (0200 - 0249)
      Self::InvalidKeyword(_) =>                                rcode!(0200),
      Self::WeightNotAllowed =>                                 rcode!(0201),
      
      // Common warnings (1000 - 1099)
      Self::UnusedVariable(_) =>                                rcode!(1000),
      Self::UnusedParameter(_) =>                               rcode!(1001),
      Self::UnusedFunction(_) =>                                rcode!(1002),
      Self::EmptyFunctionBody(_) =>                             rcode!(1003),
      Self::NestedFunctionDefMarkedConstant =>                  rcode!(1004),
      Self::FallibleOptionalArgAccess(_) =>                     rcode!(1005),
      
      // File access errors (0100 - 0109)
      Self::FileNotFound(_) =>                                  rcode!(2100),
      Self::FileSystemError(_) =>                               rcode!(2101),
    }
  }

  fn message(&self) -> String {
    match self {
      Self::UnclosedBlock => rmsg!("unclosed block; expected '}'"),
      Self::UnclosedFunctionCall => rmsg!("unclosed function call; expected ']'"),
      Self::UnclosedStringLiteral => rmsg!("unclosed string literal"),
      Self::ExpectedToken(token) => rmsg!("expected token: '{}'", token),
      Self::UnexpectedToken(token) => rmsg!("unexpected token: '{}'", token),
      Self::MissingIdentifier => rmsg!("identifier required but is missing"),
      Self::AccessPathStartsWithIndex => rmsg!("access paths cannot start with an index; consider using a variable or anonymous value here instead"),
      Self::AccessPathStartsWithSlice => rmsg!("access paths cannot start with a slice; consider using a variable or anonymous value here instead"),
      Self::InvalidSliceBound(token) => rmsg!("invalid slice bound: '{}'", token),
      Self::InvalidSinkOn(elname) => rmsg!("sink is not valid on {}", elname),
      Self::InvalidHintOn(elname) => rmsg!("hint is not valid on {}", elname),
      Self::InvalidSink => rmsg!("sink is not valid"),
      Self::InvalidHint => rmsg!("hint is not valid"),
      Self::InvalidIdentifier(idname) => rmsg!("'{}' is not a valid identifier; identifiers may only use alphanumerics, underscores, and hyphens but must also contain at least one non-digit", idname),
      Self::UnclosedFunctionSignature => rmsg!("unclosed function signature; expected ']' followed by body block"),
      Self::InvalidParamOrder(first, second) => rmsg!("{} is not allowed after {}", second, first),
      Self::MissingFunctionBody => rmsg!("missing body in function definition"),
      Self::UnclosedFunctionBody => rmsg!("unclosed function body; expected '}}'"),
      Self::InvalidParameter(pname) => rmsg!("invalid parameter '{}'; must be a valid identifier or '*'", pname),
      Self::DuplicateParameter(pname) => rmsg!("duplicate parameter '{}' in function signature", pname),
      Self::MultipleVariadicParams => rmsg!("multiple variadic parameters are not allowed"),
      Self::UnusedVariable(vname) => rmsg!("variable '{}' is defined but never used", vname),
      Self::UnusedParameter(pname) => rmsg!("parameter '{}' is never used", pname),
      Self::UnusedFunction(fname) => rmsg!("function '{}' is defined but never used", fname),
      Self::EmptyFunctionBody(fname) => rmsg!("function '{}' is empty", fname),
      Self::NestedFunctionDefMarkedConstant => rmsg!("nested function definition can't be made constant; function will be mutable"),
      Self::FileNotFound(file) => rmsg!("file not found: '{}'", file),
      Self::FileSystemError(err) => rmsg!("filesystem error: {}", err),
      Self::UnclosedVariableAccess => rmsg!("unclosed accessor; expected '>'"),
      Self::UnclosedList => rmsg!("unclosed list initializer; expected ')'"),
      Self::UnclosedMap => rmsg!("unclosed map initializer; expected ')'"),
      Self::DynamicKeyBlockMultiElement => rmsg!("dynamic key blocks can't have more than one element; if branching is desired, create an inner block"),
      Self::FunctionBodyBlockMultiElement => rmsg!("function body blocks can't have more than one element; if branching is desired, create an inner block"),
      Self::AnonValueAssignment => rmsg!("can't assign directly to anonymous value; try assigning to a key or index instead"),
      Self::NothingToPipe => rmsg!("no pipe value is available in this scope"),
      Self::ConstantReassignment(cname) => rmsg!("reassignment of known constant '{}'", cname),
      Self::ConstantRedefinition(cname) => rmsg!("redefinition of known constant '{}'", cname),
      Self::InvalidKeyword(kw) => rmsg!("invalid keyword: '@{}'", kw),
      Self::WeightNotAllowed => rmsg!("@weight is not allowed in this context"),
      Self::DynamicDepth => rmsg!("depth operator cannot be used on dynamic variable names"),
      Self::DepthAssignment => rmsg!("variable depth cannot be assigned to"),
      Self::InvalidDepthUsage => rmsg!("depth operator is not valid in this context"),
      Self::FallibleOptionalArgAccess(argname) => rmsg!("access to optional argument '{}' can fail; consider adding a fallback to the accessor or specifying a default argument", argname),
    }
  }
  
  fn inline_message(&self) -> Option<String> {
    Some(match self {
      Self::UnclosedBlock => rmsg!("no matching '}}' found"),
      Self::UnclosedStringLiteral => rmsg!("string literal needs closing delimiter"),
      Self::ExpectedToken(token) => rmsg!("expected '{}'", token),
      Self::MissingIdentifier => rmsg!("missing identifier"),
      Self::AccessPathStartsWithIndex => rmsg!("nothing to index"),
      Self::AccessPathStartsWithSlice => rmsg!("nothing to slice"),
      Self::InvalidSink | Self::InvalidSinkOn(_) => rmsg!("sink not allowed here"),
      Self::InvalidHint | Self::InvalidHintOn(_) => rmsg!("hint not allowed here"),
      Self::InvalidIdentifier(_) => rmsg!("invalid identifier"),
      Self::UnclosedFunctionCall => rmsg!("no matching ']' found"),
      Self::UnclosedFunctionSignature => rmsg!("no matching ']' found"),
      Self::InvalidParamOrder(_, second) => rmsg!("{} is not valid in this position", second),
      Self::MissingFunctionBody => rmsg!("function body should follow"),
      Self::UnclosedFunctionBody => rmsg!("no matching '}}' found"),
      Self::InvalidParameter(_) => rmsg!("invalid parameter"),
      Self::DuplicateParameter(_) => rmsg!("rename parameter to something unique"),
      Self::MultipleVariadicParams => rmsg!("remove extra variadic parameter"),
      Self::UnclosedVariableAccess => rmsg!("no matching '>' found"),
      Self::UnclosedList => rmsg!("no matching ')' found"),
      Self::UnclosedMap => rmsg!("no matching ')' found"),
      Self::FunctionBodyBlockMultiElement => rmsg!("multiple elements not allowed here"),
      Self::DynamicKeyBlockMultiElement => rmsg!("multiple elements not allowed here"),
      Self::AnonValueAssignment => rmsg!("direct assignment impossible"),
      Self::NothingToPipe => rmsg!("no previous output to consume"),
      Self::NestedFunctionDefMarkedConstant => rmsg!("use '$' here instead"),
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