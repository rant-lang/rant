//! Contains Rant's syntax tree implementation and supporting data structures.

use std::{ops::{DerefMut, Deref}, fmt::Display, rc::Rc};
use crate::RantString;

/// Printflags indicate to the compiler whether a given program element is likely to print something or not.
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrintFlag {
  /// Use default printing behavior.
  None,
  /// Treat the marked element as printing.
  Hint,
  /// Treat the marked element as non-printing.
  Sink
}

impl PrintFlag {
  #[inline]
  pub fn prioritize(prev: PrintFlag, next: PrintFlag) -> PrintFlag {
    match next {
      PrintFlag::None => prev,
      _ => next,
    }
  }

  #[inline]
  pub fn is_sink(&self) -> bool {
    matches!(self, PrintFlag::Sink)
  }
}

/// Identifiers are special strings used to name variables and static (non-procedural) map keys.
/// This is just a wrapper around a SmartString that enforces identifier formatting requirements.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier(RantString);

impl Identifier {
  pub fn new(idstr: RantString) -> Self {
    Self(idstr)
  }
}

impl Deref for Identifier {
  type Target = RantString;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl Display for Identifier {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// Checks if an identifier (variable name, arg name, static map key) is valid
pub(crate) fn is_valid_ident(name: &str) -> bool {
  if name.is_empty() { return false }
  let mut has_non_digit = false;
  let is_valid_chars = name.chars().all(|c| {
    has_non_digit |= !c.is_ascii_digit();
    c.is_alphanumeric() || matches!(c, '_' | '-')
  });
  has_non_digit && is_valid_chars
}

/// Component in an accessor path.
#[derive(Debug)]
pub enum AccessPathComponent {
  /// Name of variable or map item
  Name(Identifier),
  /// List index
  Index(i64),
  /// Dynamic key
  Expression(Rc<Sequence>),
}

impl Display for AccessPathComponent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AccessPathComponent::Name(name) => write!(f, "{}", name),
      AccessPathComponent::Index(i) => write!(f, "{}", i),
      AccessPathComponent::Expression(expr) => write!(f, "{{{}...}}", expr.name().map(|name| name.as_str()).unwrap_or("")),
    }
  }
}

/// Types of access paths.
#[derive(Debug, Copy, Clone)]
pub enum AccessPathKind {
  /// Path points to a variable in the current scope.
  Local,
  /// Path points explicitly to a global variable.
  ExplicitGlobal,
  /// Path points explicitly to a variable that is at most _n_ scopes above the current scope.
  Descope(usize),
}

impl AccessPathKind {
  /// Gets the number of explicit descopes required by this path. If the path is explicitly global, returns 0.
  pub fn descope_count(&self) -> usize {
    match self {
      AccessPathKind::Local | AccessPathKind::ExplicitGlobal => 0,
      AccessPathKind::Descope(n) => *n
    }
  }
}

/// Describes the location of a value.
#[derive(Debug)]
pub struct AccessPath {
  path: Vec<AccessPathComponent>,
  kind: AccessPathKind,
}

impl AccessPath {
  #[inline]
  pub fn new(path: Vec<AccessPathComponent>, kind: AccessPathKind) -> Self {
    Self {
      path,
      kind
    }
  }

  #[inline]
  pub fn is_explicit_global(&self) -> bool {
    matches!(self.kind, AccessPathKind::ExplicitGlobal)
  }

  #[inline]
  pub fn kind(&self) -> AccessPathKind {
    self.kind
  }

  /// Returns a list of dynamic keys used by the path in order.
  #[inline]
  pub fn dynamic_keys(&self) -> Vec<Rc<Sequence>> {
    self.iter().filter_map(|c| {
      match c {
        AccessPathComponent::Expression(expr) => Some(Rc::clone(expr)),
        _ => None
      }
    }).collect()
  }

  /// If the path statically accesses a variable, returns the name of the variable accessed; otherwise, returns `None`.
  #[inline]
  pub fn capture_var_name(&self) -> Option<Identifier> {
    if let Some(AccessPathComponent::Name(id)) = self.first() {
      Some(id.clone())
    } else {
      None
    }
  }
}

impl Deref for AccessPath {
  type Target = Vec<AccessPathComponent>;
  fn deref(&self) -> &Self::Target {
    &self.path
  }
}

impl DerefMut for AccessPath {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.path
  }
}

impl Display for AccessPath {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.iter().map(|part| part.to_string()).collect::<Vec<String>>().join("/"))
  }
}

/// A series of Rant program elements.
#[derive(Debug)]
pub struct Sequence {
  elements: Vec<Rc<Rst>>,
  name: Option<RantString>,
}

impl Sequence {
  pub fn new(seq: Vec<Rc<Rst>>) -> Self {
    Self {
      elements: seq,
      name: None,
    }
  }
  
  pub fn one(rst: Rst) -> Self {
    Self {
      elements: vec![Rc::new(rst)],
      name: None,
    }
  }
  
  pub fn empty() -> Self {
    Self::new(vec![])
  }

  #[inline(always)]
  pub fn with_name(mut self, name: RantString) -> Self {
    self.name = Some(name);
    self
  }

  #[inline(always)]
  pub fn with_name_str(mut self, name: &str) -> Self {
    self.name = Some(RantString::from(name));
    self
  }

  pub fn name(&self) -> Option<&RantString> {
    self.name.as_ref()
  }
}

impl Deref for Sequence {
  type Target = Vec<Rc<Rst>>;
  fn deref(&self) -> &Self::Target {
    &self.elements
  }
}

impl DerefMut for Sequence {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.elements
  }
}

/// A block is a set of zero or more distinct Rant code snippets.
#[derive(Debug)]
pub struct Block {
  pub flag: PrintFlag,
  pub elements: Rc<Vec<Rc<Sequence>>>
}

impl Block {
  pub fn new(flag: PrintFlag, elements: Vec<Rc<Sequence>>) -> Self {
    Block {
      flag,
      elements: Rc::new(elements)
    }
  }
  
  #[inline]
  pub fn len(&self) -> usize {
    self.elements.len()
  }
}

/// Describes the arity requirements of a function parameter.
#[derive(Debug, Copy, Clone)]
pub enum Varity {
  /// Single-value, always required
  Required,
  /// Single-value, may be omitted in favor of a default value
  Optional,
  /// Optional series of zero or more values; defaults to empty list
  VariadicStar,
  /// Required series of one or more values
  VariadicPlus,
}

impl Varity {
  /// Returns true if the supplied varity pair is in a valid order.
  pub fn is_valid_order(first: Varity, second: Varity) -> bool {
    use Varity::*;
    matches!((first, second), 
      (Required, Required) |
      (Required, Optional) |
      (Required, VariadicStar) |
      (Required, VariadicPlus) |
      (Optional, Optional) |
      (Optional, VariadicStar)
    )
  }

  /// Returns true if the varity is variadic.
  pub fn is_variadic(&self) -> bool {
    use Varity::*;
    matches!(self, VariadicStar | VariadicPlus)
  }
}

impl Display for Varity {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use Varity::*;
    match self {
      Required => write!(f, "required parameter"),
      Optional => write!(f, "optional parameter"),
      VariadicStar => write!(f, "optional variadic parameter"),
      VariadicPlus => write!(f, "required variadic parameter"),
    }
  }
}

/// Describes a function parameter.
#[derive(Debug)]
pub struct Parameter {
  /// The name of the parameter
  pub name: Identifier,
  /// The varity of the parameter
  pub varity: Varity,
}

impl Parameter {
  /// Returns true if the parameter is required.
  #[inline]
  pub fn is_required(&self) -> bool {
    use Varity::*;
    matches!(self.varity, Required | VariadicPlus)
  }
}

/// Describes a function call.
#[derive(Debug)]
pub struct FunctionCall {
  pub flag: PrintFlag,
  pub id: Rc<AccessPath>,
  pub arguments: Rc<Vec<Rc<Sequence>>>,
}

/// Describes a function definition.
#[derive(Debug)]
pub struct FunctionDef {
  pub id: Rc<AccessPath>,
  pub params: Rc<Vec<Parameter>>,
  pub capture_vars: Rc<Vec<Identifier>>,
  pub body: Rc<Sequence>,
}

/// Describes a boxing (closure) operation to turn a block into a function.
#[derive(Debug)]
pub struct ClosureExpr {
  pub expr: Rc<Sequence>,
  pub params: Rc<Vec<Parameter>>,
  pub capture_vars: Rc<Vec<Identifier>>,
}

/// Describes an anonymous (nameless) function call.
#[derive(Debug)]
pub struct AnonFunctionCall {
  pub flag: PrintFlag,
  pub expr: Rc<Sequence>,
  pub args: Rc<Vec<Rc<Sequence>>>,
}

/// Key creation methods for map initializer entries.
#[derive(Debug)]
pub enum MapKeyExpr {
  /// Map key is evaluated from an expression at runtime.
  Dynamic(Rc<Sequence>),
  /// Map key is evaluated at compile time from an identifier.
  Static(RantString),
}

/// Rant Syntax Tree
#[derive(Debug)]
pub enum Rst {
  /// No Operation
  Nop,
  /// Program sequence
  Sequence(Rc<Sequence>),
  /// Rant block containing zero or more sequences
  Block(Block),
  /// Block as value
  BlockValue(Rc<Block>),
  /// List initializer
  ListInit(Rc<Vec<Rc<Sequence>>>),
  /// Map initializer
  MapInit(Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>),
  /// Closure expression
  Closure(ClosureExpr),
  /// Anonymous function call
  AnonFuncCall(AnonFunctionCall),
  /// Named function call
  FuncCall(FunctionCall),
  /// Function definition
  FuncDef(FunctionDef),
  /// Variable definition
  VarDef(Identifier, AccessPathKind, Option<Rc<Sequence>>),
  /// Value getter
  VarGet(Rc<AccessPath>),
  /// Value setter
  VarSet(Rc<AccessPath>, Rc<Sequence>),
  /// Fragment
  Fragment(RantString),
  /// Whitespace
  Whitespace(RantString),
  /// Integer value
  Integer(i64),
  /// Floating-point value
  Float(f64),
  /// Boolean value
  Boolean(bool),
  /// Empty value
  EmptyVal,
  /// Provides debug information about the next sequence element
  DebugCursor(DebugInfo),
}

impl Rst {
  pub fn display_name(&self) -> &'static str {
    match self {
      Rst::Sequence(_) =>                     "sequence",
      Rst::Block(..) =>                       "block",
      Rst::ListInit(_) =>                     "list",
      Rst::MapInit(_) =>                      "map",
      Rst::Closure(_) =>                      "closure",
      Rst::AnonFuncCall(_) =>                 "anonymous function call",
      Rst::FuncCall(_) =>                     "function call",
      Rst::FuncDef(_) =>                      "function definition",
      Rst::Fragment(_) =>                     "fragment",
      Rst::Whitespace(_) =>                   "whitespace",
      Rst::Integer(_) =>                      "integer",
      Rst::Float(_) =>                        "float",
      Rst::Boolean(_) =>                      "boolean",
      Rst::EmptyVal =>                        "empty",
      Rst::Nop =>                             "nothing",
      Rst::VarDef(..) =>                      "variable definition",
      Rst::VarGet(_) =>                       "variable",
      Rst::VarSet(..) =>                      "variable assignment",
      _ =>                                    "???"
    }
  }
  
  pub fn is_printing(&self) -> bool {
    matches!(self, 
      Rst::Block(Block { flag: PrintFlag::Hint, .. }) |
      Rst::AnonFuncCall(AnonFunctionCall { flag: PrintFlag::Hint, .. }) |
      Rst::FuncCall(FunctionCall { flag: PrintFlag::Hint, .. }) |
      Rst::Integer(_) |
      Rst::Float(_) |
      Rst::Boolean(_) |
      Rst::Fragment(_) |
      Rst::Whitespace(_) |
      Rst::VarGet(_)
    )
  }
}

impl Display for Rst {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display_name())
  }
}

/// Provides debug information about a program element.
#[derive(Debug)]
pub enum DebugInfo {
  Location { line: usize, col: usize },
  SourceName(RantString),
  ScopeName(RantString),
}