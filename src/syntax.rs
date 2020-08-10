//! # Syntax module
//! The `syntax` module contains Rant's AST implementation and supporting data structures.

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

/// Component in a variable accessor chain
#[derive(Debug, Clone)]
pub enum VarAccessComponent {
  /// Name of variable or map item
  Name(Identifier),
  /// List index
  Index(i64)
}

/// An accessor consisting of a chain of variable names and indices
#[derive(Debug, Clone)]
pub struct VarAccessPath(Vec<VarAccessComponent>);

impl VarAccessPath {
  pub fn new(parts: Vec<VarAccessComponent>) -> Self {
    Self(parts)
  }
}

/// A series of Rant program elements.
#[derive(Debug)]
pub struct Sequence(Vec<Rc<RST>>);

impl Sequence {
  pub fn new(seq: Vec<Rc<RST>>) -> Self {
    Self(seq)
  }
  
  pub fn one(rst: RST) -> Self {
    Self(vec![Rc::new(rst)])
  }
  
  pub fn empty() -> Self {
    Self::new(vec![])
  }
}

impl Deref for Sequence {
  type Target = Vec<Rc<RST>>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for Sequence {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

/// A block is a set of zero or more distinct Rant code snippets.
#[derive(Debug)]
pub struct Block {
  pub flag: PrintFlag,
  pub elements: Vec<Rc<Sequence>>
}

impl Block {
  pub fn new(flag: PrintFlag, elements: Vec<Rc<Sequence>>) -> Self {
    Block {
      flag,
      elements
    }
  }
  
  #[inline]
  pub fn len(&self) -> usize {
    self.elements.len()
  }
}

/// Describes the type of a function parameter.
#[derive(Debug)]
pub enum ParameterType {
  /// Always required
  Required,
  /// May be omitted in favor of a default value
  Optional,
  /// Optional series of zero or more values; defaults to empty list
  VariadicStar,
  /// Required series of one or more values
  VariadicPlus
}

/// Describes a function parameter.
#[derive(Debug)]
pub struct Parameter {
  /// The name of the parameter
  pub name: Identifier,
  /// The type of the parameter
  pub kind: ParameterType
}

/// Describes a function call.
#[derive(Debug)]
pub struct FunctionCall {
  pub flag: PrintFlag,
  pub id: VarAccessPath,
  pub arguments: Vec<RST>
}

/// Describes a function definition.
#[derive(Debug)]
pub struct FunctionDef {
  pub id: VarAccessPath,
  pub params: Vec<Parameter>,
  pub capture_vars: Rc<Vec<Identifier>>,
  pub body: Rc<Block>,
}

/// Describes a boxing (closure) operation to turn a block into a function.
#[derive(Debug)]
pub struct FunctionBox {
  pub flag: PrintFlag,
  pub params: Vec<RantString>,
  pub is_variadic: bool,
  pub body: Rc<Block>
}

#[derive(Debug)]
pub struct AnonFunctionCall {
  pub flag: PrintFlag,
  pub expr: Rc<RST>,
  pub args: Vec<RST>
}

/// Rant Syntax Tree
#[derive(Debug)]
pub enum RST {
  Sequence(Rc<Sequence>),
  Block(Block),
  List(Vec<RST>),
  Map(Vec<(RST, RST)>),
  Box(FunctionBox),
  AnonFunctionCall(AnonFunctionCall),
  FunctionCall(FunctionCall),
  FunctionDef(FunctionDef),
  VarDef(VarAccessPath, Option<Rc<RST>>),
  VarGet(VarAccessPath),
  VarSet(VarAccessPath, Rc<RST>),
  Fragment(RantString),
  Whitespace(RantString),
  Integer(i64),
  Float(f64),
  Boolean(bool),
  Nop
}

impl RST {
  pub fn display_name(&self) -> &'static str {
    match self {
      RST::Sequence(_) =>                     "sequence",
      RST::Block(..) =>                       "block",
      RST::List(_) =>                         "list",
      RST::Map(_) =>                          "map",
      RST::Box { .. } =>                      "box",
      RST::AnonFunctionCall { .. } =>         "anonymous function call",
      RST::FunctionCall { .. } =>             "function call",
      RST::FunctionDef { .. } =>              "function definition",
      RST::Fragment(_) =>                     "fragment",
      RST::Whitespace(_) =>                   "whitespace",
      RST::Integer(_) =>                      "integer",
      RST::Float(_) =>                        "float",
      RST::Boolean(_) =>                      "boolean",
      RST::Nop =>                             "nothing",
      RST::VarDef(..) =>                      "variable definition",
      RST::VarGet(_) =>                       "variable",
      RST::VarSet(..) =>                      "variable assignment"
    }
  }
  
  pub fn is_printing(&self) -> bool {
    matches!(self, 
      RST::Block(Block { flag: PrintFlag::Hint, .. }) |
      RST::AnonFunctionCall(AnonFunctionCall { flag: PrintFlag::Hint, .. }) |
      RST::FunctionCall(FunctionCall { flag: PrintFlag::Hint, .. }) |
      RST::Integer(_) |
      RST::Float(_) |
      RST::Boolean(_) |
      RST::Fragment(_) |
      RST::Whitespace(_) |
      RST::VarGet(_)
    )
  }
}

impl Display for RST {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display_name())
  }
}