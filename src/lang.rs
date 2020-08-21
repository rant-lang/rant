//! The `lang` module contains Rant's syntax tree implementation and supporting data structures.

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

impl Display for Identifier {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// Component in a variable accessor chain
#[derive(Debug)]
pub enum VarAccessComponent {
  /// Name of variable or map item
  Name(Identifier),
  /// List index
  Index(i64),
  /// Dynamic key
  Expression(Rc<Block>),
}

/// An accessor consisting of a chain of variable names and indices
#[derive(Debug)]
pub struct VarAccessPath(Vec<VarAccessComponent>);

impl VarAccessPath {
  pub fn new(parts: Vec<VarAccessComponent>) -> Self {
    Self(parts)
  }

  pub fn dynamic_keys(&self) -> Vec<Rc<Block>> {
    self.iter().filter_map(|c| {
      match c {
        VarAccessComponent::Expression(expr) => Some(Rc::clone(expr)),
        _ => None
      }
    }).collect()
  }
}

impl Deref for VarAccessPath {
  type Target = Vec<VarAccessComponent>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for VarAccessPath {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0   
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

/// Describes a function call.
#[derive(Debug)]
pub struct FunctionCall {
  pub flag: PrintFlag,
  pub id: VarAccessPath,
  pub arguments: Vec<Rc<Sequence>>,
}

/// Describes a function definition.
#[derive(Debug)]
pub struct FunctionDef {
  pub id: VarAccessPath,
  pub params: Vec<Parameter>,
  pub capture_vars: Rc<Vec<Identifier>>,
  pub body: Block,
}

/// Describes a boxing (closure) operation to turn a block into a function.
#[derive(Debug)]
pub struct ClosureExpr {
  pub expr: Block,
  pub params: Vec<Parameter>,
  pub capture_vars: Rc<Vec<Identifier>>,
}

/// Describes an anonymous (nameless) function call.
#[derive(Debug)]
pub struct AnonFunctionCall {
  pub flag: PrintFlag,
  pub expr: Rc<Sequence>,
  pub args: Vec<Rc<Sequence>>,
}

/// Key creation methods for map initializer entries.
#[derive(Debug)]
pub enum MapKeyExpr {
  /// Map key is evaluated from an expression at runtime.
  Dynamic(Block),
  /// Map key is evaluated at compile time from an identifier.
  Static(RantString),
}

/// Rant Syntax Tree
#[derive(Debug)]
pub enum RST {
  /// No Operation
  Nop,
  /// Program sequence
  Sequence(Rc<Sequence>),
  /// Rant block containing zero or more sequences
  Block(Block),
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
  VarDef(Identifier, Option<Rc<Sequence>>),
  /// Value getter
  VarGet(Rc<VarAccessPath>),
  /// Value setter
  VarSet(Rc<VarAccessPath>, Rc<Sequence>),
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
  DebugInfoUpdateOuter(DebugInfo),
  /// Provides debug information about the containing element
  DebugInfoUpdateInner(DebugInfo),
}

impl RST {
  pub fn display_name(&self) -> &'static str {
    match self {
      RST::Sequence(_) =>                     "sequence",
      RST::Block(..) =>                       "block",
      RST::ListInit(_) =>                     "list",
      RST::MapInit(_) =>                      "map",
      RST::Closure(_) =>                      "closure",
      RST::AnonFuncCall(_) =>                 "anonymous function call",
      RST::FuncCall(_) =>                     "function call",
      RST::FuncDef(_) =>                      "function definition",
      RST::Fragment(_) =>                     "fragment",
      RST::Whitespace(_) =>                   "whitespace",
      RST::Integer(_) =>                      "integer",
      RST::Float(_) =>                        "float",
      RST::Boolean(_) =>                      "boolean",
      RST::EmptyVal =>                        "empty",
      RST::Nop =>                             "nothing",
      RST::VarDef(..) =>                      "variable definition",
      RST::VarGet(_) =>                       "variable",
      RST::VarSet(..) =>                      "variable assignment",
      _ =>                                    "???"
    }
  }
  
  pub fn is_printing(&self) -> bool {
    matches!(self, 
      RST::Block(Block { flag: PrintFlag::Hint, .. }) |
      RST::AnonFuncCall(AnonFunctionCall { flag: PrintFlag::Hint, .. }) |
      RST::FuncCall(FunctionCall { flag: PrintFlag::Hint, .. }) |
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

/// Provides debug information about a program element.
#[derive(Debug)]
pub enum DebugInfo {
  Location { line: usize, col: usize },
  SourceName(RantString),
  ScopeName(RantString),
}