//! Contains Rant's syntax tree implementation and supporting data structures.

use std::{collections::HashMap, fmt::Display, ops::{DerefMut, Deref}, rc::Rc};
use crate::{RantProgramInfo, InternalString, RantValue, RantValueType};

pub(crate) const COMPOSE_VALUE_NAME: &str = "~COMPOSE";

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

  /// Returns `true` if the flag is a sink.
  #[inline]
  pub fn is_sink(&self) -> bool {
    matches!(self, PrintFlag::Sink)
  }
}

/// Identifiers are special strings used to name variables and static (non-procedural) map keys.
/// This is just a wrapper around a SmartString that enforces identifier formatting requirements.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Identifier(InternalString);

impl Identifier {
  pub fn new(idstr: InternalString) -> Self {
    Self(idstr)
  }
}

impl From<&'static str> for Identifier {
  fn from(s: &'static str) -> Self {
    Self::new(InternalString::from(s))
  }
}

impl std::borrow::Borrow<str> for Identifier {
  fn borrow(&self) -> &str {
    self.0.as_str()
  }
}

impl Deref for Identifier {
  type Target = InternalString;
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

/// A single bound index for a slice expression.
#[derive(Debug)]
pub enum SliceIndex {
  /// Static index.
  Static(i64),
  /// Dynamic index.
  Dynamic(Rc<Sequence>)
}

impl Display for SliceIndex {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SliceIndex::Static(i) => write!(f, "{}", i),
      SliceIndex::Dynamic(_expr) => write!(f, "{{...}}"),
    }
  }
}

/// An unevaluated list slice.
#[derive(Debug)]
pub enum SliceExpr {
  /// Unbounded slice.
  Full,
  /// Start-bounded slice.
  From(SliceIndex),
  /// End-bounded slice.
  To(SliceIndex),
  /// Fully-bounded slice.
  Between(SliceIndex, SliceIndex),
}

impl SliceExpr {
  /// Creates a static slice from a dynamic slice, using a callback to retrieve a static index for each dynamic index.
  ///
  /// If any of the dynamic indices evaluate to a non-integer, function returns `Err` with the incompatible type.
  pub(crate) fn as_static_slice<F: FnMut(&Rc<Sequence>) -> RantValue>(&self, mut index_converter: F) -> Result<Slice, RantValueType> {
    macro_rules! convert_index {
      ($index:expr) => {
        match $index {
          SliceIndex::Static(i) => *i,
          SliceIndex::Dynamic(expr) => {
            match index_converter(expr) {
              RantValue::Int(i) => i,
              other => return Err(other.get_type())
            }
          }
        }
      }
    }

    Ok(match self {
      SliceExpr::Full => Slice::Full,
      SliceExpr::From(from) => Slice::From(convert_index!(from)),
      SliceExpr::To(to) => Slice::To(convert_index!(to)),
      SliceExpr::Between(from, to) => Slice::Between(convert_index!(from), convert_index!(to)),
    })
  }
}

impl Display for SliceExpr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SliceExpr::Full => write!(f, ":"),
      SliceExpr::From(i) => write!(f, "{}:", i),
      SliceExpr::To(i) => write!(f, ":{}", i),
      SliceExpr::Between(l, r) => write!(f, "{}:{}", l, r),
    }
  }
}

/// An evaluated list slice.
#[derive(Debug)]
pub enum Slice {
  /// Unbounded slice.
  Full,
  /// Start-bounded slice.
  From(i64),
  /// End-bounded slice.
  To(i64),
  /// Fully-bounded slice.
  Between(i64, i64),
}

/// Component in an accessor path.
#[derive(Debug)]
pub enum AccessPathComponent {
  /// Name of variable or map item
  Name(Identifier),
  /// List index
  Index(i64),
  /// Slice
  Slice(SliceExpr),
  /// Dynamic key
  DynamicKey(Rc<Sequence>),
  /// Anonymous value
  AnonymousValue(Rc<Sequence>),
}

impl Display for AccessPathComponent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AccessPathComponent::Name(name) => write!(f, "{}", name),
      AccessPathComponent::Index(i) => write!(f, "{}", i),
      AccessPathComponent::Slice(slice_expr) => write!(f, "{}", slice_expr),
      AccessPathComponent::DynamicKey(expr) => write!(f, "{{{}...}}", expr.name().map(|name| name.as_str()).unwrap_or("")),
      AccessPathComponent::AnonymousValue(expr) => write!(f, "!{{{}...}}", expr.name().map(|name| name.as_str()).unwrap_or("")),
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

  /// Returns `true` if the access type is `Local`.
  #[inline]
  pub fn is_local(&self) -> bool {
    matches!(self, Self::Local)
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
  pub fn is_anonymous(&self) -> bool {
    matches!(self.first(), Some(AccessPathComponent::AnonymousValue(..)))
  }

  #[inline]
  pub fn is_variable(&self) -> bool {
    self.len() == 1 && matches!(self.first(), Some(AccessPathComponent::Name(..)) | Some(AccessPathComponent::DynamicKey(..)))
  }

  #[inline]
  pub fn kind(&self) -> AccessPathKind {
    self.kind
  }

  /// Returns a list of dynamic keys used by the path in order.
  #[inline]
  pub fn dynamic_exprs(&self) -> Vec<Rc<Sequence>> {
    use AccessPathComponent::*;
    let mut exprs = vec![];
    for component in self.iter() {
      match component {
        DynamicKey(expr) | AnonymousValue(expr) => exprs.push(Rc::clone(expr)),
        Slice(SliceExpr::From(SliceIndex::Dynamic(expr)))
        | Slice(SliceExpr::To(SliceIndex::Dynamic(expr))) 
        | Slice(SliceExpr::Between(SliceIndex::Static(_), SliceIndex::Dynamic(expr)))
        | Slice(SliceExpr::Between(SliceIndex::Dynamic(expr), SliceIndex::Static(_))) => exprs.push(Rc::clone(expr)),
        Slice(SliceExpr::Between(SliceIndex::Dynamic(expr_from), SliceIndex::Dynamic(expr_to))) => {
          exprs.push(Rc::clone(expr_from));
          exprs.push(Rc::clone(expr_to));
        },
        _ => {}
      }
    }
    exprs
  }

  /// If the path statically accesses a variable, returns the name of the variable accessed; otherwise, returns `None`.
  #[inline]
  pub fn var_name(&self) -> Option<Identifier> {
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
  pub name: Option<InternalString>,
  pub origin: Rc<RantProgramInfo>,
}

impl Sequence {
  pub fn new(seq: Vec<Rc<Rst>>, origin: &Rc<RantProgramInfo>) -> Self {
    Self {
      elements: seq,
      name: None,
      origin: Rc::clone(origin),
    }
  }
  
  pub fn one(rst: Rst, origin: &Rc<RantProgramInfo>) -> Self {
    Self {
      elements: vec![Rc::new(rst)],
      name: None,
      origin: Rc::clone(origin),
    }
  }
  
  pub fn empty(origin: &Rc<RantProgramInfo>) -> Self {
    Self::new(vec![], origin)
  }

  #[inline(always)]
  pub fn with_name(mut self, name: InternalString) -> Self {
    self.name = Some(name);
    self
  }

  #[inline(always)]
  pub fn with_name_str(mut self, name: &str) -> Self {
    self.name = Some(InternalString::from(name));
    self
  }

  pub fn name(&self) -> Option<&InternalString> {
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
  pub is_weighted: bool,
  pub elements: Rc<Vec<BlockElement>>
}

impl Block {
  pub fn new(flag: PrintFlag, is_weighted: bool, elements: Vec<BlockElement>) -> Self {
    Block {
      flag,
      is_weighted,
      elements: Rc::new(elements)
    }
  }
  
  #[inline]
  pub fn len(&self) -> usize {
    self.elements.len()
  }
}

/// A single element of a regular block.
#[derive(Debug)]
pub struct BlockElement {
  pub main: Rc<Sequence>,
  pub weight: Option<Rc<Sequence>>,
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

/// Defines spread modes for function arguments.
#[derive(Debug)]
pub enum ArgumentSpreadMode {
  /// Pass as one argument.
  NoSpread,
  /// Iterate over the value and pass each element as a separate argument.
  Parametric,
  /// Iterate over the value and pass in each item as the argument in separate function calls.
  ///
  /// If multiple arguments are temporal, the receiving function is called for each valid combination between them.
  ///
  /// Temporal arguments with matching labels will be iterated simultaneously.
  Temporal { label: usize },
}

/// Describes a function argument expression.
#[derive(Debug)]
pub struct ArgumentExpr {
  /// The expression that produces the argument value.
  pub expr: Rc<Sequence>,
  /// The spread mode for the argument.
  pub spread_mode: ArgumentSpreadMode,
}

/// Describes what to call for a function call.
#[derive(Debug)]
pub enum FunctionCallTarget {
  /// Indicates a path to a function variable.
  /// Used for named function calls.
  Path(Rc<AccessPath>),
  /// Indicates an expression (which returns the function to call).
  /// Used for anonymous function calls.
  Expression(Rc<Sequence>),
}

/// A function call.
#[derive(Debug)]
pub struct FunctionCall {
  /// The primting behavior.
  pub flag: PrintFlag,
  /// The function to call.
  pub target: FunctionCallTarget,
  /// The arguments to pass.
  pub arguments: Rc<Vec<ArgumentExpr>>,
  /// Runtime flag to enable temporal calling.
  pub is_temporal: bool,
}

/// A composed function call.
#[derive(Debug)]
pub struct ComposedFunctionCall {
  pub flag: PrintFlag,
  pub steps: Rc<Vec<FunctionCall>>,
  pub is_temporal: bool,
}

/// Keeps track of combination indices in a temporally-spread function call.
#[derive(Debug)]
pub struct TemporalSpreadState {
  counters: Vec<(usize, usize)>,
  arg_labels: HashMap<usize, usize>,
}

impl TemporalSpreadState {
  #[inline]
  pub fn new(arg_exprs: &[ArgumentExpr], args: &[RantValue]) -> Self {
    let mut counters = Vec::with_capacity(args.len());
    let mut arg_labels: HashMap<usize, usize> = Default::default();
    for (i, expr) in arg_exprs.iter().enumerate() {
      if let ArgumentSpreadMode::Temporal { label } = expr.spread_mode {
        arg_labels.insert(i, label);
        // Since temporal indices are always incremental, we can assume the next label index will only be 1 ahead at most.
        // This way, duplicate labels share the same counter.
        if label >= counters.len() {
          let arg = &args[i];
          let counter_size = if arg.is_indexable() {
            arg.len()
          } else {
            0
          };
          counters.push((0, counter_size));
        } else {
          // If it's an existing index, update the counter size to the minimum argument length.
          // This way, we guarantee that temporal arguments with a shared label *always* provide the same number of values.
          let arg = &args[i];
          if arg.is_indexable() {
            let (_, n) = &mut counters[label];
            *n = arg.len().min(*n);
          }
        }
      }
    }
    Self {
      counters,
      arg_labels,
    }
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.counters.len()
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.counters.is_empty() || self.counters.iter().all(|(.., n)| *n == 0)
  }

  #[inline]
  pub fn get(&self, arg_index: usize) -> Option<usize> {
    self.arg_labels.get(&arg_index).map(|i| self.counters[*i].0)
  }

  /// Increments the temporal counters.
  /// Returns `true` if another function call should be queued.
  #[inline]
  pub fn increment(&mut self) -> bool {
    let mut success = false;
    for (c, n) in self.counters.iter_mut() {
      *c += 1;
      // Check if counter has reached the end
      if c >= n {
        *c = 0;
      } else {
        success = true;
        break
      }
    }
    success
  }
}

/// Describes a function definition.
#[derive(Debug)]
pub struct FunctionDef {
  pub path: Rc<AccessPath>,
  pub is_const: bool, // only used on variable definitions
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

/// Key creation methods for map initializer entries.
#[derive(Debug)]
pub enum MapKeyExpr {
  /// Map key is evaluated from an expression at runtime.
  Dynamic(Rc<Sequence>),
  /// Map key is evaluated at compile time from an identifier.
  Static(InternalString),
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
  /// Single function call
  FuncCall(FunctionCall),
  /// Composed function call
  ComposedCall(ComposedFunctionCall),
  /// Function definition
  FuncDef(FunctionDef),
  /// Variable definition
  VarDef(Identifier, AccessPathKind, Option<Rc<Sequence>>),
  /// Constant definition
  ConstDef(Identifier, AccessPathKind, Option<Rc<Sequence>>),
  /// Value getter
  VarGet(Rc<AccessPath>, Option<Rc<Sequence>>),
  /// Value setter
  VarSet(Rc<AccessPath>, Rc<Sequence>),
  /// Compose value
  ComposeValue,
  /// Fragment
  Fragment(InternalString),
  /// Whitespace
  Whitespace(InternalString),
  /// Integer value
  Integer(i64),
  /// Floating-point value
  Float(f64),
  /// Boolean value
  Boolean(bool),
  /// Empty value
  EmptyVal,
  /// Return
  Return(Option<Rc<Sequence>>),
  /// Continue
  Continue(Option<Rc<Sequence>>),
  /// Break
  Break(Option<Rc<Sequence>>),
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
      Rst::FuncCall(_) =>                     "function call",
      Rst::FuncDef(_) =>                      "function definition",
      Rst::Fragment(_) =>                     "fragment",
      Rst::Whitespace(_) =>                   "whitespace",
      Rst::Integer(_) =>                      "integer",
      Rst::Float(_) =>                        "float",
      Rst::Boolean(_) =>                      "boolean",
      Rst::EmptyVal =>                        "empty",
      Rst::Nop =>                             "no-op",
      Rst::VarDef(..) =>                      "variable definition",
      Rst::ConstDef(..) =>                    "constant definition",
      Rst::VarGet(..) =>                      "getter",
      Rst::VarSet(..) =>                      "setter",
      Rst::BlockValue(_) =>                   "block value",
      Rst::ComposedCall(_) =>                 "composed call",
      Rst::ComposeValue =>                    "compose value",
      Rst::Return(_) =>                       "return",
      Rst::Continue(_) =>                     "continue",
      Rst::Break(_) =>                        "break",
      Rst::DebugCursor(_) =>                  "debug cursor",
    }
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
}