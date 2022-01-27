//! Contains Rant's syntax tree implementation and supporting data structures.

use std::{collections::HashMap, fmt::Display, ops::{Deref, DerefMut}, rc::Rc};
use crate::{RantProgramInfo, InternalString, RantValue, RantValueType};

pub(crate) const PIPE_VALUE_NAME: &str = "~PIPE";

/// Printflags indicate to the compiler whether a given program element is likely to print something or not.
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrintFlag {
  /// Use default printing behavior.
  None,
  /// Treat the marked element as text.
  Hint,
  /// Suppress output from the next element..
  Sink
}

impl PrintFlag {
  /// Determines which of two `PrintFlag` values should take priority.
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
  /// Pipeval
  PipeValue,
}

impl Display for AccessPathComponent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Name(name) => write!(f, "{}", name),
      Self::Index(i) => write!(f, "{}", i),
      Self::Slice(slice_expr) => write!(f, "{}", slice_expr),
      Self::DynamicKey(expr) => write!(f, "{{{}...}}", expr.name().map(|name| name.as_str()).unwrap_or("")),
      Self::AnonymousValue(expr) => write!(f, "!{{{}...}}", expr.name().map(|name| name.as_str()).unwrap_or("")),
      Self::PipeValue => write!(f, "[]"),
    }
  }
}

/// Defines available variable resolution modes for variable access paths.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum VarAccessMode {
  /// Path points to a variable in the current scope.
  Local,
  /// Path points explicitly to a global variable.
  ExplicitGlobal,
  /// Path points explicitly to a variable that is at most _n_ scopes above the current scope.
  Descope(usize),
}

impl VarAccessMode {
  /// Gets the number of explicit descopes required for this mode. If the path is explicitly global, returns 0.
  pub fn descope_count(&self) -> usize {
    match self {
      VarAccessMode::Local | VarAccessMode::ExplicitGlobal => 0,
      VarAccessMode::Descope(n) => *n
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
  mode: VarAccessMode,
}

impl AccessPath {
  #[inline]
  pub fn new(path: Vec<AccessPathComponent>, kind: VarAccessMode) -> Self {
    Self {
      path,
      mode: kind
    }
  }

  /// Determines whether the access path is explicitly accessing a global value.
  #[inline]
  pub fn is_explicit_global(&self) -> bool {
    matches!(self.mode, VarAccessMode::ExplicitGlobal)
  }

  /// Determines whether the root of the access path is an inline value.
  #[inline]
  pub fn is_anonymous(&self) -> bool {
    matches!(self.first(), Some(AccessPathComponent::AnonymousValue(..)))
  }

  /// Determines whether the access path targets *only* a variable (i.e. no child access).
  #[inline]
  pub fn is_variable_target(&self) -> bool {
    self.len() == 1 && matches!(self.first(), Some(AccessPathComponent::Name(..) | AccessPathComponent::DynamicKey(..) | AccessPathComponent::PipeValue))
  }

  /// Gets the kind access path this is.
  #[inline]
  pub fn mode(&self) -> VarAccessMode {
    self.mode
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
    if let Some(first) = self.first() {
      return Some(match first {
        AccessPathComponent::Name(id) => id.clone(),
        AccessPathComponent::PipeValue => Identifier::from(PIPE_VALUE_NAME),
        _ => return None
      })
    }
    None
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

/// A series of Rant expressions to be executed in order.
#[derive(Debug)]
pub struct Sequence {
  elements: Vec<Rc<Expression>>,
  /// An optional name for the sequence.
  pub name: Option<InternalString>,
  /// Information about where the sequence came from, such as its source file.
  pub origin: Rc<RantProgramInfo>,
}

impl Sequence {
  /// Creates a new sequence.
  #[inline]
  pub fn new(seq: Vec<Rc<Expression>>, origin: &Rc<RantProgramInfo>) -> Self {
    Self {
      elements: seq,
      name: None,
      origin: Rc::clone(origin),
    }
  }
  
  /// Creates a new sequence with a single element.
  #[inline]
  pub fn one(rst: Expression, origin: &Rc<RantProgramInfo>) -> Self {
    Self {
      elements: vec![Rc::new(rst)],
      name: None,
      origin: Rc::clone(origin),
    }
  }
  
  /// Creates an empty sequence.
  pub fn empty(origin: &Rc<RantProgramInfo>) -> Self {
    Self::new(vec![], origin)
  }

  /// Creates an empty sequence with the specified name.
  #[inline(always)]
  pub fn with_name(mut self, name: InternalString) -> Self {
    self.name = Some(name);
    self
  }

  /// Creates an empty sequence with the specified name.
  #[inline(always)]
  pub fn with_name_str(mut self, name: &str) -> Self {
    self.name = Some(InternalString::from(name));
    self
  }

  /// Gets the name of the sequence.
  pub fn name(&self) -> Option<&InternalString> {
    self.name.as_ref()
  }
}

impl Deref for Sequence {
  type Target = Vec<Rc<Expression>>;
  fn deref(&self) -> &Self::Target {
    &self.elements
  }
}

impl DerefMut for Sequence {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.elements
  }
}

/// A block is an ordered collection of one or more Rant expressions.
#[derive(Debug)]
pub struct Block {
  /// Determines whether the block uses weights.
  pub is_weighted: bool,
  /// Determines the protection level of the block.
  pub protection: Option<BlockProtection>,
  /// The elements associated with the block.
  pub elements: Rc<Vec<Rc<BlockElement>>>
}

impl Block {
  /// Creates a new block.
  pub fn new(is_weighted: bool, protection: Option<BlockProtection>, elements: Vec<Rc<BlockElement>>) -> Self {
    Block {
      is_weighted,
      protection,
      elements: Rc::new(elements),
    }
  }
  
  /// Gets the number of elements contained in the block.
  #[inline]
  pub fn len(&self) -> usize {
    self.elements.len()
  }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BlockProtection {
  Outer,
}

/// A single element of a regular block.
#[derive(Debug)]
pub struct BlockElement {
  /// The main body of the element.
  pub main: Rc<Sequence>,
  /// The weight of the element.
  pub weight: Option<BlockWeight>,
  /// Output modifier signature associated with the element sequence.
  pub output_modifier: Option<OutputModifierSig>,
}

impl Clone for BlockElement {
  #[inline]
  fn clone(&self) -> Self {
    Self {
      main: Rc::clone(&self.main),
      weight: self.weight.clone(),
      output_modifier: self.output_modifier.clone(),
    }
  }
}

/// A block weight.
#[derive(Debug)]
pub enum BlockWeight {
  /// A weight that is evaluated from an expression.
  Dynamic(Rc<Sequence>),
  /// A weight that is a constant value.
  Constant(f64),
}

impl Clone for BlockWeight {
  #[inline]
  fn clone(&self) -> Self {
    match self {
      BlockWeight::Dynamic(s) => Self::Dynamic(Rc::clone(s)),
      BlockWeight::Constant(c) => Self::Constant(*c),
    }
  }
}

/// Signature information for an output modifier.
#[derive(Debug, Clone)]
pub struct OutputModifierSig {
  pub input_var: Option<Identifier>
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
  /// The function to call.
  pub target: FunctionCallTarget,
  /// The arguments to pass.
  pub arguments: Rc<Vec<ArgumentExpr>>,
  /// Runtime flag to enable temporal calling.
  pub is_temporal: bool,
}

/// A piped function call.
#[derive(Debug)]
pub struct PipedCall {
  /// The function calls in the chain.
  pub steps: Rc<Vec<FunctionCall>>,
  /// Determines whether the call executes temporally.
  pub is_temporal: bool,
}

/// Keeps track of combination indices in a temporally-spread function call.
#[derive(Debug)]
pub struct TemporalSpreadState {
  /// Counters associated with each temporal call in the chain.
  counters: Vec<(usize, usize)>,
  /// Maps argument indices to temporal counter indices.
  arg_labels: HashMap<usize, usize>,
}

impl TemporalSpreadState {
  /// Creates a new `TemporalSpreadState`.
  #[inline]
  pub fn new(arg_exprs: &[ArgumentExpr], args: &[RantValue]) -> Self {
    let mut counters = Vec::with_capacity(args.len());
    let mut arg_labels: HashMap<usize, usize> = Default::default();
    for (i, expr) in arg_exprs.iter().enumerate() {
      if let ArgumentSpreadMode::Temporal { label } = expr.spread_mode {
        arg_labels.insert(i, label);
        // Since temporal indices are always incremental, we can assume the next label index will only be 1 ahead at most.
        // This way, duplicate labels share the same counter.
        let arg = &args[i];
        if label >= counters.len() {
          let counter_size = if arg.is_indexable() {
            arg.len()
          } else {
            0
          };
          counters.push((0, counter_size));
        } else {
          // If it's an existing index, update the counter size to the minimum argument length.
          // This way, we guarantee that temporal arguments with a shared label *always* provide the same number of values.
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

  /// Gets the number of counters in the state.
  #[inline]
  pub fn len(&self) -> usize {
    self.counters.len()
  }

  /// Determines whether there are no counters in the state.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.counters.is_empty() || self.counters.iter().all(|(.., n)| *n == 0)
  }

  /// Gets the current counter value of the specified argument index.
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

/// Describes a Rant function definition.
#[derive(Debug, Clone)]
pub struct FunctionDef {
  /// The path to the function to define.
  pub path: Rc<AccessPath>,
  /// Indicates whether the function will be constant.
  pub is_const: bool, // only used on variable definitions
  /// The parameters associated with the function being defined.
  pub params: Rc<Vec<Parameter>>,
  /// The variables to capture into the function being defined.
  pub capture_vars: Rc<Vec<Identifier>>,
  /// The body of the function being defined.
  pub body: Rc<Sequence>,
}

/// Describes a Rant lambda.
#[derive(Debug, Clone)]
pub struct LambdaExpr {
  /// The body of the lambda. 
  pub body: Rc<Sequence>,
  /// The parameters associated with the lambda.
  pub params: Rc<Vec<Parameter>>,
  /// The variables to capture into the lambda.
  pub capture_vars: Rc<Vec<Identifier>>,
}

/// Describes a function parameter.
#[derive(Debug)]
pub struct Parameter {
  /// The name of the parameter
  pub name: Identifier,
  /// The varity of the parameter
  pub varity: Varity,
  /// The default value of the parameter.
  pub default_value_expr: Option<Rc<Sequence>>,
}

impl Parameter {
  /// Returns true if the parameter is required.
  #[inline]
  pub fn is_required(&self) -> bool {
    use Varity::*;
    matches!(self.varity, Required | VariadicPlus)
  }

  #[inline]
  pub fn is_optional(&self) -> bool {
    use Varity::*;
    matches!(self.varity, Optional)
  }
}

/// Key creation methods for map initializer entries.
#[derive(Debug)]
pub enum MapKeyExpr {
  /// Map key is evaluated from an expression at runtime.
  Dynamic(Rc<Sequence>),
  /// Map key is evaluated at compile time from an identifier.
  Static(InternalString),
}

/// Defines Rant expression tree node types. These are directly executable by the VM.
#[derive(Debug)]
pub enum Expression {
  /// No Operation
  Nop,
  /// Program sequence
  Sequence(Rc<Sequence>),
  /// Rant block containing zero or more sequences
  Block(Rc<Block>),
  /// List initializer
  ListInit(Rc<Vec<Rc<Sequence>>>),
  /// Tuple initializer
  TupleInit(Rc<Vec<Rc<Sequence>>>),
  /// Map initializer
  MapInit(Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>),
  /// Lambda expression
  Lambda(LambdaExpr),
  /// Single function call
  FuncCall(FunctionCall),
  /// Piped function call
  PipedCall(PipedCall),
  /// Function definition
  FuncDef(FunctionDef),
  /// Variable definition
  DefVar(Identifier, VarAccessMode, Option<Rc<Sequence>>),
  /// Constant definition
  DefConst(Identifier, VarAccessMode, Option<Rc<Sequence>>),
  /// Variable depth
  Depth(Identifier, VarAccessMode, Option<Rc<Sequence>>),
  /// Getter
  Get(Rc<AccessPath>, Option<Rc<Sequence>>),
  /// Setter
  Set(Rc<AccessPath>, Rc<Sequence>),
  /// Pipe value
  PipeValue,
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
  EmptyValue,
  /// Return
  Return(Option<Rc<Sequence>>),
  /// Continue
  Continue(Option<Rc<Sequence>>),
  /// Break
  Break(Option<Rc<Sequence>>),
  /// Logical NOT
  LogicNot(Rc<Sequence>),
  /// Negation
  Negate(Rc<Sequence>),
  /// Exponentiation
  Power(Rc<Sequence>, Rc<Sequence>),
  /// Multipliation
  Multiply(Rc<Sequence>, Rc<Sequence>),
  /// Division
  Divide(Rc<Sequence>, Rc<Sequence>),
  /// Modulo
  Modulo(Rc<Sequence>, Rc<Sequence>),
  /// Addition
  Add(Rc<Sequence>, Rc<Sequence>),
  /// Subtraction
  Subtract(Rc<Sequence>, Rc<Sequence>),
  /// Less than
  Less(Rc<Sequence>, Rc<Sequence>),
  /// Less than or equal
  LessOrEqual(Rc<Sequence>, Rc<Sequence>),
  /// Greater than
  Greater(Rc<Sequence>, Rc<Sequence>),
  /// Greater than or equal
  GreaterOrEqual(Rc<Sequence>, Rc<Sequence>),
  /// Equality
  Equals(Rc<Sequence>, Rc<Sequence>),
  /// Inequality
  NotEquals(Rc<Sequence>, Rc<Sequence>),
  /// Logical AND
  LogicAnd(Rc<Sequence>, Rc<Sequence>),
  /// Logical NOR
  LogicNor(Rc<Sequence>, Rc<Sequence>),
  /// Logical XOR
  LogicXor(Rc<Sequence>, Rc<Sequence>),
  /// Logical OR
  LogicOr(Rc<Sequence>, Rc<Sequence>),
  /// Logical NAND
  LogicNand(Rc<Sequence>, Rc<Sequence>),
  /// Conditional branch
  Conditional { conditions: Rc<Vec<(Rc<Sequence>, Rc<Block>)>>, fallback: Option<Rc<Block>> },
  /// Provides debug information about the next sequence element
  DebugCursor(DebugInfo),
  /// Require statement
  Require { alias: Option<InternalString>, path: InternalString },
}

impl Expression {
  /// Gets the diagnostic display name for the node.
  pub fn display_name(&self) -> &'static str {
    match self {
      Self::Sequence(_) =>                     "sequence",
      Self::Block(..) =>                       "block",
      Self::ListInit(_) =>                     "list",
      Self::TupleInit(_) =>                    "tuple",
      Self::MapInit(_) =>                      "map",
      Self::Lambda(_) =>                       "lambda",
      Self::FuncCall(_) =>                     "call function",
      Self::FuncDef(_) =>                      "define function",
      Self::Fragment(_) =>                     "fragment",
      Self::Whitespace(_) =>                   "whitespace",
      Self::Integer(_) =>                      "integer",
      Self::Float(_) =>                        "float",
      Self::Boolean(_) =>                      "bool",
      Self::EmptyValue =>                      "emptyval",
      Self::Nop =>                             "no-op",
      Self::DefVar(..) =>                      "define variable",
      Self::DefConst(..) =>                    "define constant",
      Self::Depth(..) =>                       "variable depth",
      Self::Get(..) =>                         "getter",
      Self::Set(..) =>                         "setter",
      Self::PipedCall(_) =>                    "piped call",
      Self::PipeValue =>                       "pipeval",
      Self::Return(_) =>                       "return",
      Self::Continue(_) =>                     "continue",
      Self::Break(_) =>                        "break",
      Self::LogicNot(_) =>                     "not",
      Self::Negate(_) =>                       "negate",
      Self::Power(_, _) =>                     "power",
      Self::Multiply(_, _) =>                  "multiply",
      Self::Divide(_, _) =>                    "divide",
      Self::Modulo(_, _) =>                    "modulo",
      Self::Add(_, _) =>                       "add",
      Self::Subtract(_, _) =>                  "subtract",
      Self::Less(_, _) =>                      "less than",
      Self::LessOrEqual(_, _) =>               "less than or equal",
      Self::Greater(_, _) =>                   "greater than",
      Self::GreaterOrEqual(_, _) =>            "greater than or equal",
      Self::Equals(_, _) =>                    "equals",
      Self::NotEquals(_, _) =>                 "not equals",
      Self::LogicAnd(_, _) =>                  "and",
      Self::LogicNor(_, _) =>                  "nor",
      Self::LogicXor(_, _) =>                  "xor",
      Self::LogicOr(_, _) =>                   "or",
      Self::LogicNand(_, _) =>                 "nand",
      Self::DebugCursor(_) =>                  "debug cursor",
      Self::Conditional { .. } =>              "conditional",
      Self::Require { .. } =>                  "require",
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.display_name())
  }
}

/// Provides debug information about a program element.
#[derive(Debug)]
pub enum DebugInfo {
  /// Provides source code location information for the following sequence element.
  Location { line: usize, col: usize },
}