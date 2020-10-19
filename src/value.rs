use crate::{lang::{Block, Parameter, Sequence}, lang::Identifier, RantVar};
use crate::collections::*;
use crate::resolver::*;
use crate::runtime::*;
use crate::util::*;
use crate::{IntoRuntimeResult, RuntimeResult, RuntimeError, RuntimeErrorType, stdlib::RantStdResult};
use std::{fmt::{Display, Debug}, rc::Rc, ops::{Add, Not, Sub, Neg, Mul, Div, Rem}, cell::RefCell};
use std::error::Error;
use std::mem;
use std::cmp::Ordering;
use cast::*;

/// Adds a barebones `Error` implementation to the specified type.
macro_rules! impl_error_default {
  ($t:ty) => {
    impl Error for $t {
      fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
      }
    
      fn cause(&self) -> Option<&dyn Error> {
        self.source()
      }
    }
  }
}

/// The result type used by Rant value operators and conversion.
pub type ValueResult<T> = Result<T, ValueError>;
/// The result type used by Rant value index read operations.
pub type ValueIndexResult = Result<RantValue, IndexError>;
/// The result type used by Rant value key read operations.
pub type ValueKeyResult = Result<RantValue, KeyError>;
/// The result type used by Rant value index write operations.
pub type ValueIndexSetResult = Result<(), IndexError>;
/// The result type used by Rant value key write operations.
pub type ValueKeySetResult = Result<(), KeyError>;

/// A reference-counting handle to a Rant function.
pub type RantFunctionRef = Rc<RantFunction>;

/// Rant's "empty" value.
pub struct RantEmpty;

/// A dynamically-typed Rant value.
///
/// ## Cloning
///
/// It is important to note that calling `clone()` on a `RantValue` will only result in a shallow clone of the data.
/// Since collection types like `list` and `map` are represented by handles to their actual contents, cloning these will
/// only make copies of these handles; both copies will still point to the same data.
#[derive(Clone)]
pub enum RantValue {
  /// A Rant value of type `string`. Passed by-value.
  String(String),
  /// A Rant value of type `float`. Passed by-value.
  Float(f64),
  /// A Rant value of type `int`. Passed by-value.
  Int(i64),
  /// A Rant value of type `bool`. Passed by-value.
  Boolean(bool),
  /// A Rant value of type `function`. Passed by-reference.
  Function(RantFunctionRef),
  /// A rant value of type `block`. Passed by-reference.
  Block(Rc<Block>),
  /// A Rant value of type `list`. Passed by-reference.
  List(RantListRef),
  /// A Rant value of type `map`. Passed by-reference.
  Map(RantMapRef),
  /// A Rant value of type `special`. Passed by-value.
  Special(RantSpecial),
  /// A Rant unit value of type `empty`. Passed by-value.
  Empty,
}

impl RantValue {
  /// Returns NaN (Not a Number).
  #[inline]
  pub fn nan() -> Self {
    RantValue::Float(f64::NAN)
  }

  /// Returns true if the value is of type `empty`.
  #[inline]
  pub fn is_empty(&self) -> bool {
    matches!(self, RantValue::Empty)
  }

  /// Returns true if the value is NaN (Not a Number).
  #[inline]
  pub fn is_nan(&self) -> bool {
    if let RantValue::Float(f) = self {
      f64::is_nan(*f)
    } else {
      false
    }
  }

  /// Returns true if the value is callable (e.g. a function).
  #[inline]
  pub fn is_callable(&self) -> bool {
    matches!(self, RantValue::Function(..))
  }
}

#[allow(clippy::len_without_is_empty)]
impl RantValue {
  #[inline]
  pub fn into_rant_int(self) -> RantValue {
    match self {
      RantValue::Int(_) => self,
      RantValue::Float(n) => RantValue::Int(n as i64),
      RantValue::String(s) => {
        match s.parse() {
          Ok(n) => RantValue::Int(n),
          Err(_) => RantValue::Empty,
        }
      },
      RantValue::Boolean(b) => RantValue::Int(bi64(b)),
      _ => RantValue::Empty
    }
  }

  #[inline]
  pub fn into_rant_float(self) -> RantValue {
    match self {
      RantValue::Float(_) => self,
      RantValue::Int(n) => RantValue::Float(n as f64),
      RantValue::String(s) => {
        match s.parse() {
          Ok(n) => RantValue::Float(n),
          Err(_) => RantValue::Empty,
        }
      },
      RantValue::Boolean(b) => RantValue::Float(bf64(b)),
      _ => RantValue::Empty
    }
  }

  #[inline]
  pub fn into_rant_string(self) -> RantValue {
    match self {
      RantValue::String(_) => self,
      _ => RantValue::String(self.to_string())
    }
  }

  #[inline]
  pub fn len(&self) -> usize {
    match self {
      // Length of string is character count
      RantValue::String(s) => s.chars().count(),
      // Length of block is element count
      RantValue::Block(b) => b.elements.len(),
      // Length of list is element count
      RantValue::List(lst) => lst.borrow().len(),
      // Length of map is element count
      RantValue::Map(map) => map.borrow().raw_len(),
      // Treat everything else as length 1, since all other value types are primitives
      _ => 1
    }
  }

  /// Returns a shallow copy of the value.
  #[inline]
  pub fn shallow_copy(&self) -> Self {
    match self {
      RantValue::List(list) => RantValue::List(Rc::new(RefCell::new(list.borrow().clone()))),
      RantValue::Map(map) => RantValue::Map(Rc::new(RefCell::new(map.borrow().clone()))),
      RantValue::Special(special) => RantValue::Special(special.clone()),
      _ => self.clone(),
    }
  }

  /// Gets the Rant type associated with the value.
  #[inline]
  pub fn get_type(&self) -> RantValueType {
    match self {
      RantValue::String(_) =>     RantValueType::String,
      RantValue::Float(_) =>      RantValueType::Float,
      RantValue::Int(_) =>    RantValueType::Int,
      RantValue::Boolean(_) =>    RantValueType::Boolean,
      RantValue::Function(_) =>   RantValueType::Function,
      RantValue::List(_) =>       RantValueType::List,
      RantValue::Block(_) =>      RantValueType::Block,
      RantValue::Map(_) =>        RantValueType::Map,
      RantValue::Special(_) =>    RantValueType::Special,
      RantValue::Empty =>         RantValueType::Empty,
    }
  }
  
  /// Gets the type name of the value.
  #[inline]
  pub fn type_name(&self) -> &'static str {
    self.get_type().name()
  }

  #[inline]
  fn get_uindex(&self, index: i64) -> i64 {
    if index < 0 {
      self.len() as i64 + index
    } else {
      index
    }
  }

  /// Attempts to get a value by index.
  pub fn index_get(&self, index: i64) -> ValueIndexResult {
    let uindex = self.get_uindex(index);

    if uindex < 0 {
      return Err(IndexError::OutOfRange)
    }

    let uindex = uindex as usize;

    match self {
      RantValue::String(s) => {
        if uindex < s.len() {
          Ok(RantValue::String(s[uindex..uindex + 1].to_owned()))
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      RantValue::List(list) => {
        let list = list.borrow();
        if uindex < list.len() {
          Ok(list[uindex].clone())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      _ => Err(IndexError::CannotIndexType(self.get_type()))
    }
  }

  /// Attempts to set a value by index.
  pub fn index_set(&mut self, index: i64, val: RantValue) -> ValueIndexSetResult {
    let uindex = self.get_uindex(index);

    if uindex < 0 {
      return Err(IndexError::OutOfRange)
    }

    let uindex = uindex as usize;

    match self {
      RantValue::List(list) => {
        let mut list = list.borrow_mut();

        if uindex < list.len() {
          list[uindex] = val;
          Ok(())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      RantValue::Map(map) => {
        let mut map = map.borrow_mut();
        map.raw_set(uindex.to_string().as_str(), val);
        Ok(())
      },
      _ => Err(IndexError::CannotSetIndexOnType(self.get_type()))
    }
  }

  /// Attempts to get a value by key.
  pub fn key_get(&self, key: &str) -> ValueKeyResult {
    match self {
      RantValue::Map(map) => {
        let map = map.borrow();
        // TODO: Use prototype getter here
        if let Some(val) = map.raw_get(key) {
          Ok(val.clone())
        } else {
          Err(KeyError::KeyNotFound(key.to_owned()))
        }
      },
      _ => Err(KeyError::CannotKeyType(self.get_type()))
    }
  }

  /// Attempts to set a value by key.
  pub fn key_set(&mut self, key: &str, val: RantValue) -> ValueKeySetResult {
    match self {
      RantValue::Map(map) => {
        let mut map = map.borrow_mut();
        // TODO: use prototype setter here
        map.raw_set(key, val);
        Ok(())
      },
      _ => Err(KeyError::CannotKeyType(self.get_type()))
    }
  }
}

impl Default for RantValue {
  /// Gets the default RantValue (`empty`).
  fn default() -> Self {
    RantValue::Empty
  }
}

/// A lightweight representation of a Rant value's type.
#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum RantValueType {
  /// The `string` type.
  String,
  /// The `float` type.
  Float,
  /// The `int` type.
  Int,
  /// The `bool` type.
  Boolean,
  /// The `function` type.
  Function,
  /// The `block` type.
  Block,
  /// The `list` type.
  List,
  /// The `map` type.
  Map,
  /// The `special` type.
  Special,
  /// The `empty` type.
  Empty
}

impl RantValueType {
  /// Gets a string slice representing the type.
  pub fn name(&self) -> &'static str {
    match self {
      RantValueType::String =>      "string",
      RantValueType::Float =>       "float",
      RantValueType::Int =>         "int",
      RantValueType::Boolean =>     "bool",
      RantValueType::Function =>    "function",
      RantValueType::Block =>       "block",
      RantValueType::List =>        "list",
      RantValueType::Map =>         "map",
      RantValueType::Special =>     "special",
      RantValueType::Empty =>       "empty",
    }
  }
}

impl Display for RantValueType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}

/// Error produced by a RantValue operator or conversion.
#[derive(Debug)]
pub enum ValueError {
  /// The requested conversion was not valid.
  InvalidConversion {
    from: &'static str,
    to: &'static str,
    message: Option<String>,
  },
  /// Attempted to divide by zero.
  DivideByZero,
  /// An arithmetic operation overflowed.
  Overflow,
}

impl_error_default!(ValueError);

impl Display for ValueError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ValueError::InvalidConversion { from, to, message } => {
        if let Some(message) = message {
          write!(f, "unable to convert from {} to {}: {}", from, to, message)
        } else {
          write!(f, "unable to convert from {} to {}", from, to)
        }
      },
      ValueError::DivideByZero => write!(f, "attempted to divide by zero"),
      ValueError::Overflow => write!(f, "arithmetic overflow"),
    }
  }
}

impl<T> IntoRuntimeResult<T> for Result<T, ValueError> {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<T> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::ValueError(err),
      stack_trace: None,
    })
  }
}

/// Error produced by indexing a RantValue.
#[derive(Debug)]
pub enum IndexError {
  /// Index was out of range.
  OutOfRange,
  /// Values of this type cannot be indexed.
  CannotIndexType(RantValueType),
  /// Values of this type cannot have indices written to.
  CannotSetIndexOnType(RantValueType),
}

impl_error_default!(IndexError);

impl Display for IndexError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      IndexError::OutOfRange => write!(f, "value index is out of range"),
      IndexError::CannotIndexType(t) => write!(f, "cannot read index on value of type '{}'", t),
      IndexError::CannotSetIndexOnType(t) => write!(f, "cannot write index on value of type '{}'", t),
    }
  }
}

impl IntoRuntimeResult<RantValue> for ValueIndexResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<RantValue> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::IndexError(err),
      stack_trace: None,
    })
  }
}

impl IntoRuntimeResult<()> for ValueIndexSetResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<()> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::IndexError(err),
      stack_trace: None,
    })
  }
}

/// Error produced by keying a RantValue.
#[derive(Debug)]
pub enum KeyError {
  /// The specified key could not be found.
  KeyNotFound(String),
  /// Values of this type cannot be keyed.
  CannotKeyType(RantValueType),
}

impl_error_default!(KeyError);

impl Display for KeyError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        KeyError::KeyNotFound(k) => write!(f, "key not found: '{}'", k),
        KeyError::CannotKeyType(t) => write!(f, "cannot key value of type '{}'", t),
    }
  }
}

impl IntoRuntimeResult<RantValue> for ValueKeyResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<RantValue> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::KeyError(err),
      stack_trace: None,
    })
  }
}

impl IntoRuntimeResult<()> for ValueKeySetResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<()> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::KeyError(err),
      stack_trace: None,
    })
  }
}

/// Represents "special" values; opaque data structures for various internal runtime uses.
#[derive(Debug, Clone)]
pub enum RantSpecial {
  Selector(SelectorRef),
}

impl PartialEq for RantSpecial {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (RantSpecial::Selector(a), RantSpecial::Selector(b)) => a.as_ptr() == b.as_ptr(),
    }
  }
}

// TODO: Use `RantNumber` to accept any number type in stdlib functions
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub(crate) enum RantNumber {
  Int(i64),
  Float(f64)
}

/// A function callable from Rant.
#[derive(Debug)]
pub struct RantFunction {
  pub(crate) params: Rc<Vec<Parameter>>,
  pub(crate) min_arg_count: usize,
  pub(crate) vararg_start_index: usize,
  pub(crate) captured_vars: Vec<(Identifier, RantVar)>,
  pub(crate) body: RantFunctionInterface,
}

impl RantFunction {
  /// Returns true if the function should be treated as variadic
  #[inline]
  pub fn is_variadic(&self) -> bool {
    self.vararg_start_index < self.params.len() || self.vararg_start_index < self.min_arg_count
  }

  /// Returns true if the function is native
  #[inline]
  pub fn is_native(&self) -> bool {
    matches!(self.body, RantFunctionInterface::Foreign(_))
  }
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunctionInterface {
  /// Represents a foreign function as a wrapper function accepting a variable number of arguments.
  Foreign(Rc<dyn Fn(&mut VM, Vec<RantValue>) -> RantStdResult>),
  /// Represents a user function as an RST.
  User(Rc<Sequence>)
}

impl Debug for RantFunctionInterface {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    unsafe {
      match self {
        RantFunctionInterface::Foreign(func) => write!(f, "{:#016x}", mem::transmute::<_, u128>(Rc::as_ptr(func))),
        RantFunctionInterface::User(func) => write!(f, "{:#016x}", mem::transmute::<_, usize>(Rc::as_ptr(func)))
      }
    }
  }
}

impl Debug for RantValue {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      RantValue::String(s) => write!(f, "{}", s),
      RantValue::Float(n) => write!(f, "{}", n),
      RantValue::Int(n) => write!(f, "{}", n),
      RantValue::Boolean(b) => write!(f, "{}", bstr(*b)),
      RantValue::Function(func) => write!(f, "[function({:?})]", func.body),
      RantValue::Block(block) => write!(f, "[block({})]", block.elements.len()),
      RantValue::List(l) => write!(f, "[list({})]", l.borrow().len()),
      RantValue::Map(m) => write!(f, "[map({})]", m.borrow().raw_len()),
      RantValue::Special(special) => write!(f, "[special({:?})]", special),
      RantValue::Empty => write!(f, "[empty]"),
    }
  }
}

impl Display for RantValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RantValue::String(s) => write!(f, "{}", s),
      RantValue::Int(n) => write!(f, "{}", n),
      RantValue::Float(n) => write!(f, "{}", n),
      RantValue::Boolean(b) => write!(f, "{}", bstr(*b)),
      RantValue::Function(func) => write!(f, "[function({:?})]", func.body),
      RantValue::Block(block) => write!(f, "[block({})]", block.elements.len()),
      RantValue::List(l) => write!(f, "[list({})]", l.borrow().len()),
      RantValue::Map(m) => write!(f, "[map({})]", m.borrow().raw_len()),
      RantValue::Special(_) => write!(f, "[special]"),
      RantValue::Empty => Ok(()),
    }
  }
}

impl PartialEq for RantValue {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (RantValue::Empty, RantValue::Empty) => true,
      (RantValue::String(a), RantValue::String(b)) => a == b,
      (RantValue::Int(a), RantValue::Int(b)) => a == b,
      (RantValue::Int(a), RantValue::Float(b)) => *a as f64 == *b,
      (RantValue::Float(a), RantValue::Float(b)) => a == b,
      (RantValue::Float(a), RantValue::Int(b)) => *a == *b as f64,
      (RantValue::Boolean(a), RantValue::Boolean(b)) => a == b,
      (RantValue::List(a), RantValue::List(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
      (RantValue::Map(a), RantValue::Map(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
      (RantValue::Special(a), RantValue::Special(b)) => a == b,
      _ => false
    }
  }
}

impl Eq for RantValue {}

impl PartialOrd for RantValue {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match (self, other) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => None,
      (RantValue::Int(a), RantValue::Int(b)) => a.partial_cmp(b),
      (RantValue::Float(a), RantValue::Float(b)) => a.partial_cmp(b),
      (RantValue::Float(a), RantValue::Int(b)) => a.partial_cmp(&(*b as f64)),
      (RantValue::Int(a), RantValue::Float(b)) => (&(*a as f64)).partial_cmp(b),
      (RantValue::String(a), RantValue::String(b)) => a.partial_cmp(b),
      (a, b) => if a == b { Some(Ordering::Equal) } else { None }
    }
  }
}

impl Not for RantValue {
  type Output = Self;
  fn not(self) -> Self::Output {
    match self {
      RantValue::Empty => RantValue::Boolean(true),
      RantValue::Boolean(b) => RantValue::Boolean(!b),
      _ => self
    }
  }
}

impl Neg for RantValue {
  type Output = RantValue;
  fn neg(self) -> Self::Output {
    match self {
      RantValue::Int(a) => RantValue::Int(a.saturating_neg()),
      RantValue::Float(a) => RantValue::Float(-a),
      RantValue::Boolean(a) => RantValue::Int(-bi64(a)),
      _ => self
    }
  }
}

impl Add for RantValue {
  type Output = RantValue;
  fn add(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (RantValue::Empty, RantValue::Empty) => RantValue::Empty,
      (lhs, RantValue::Empty) => lhs,
      (RantValue::Empty, rhs) => rhs,
      (RantValue::Int(a), RantValue::Int(b)) => RantValue::Int(a.saturating_add(b)),
      (RantValue::Int(a), RantValue::Float(b)) => RantValue::Float(f64(a) + b),
      (RantValue::Int(a), RantValue::Boolean(b)) => RantValue::Int(a.saturating_add(bi64(b))),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a + b),
      (RantValue::Float(a), RantValue::Int(b)) => RantValue::Float(a + f64(b)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a + bf64(b)),
      (RantValue::String(a), RantValue::String(b)) => RantValue::String(format!("{}{}", a, b)),
      (RantValue::String(a), rhs) => RantValue::String(format!("{}{}", a, rhs)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Int(bi64(a) + bi64(b)),
      (RantValue::Boolean(a), RantValue::Int(b)) => RantValue::Int(bi64(a).saturating_add(b)),
      (RantValue::Boolean(a), RantValue::Float(b)) => RantValue::Float(bf64(a) + b),
      (RantValue::List(a), RantValue::List(b)) => RantValue::List(Rc::new(RefCell::new(a.borrow().iter().cloned().chain(b.borrow().iter().cloned()).collect()))),
      (lhs, rhs) => RantValue::String(format!("{}{}", lhs, rhs))
    }
  }
}

impl Sub for RantValue {
  type Output = RantValue;
  fn sub(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (RantValue::Empty, RantValue::Empty) => RantValue::Empty,
      (lhs, RantValue::Empty) => lhs,
      (RantValue::Empty, rhs) => -rhs,
      (RantValue::Int(a), RantValue::Int(b)) => RantValue::Int(a.saturating_sub(b)),
      (RantValue::Int(a), RantValue::Float(b)) => RantValue::Float((a as f64) - b),
      (RantValue::Int(a), RantValue::Boolean(b)) => RantValue::Int(a - bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a - b),
      (RantValue::Float(a), RantValue::Int(b)) => RantValue::Float(a - (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a - bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Int(bi64(a) - bi64(b)),
      (RantValue::Boolean(a), RantValue::Int(b)) => RantValue::Int(bi64(a).saturating_sub(b)),
      (RantValue::Boolean(a), RantValue::Float(b)) => RantValue::Float(bf64(a) - b),
      _ => RantValue::nan()
    }
  }
}

impl Mul for RantValue {
  type Output = RantValue;
  fn mul(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => RantValue::Empty,
      (RantValue::Int(a), RantValue::Int(b)) => RantValue::Int(a.saturating_mul(b)),
      (RantValue::Int(a), RantValue::Float(b)) => RantValue::Float((a as f64) * b),
      (RantValue::Int(a), RantValue::Boolean(b)) => RantValue::Int(a * bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a * b),
      (RantValue::Float(a), RantValue::Int(b)) => RantValue::Float(a * (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a * bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Int(bi64(a) * bi64(b)),
      (RantValue::Boolean(a), RantValue::Int(b)) => RantValue::Int(bi64(a) * b),
      (RantValue::Boolean(a), RantValue::Float(b)) => RantValue::Float(bf64(a) * b),
      (RantValue::String(a), RantValue::Int(b)) => RantValue::String(a.as_str().repeat(clamp(b, 0, i64::MAX) as usize)),
      _ => RantValue::nan()
    }
  }
}

impl Div for RantValue {
  type Output = ValueResult<RantValue>;
  fn div(self, rhs: Self) -> Self::Output {
    Ok(match (self, rhs) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => RantValue::Empty,
      (_, RantValue::Int(0)) | (_, RantValue::Boolean(false)) => return Err(ValueError::DivideByZero),
      (RantValue::Int(a), RantValue::Int(b)) => RantValue::Int(a / b),
      (RantValue::Int(a), RantValue::Float(b)) => RantValue::Float((a as f64) / b),
      (RantValue::Int(a), RantValue::Boolean(b)) => RantValue::Int(a / bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a / b),
      (RantValue::Float(a), RantValue::Int(b)) => RantValue::Float(a / (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a / bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Int(bi64(a) / bi64(b)),
      (RantValue::Boolean(a), RantValue::Int(b)) => RantValue::Int(bi64(a) / b),
      (RantValue::Boolean(a), RantValue::Float(b)) => RantValue::Float(bf64(a) / b),
      _ => RantValue::nan()
    })
  }
}

impl Rem for RantValue {
  type Output = ValueResult<RantValue>;
  fn rem(self, rhs: Self) -> Self::Output {
    Ok(match (self, rhs) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => RantValue::Empty,
      (_, RantValue::Int(0)) | (_, RantValue::Boolean(false)) => return Err(ValueError::DivideByZero),
      (RantValue::Int(a), RantValue::Int(b)) => RantValue::Int(a % b),
      (RantValue::Int(a), RantValue::Float(b)) => RantValue::Float((a as f64) % b),
      (RantValue::Int(a), RantValue::Boolean(b)) => RantValue::Int(a % bi64(b)),
      _ => RantValue::nan()
    })
  }
}

impl RantValue {
  /// Raises `lhs` to the `rhs` power.
  #[inline]
  pub fn pow(self, rhs: RantValue) -> ValueResult<Self> {
    match (self, rhs) {
      (RantValue::Int(lhs), RantValue::Int(rhs)) => {
        if rhs >= 0 {
          cast::u32(rhs)
            .map_err(|_| ValueError::Overflow)
            .and_then(|rhs| 
              lhs
              .checked_pow(rhs)
              .ok_or(ValueError::Overflow)
            )
            .map(RantValue::Int)
        } else {
          Ok(RantValue::Float((lhs as f64).powf(rhs as f64)))
        }
      },
      (RantValue::Int(lhs), RantValue::Float(rhs)) => {
        Ok(RantValue::Float((lhs as f64).powf(rhs)))
      },
      (RantValue::Float(lhs), RantValue::Int(rhs)) => {
        Ok(RantValue::Float(lhs.powf(rhs as f64)))
      },
      (RantValue::Float(lhs), RantValue::Float(rhs)) => {
        Ok(RantValue::Float(lhs.powf(rhs)))
      },
      _ => Ok(RantValue::Empty)
    }
  }

  #[inline]
  pub fn abs(self) -> ValueResult<Self> {
    match self {
      RantValue::Int(i) => i.checked_abs().map(RantValue::Int).ok_or(ValueError::Overflow),
      RantValue::Float(f) => Ok(RantValue::Float(f.abs())),
      _ => Ok(self)
    }
  }
}