use crate::{lang::{Block, Parameter}};
use crate::runtime::*;
use crate::{collections::*, util::*, IntoRuntimeResult, RuntimeResult, RuntimeError, RuntimeErrorType, stdlib::RantStdResult};
use std::{fmt::{Display, Debug}, rc::Rc, ops::{Add, Not, Sub, Neg, Mul, Div, Rem}, cmp, cell::RefCell};
use std::mem;
use cast::*;

pub type ValueResult<T> = Result<T, ValueError>;
pub type ValueIndexResult = Result<RantValue, IndexError>;
pub type ValueKeyResult = Result<RantValue, KeyError>;
pub type ValueIndexSetResult = Result<(), IndexError>;
pub type ValueKeySetResult = Result<(), KeyError>;

/// Rant variable value.
#[derive(Clone)]
pub enum RantValue {
  String(String),
  Float(f64),
  Integer(i64),
  Boolean(bool),
  Function(Rc<RantFunction>),
  List(RantListRef),
  Map(RantMapRef),
  Empty,
}

/// Error produced by a RantValue operator or conversion.
#[derive(Debug)]
pub enum ValueError {
  /// A conversion between two value types failed.
  InvalidConversion {
    from: &'static str,
    to: &'static str,
    message: Option<String>,
  },
  /// Attempted to divide by zero.
  DivideByZero,
}

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
    }
  }
}

impl<T> IntoRuntimeResult<T> for Result<T, ValueError> {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<T> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::ValueError(err),
    })
  }
}

/// Error produced by indexing a RantValue.
#[derive(Debug)]
pub enum IndexError {
  OutOfRange,
  CannotIndexType(&'static str),
  CannotSetIndexOnType(&'static str),
}

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
    })
  }
}

impl IntoRuntimeResult<()> for ValueIndexSetResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<()> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::IndexError(err),
    })
  }
}

/// Error produced by keying a RantValue.
#[derive(Debug)]
pub enum KeyError {
  KeyNotFound(String),
  CannotKeyType(&'static str),
}

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
    })
  }
}

impl IntoRuntimeResult<()> for ValueKeySetResult {
  #[inline]
  fn into_runtime_result(self) -> RuntimeResult<()> {
    self.map_err(|err| RuntimeError {
      description: err.to_string(),
      error_type: RuntimeErrorType::KeyError(err),
    })
  }
}

impl RantValue {
  pub fn nan() -> Self {
    RantValue::Float(f64::NAN)
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    matches!(self, RantValue::Empty)
  }
}

impl Default for RantValue {
  fn default() -> Self {
    RantValue::Empty
  }
}

/// A function callable from Rant.
#[derive(Debug)]
pub struct RantFunction {
  pub(crate) min_arg_count: usize,
  pub(crate) vararg_start_index: usize,
  pub(crate) body: RantFunctionInterface,
  pub(crate) params: Rc<Vec<Parameter>>,
  pub(crate) captured_vars: Option<RantMap>,
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
  User(Rc<Block>)
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
      RantValue::Integer(n) => write!(f, "{}", n),
      RantValue::Boolean(b) => write!(f, "{}", bstr(*b)),
      RantValue::Function(func) => write!(f, "[function({:?})]", func.body),
      RantValue::List(l) => write!(f, "[list({})]", l.borrow().len()),
      RantValue::Map(m) => write!(f, "[map({})]", m.borrow().raw_len()),
      RantValue::Empty => write!(f, "<>"),
    }
  }
}

impl Display for RantValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RantValue::String(s) => write!(f, "{}", s),
      RantValue::Integer(n) => write!(f, "{}", n),
      RantValue::Float(n) => write!(f, "{}", n),
      RantValue::Boolean(b) => write!(f, "{}", bstr(*b)),
      RantValue::Function(func) => write!(f, "[function({:?})]", func.body),
      RantValue::List(l) => write!(f, "[list({})]", l.borrow().len()),
      RantValue::Map(m) => write!(f, "[map({})]", m.borrow().raw_len()),
      RantValue::Empty => Ok(())
    }
  }
}

impl PartialEq for RantValue {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (RantValue::Empty, RantValue::Empty) => true,
      (RantValue::String(a), RantValue::String(b)) => a == b,
      (RantValue::Integer(a), RantValue::Integer(b)) => a == b,
      (RantValue::Integer(a), RantValue::Float(b)) => *a as f64 == *b,
      (RantValue::Float(a), RantValue::Float(b)) => a == b,
      (RantValue::Float(a), RantValue::Integer(b)) => *a == *b as f64,
      (RantValue::Boolean(a), RantValue::Boolean(b)) => a == b,
      (RantValue::List(a), RantValue::List(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
      (RantValue::Map(a), RantValue::Map(b)) => Rc::as_ptr(a) == Rc::as_ptr(b),
      _ => false
    }
  }
}

impl Eq for RantValue {}

impl PartialOrd for RantValue {
  fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
    match (self, other) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => None,
      (RantValue::Integer(a), RantValue::Integer(b)) => a.partial_cmp(b),
      (RantValue::Float(a), RantValue::Float(b)) => a.partial_cmp(b),
      (RantValue::Float(a), RantValue::Integer(b)) => a.partial_cmp(&(*b as f64)),
      (RantValue::Integer(a), RantValue::Float(b)) => (&(*a as f64)).partial_cmp(b),
      (RantValue::String(a), RantValue::String(b)) => a.partial_cmp(b),
      (_, _) => None
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
      RantValue::Integer(a) => RantValue::Integer(a.saturating_neg()),
      RantValue::Float(a) => RantValue::Float(-a),
      RantValue::Boolean(a) => RantValue::Integer(-bi64(a)),
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
      (RantValue::Integer(a), RantValue::Integer(b)) => RantValue::Integer(a.saturating_add(b)),
      (RantValue::Integer(a), RantValue::Float(b)) => RantValue::Float(f64(a) + b),
      (RantValue::Integer(a), RantValue::Boolean(b)) => RantValue::Integer(a.saturating_add(bi64(b))),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a + b),
      (RantValue::Float(a), RantValue::Integer(b)) => RantValue::Float(a + f64(b)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a + bf64(b)),
      (RantValue::String(a), RantValue::String(b)) => RantValue::String(format!("{}{}", a, b)),
      (RantValue::String(a), rhs) => RantValue::String(format!("{}{}", a, rhs)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Integer(bi64(a) + bi64(b)),
      (RantValue::Boolean(a), RantValue::Integer(b)) => RantValue::Integer(bi64(a).saturating_add(b)),
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
      (RantValue::Integer(a), RantValue::Integer(b)) => RantValue::Integer(a.saturating_sub(b)),
      (RantValue::Integer(a), RantValue::Float(b)) => RantValue::Float((a as f64) - b),
      (RantValue::Integer(a), RantValue::Boolean(b)) => RantValue::Integer(a - bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a - b),
      (RantValue::Float(a), RantValue::Integer(b)) => RantValue::Float(a - (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a - bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Integer(bi64(a) - bi64(b)),
      (RantValue::Boolean(a), RantValue::Integer(b)) => RantValue::Integer(bi64(a).saturating_sub(b)),
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
      (RantValue::Integer(a), RantValue::Integer(b)) => RantValue::Integer(a.saturating_mul(b)),
      (RantValue::Integer(a), RantValue::Float(b)) => RantValue::Float((a as f64) * b),
      (RantValue::Integer(a), RantValue::Boolean(b)) => RantValue::Integer(a * bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a * b),
      (RantValue::Float(a), RantValue::Integer(b)) => RantValue::Float(a * (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a * bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Integer(bi64(a) * bi64(b)),
      (RantValue::Boolean(a), RantValue::Integer(b)) => RantValue::Integer(bi64(a) * b),
      (RantValue::Boolean(a), RantValue::Float(b)) => RantValue::Float(bf64(a) * b),
      (RantValue::String(a), RantValue::Integer(b)) => RantValue::String(a.as_str().repeat(clamp(b, 0, i64::MAX) as usize)),
      _ => RantValue::nan()
    }
  }
}

impl Div for RantValue {
  type Output = ValueResult<RantValue>;
  fn div(self, rhs: Self) -> Self::Output {
    Ok(match (self, rhs) {
      (RantValue::Empty, _) | (_, RantValue::Empty) => RantValue::Empty,
      (_, RantValue::Integer(0)) | (_, RantValue::Boolean(false)) => return Err(ValueError::DivideByZero),
      (RantValue::Integer(a), RantValue::Integer(b)) => RantValue::Integer(a / b),
      (RantValue::Integer(a), RantValue::Float(b)) => RantValue::Float((a as f64) / b),
      (RantValue::Integer(a), RantValue::Boolean(b)) => RantValue::Integer(a / bi64(b)),
      (RantValue::Float(a), RantValue::Float(b)) => RantValue::Float(a / b),
      (RantValue::Float(a), RantValue::Integer(b)) => RantValue::Float(a / (b as f64)),
      (RantValue::Float(a), RantValue::Boolean(b)) => RantValue::Float(a / bf64(b)),
      (RantValue::Boolean(a), RantValue::Boolean(b)) => RantValue::Integer(bi64(a) / bi64(b)),
      (RantValue::Boolean(a), RantValue::Integer(b)) => RantValue::Integer(bi64(a) / b),
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
      (_, RantValue::Integer(0)) | (_, RantValue::Boolean(false)) => return Err(ValueError::DivideByZero),
      (RantValue::Integer(a), RantValue::Integer(b)) => RantValue::Integer(a % b),
      (RantValue::Integer(a), RantValue::Float(b)) => RantValue::Float((a as f64) % b),
      (RantValue::Integer(a), RantValue::Boolean(b)) => RantValue::Integer(a % bi64(b)),
      _ => RantValue::nan()
    })
  }
}

#[allow(clippy::len_without_is_empty)]
impl RantValue {
  pub fn into_rant_int(self) -> RantValue {
    match self {
      RantValue::Integer(_) => self,
      RantValue::Float(n) => RantValue::Integer(n as i64),
      RantValue::String(s) => {
        match s.parse() {
          Ok(n) => RantValue::Integer(n),
          Err(_) => RantValue::Empty,
        }
      },
      RantValue::Boolean(b) => RantValue::Integer(bi64(b)),
      _ => RantValue::Empty
    }
  }

  pub fn into_rant_float(self) -> RantValue {
    match self {
      RantValue::Float(_) => self,
      RantValue::Integer(n) => RantValue::Float(n as f64),
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

  pub fn into_rant_string(self) -> RantValue {
    match self {
      RantValue::String(_) => self,
      _ => RantValue::String(self.to_string())
    }
  }

  pub fn len(&self) -> usize {
    match self {
      // Length of string is character count
      RantValue::String(s) => s.chars().count(),
      // Length of list is element count
      RantValue::List(lst) => lst.borrow().len(),
      // Length of map is element count
      RantValue::Map(map) => map.borrow().raw_len(),
      _ => 0
    }
  }
  
  /// Gets the type name of the value.
  pub fn type_name(&self) -> &'static str {
    match self {
      RantValue::String(_) =>     "string",
      RantValue::Float(_) =>      "float",
      RantValue::Integer(_) =>    "integer",
      RantValue::Boolean(_) =>    "bool",
      RantValue::Function(_) =>   "function",
      RantValue::List(_) =>       "list",
      RantValue::Map(_) =>        "map",
      RantValue::Empty =>         "empty"
    }
  }

  pub fn get_by_index(&self, index: i64) -> ValueIndexResult {
    if index < 0 {
      return Err(IndexError::OutOfRange)
    }
    let index = index as usize;

    match self {
        RantValue::String(s) => {
          if index < s.len() {
            Ok(RantValue::String(s[index..index + 1].to_owned()))
          } else {
            Err(IndexError::OutOfRange)
          }
        },
        RantValue::List(list) => {
          let list = list.borrow();
          if index < list.len() {
            Ok(list[index].clone())
          } else {
            Err(IndexError::OutOfRange)
          }
        },
        _ => Err(IndexError::CannotIndexType(self.type_name()))
    }
  }

  pub fn set_by_index(&mut self, index: i64, val: RantValue) -> ValueIndexSetResult {
    if index < 0 {
      return Err(IndexError::OutOfRange)
    }

    let index = index as usize;

    match self {
      RantValue::List(list) => {
        let mut list = list.borrow_mut();

        if index < list.len() {
          list[index] = val;
          Ok(())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      RantValue::Map(map) => {
        let mut map = map.borrow_mut();
        map.raw_set(index.to_string().as_str(), val);
        Ok(())
      },
      _ => Err(IndexError::CannotSetIndexOnType(self.type_name()))
    }
  }

  pub fn get_by_key(&self, key: &str) -> ValueKeyResult {
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
      _ => Err(KeyError::CannotKeyType(self.type_name()))
    }
  }

  pub fn set_by_key(&mut self, key: &str, val: RantValue) -> ValueKeySetResult {
    match self {
      RantValue::Map(map) => {
        let mut map = map.borrow_mut();
        // TODO: use prototype setter here
        map.raw_set(key, val);
        Ok(())
      },
      _ => Err(KeyError::CannotKeyType(self.type_name()))
    }
  }
}