use crate::{runtime::VM, lang::{Block, RST, Parameter}, RantResult};
use crate::{collections::*, util::*, ToRant};
use std::{fmt::{Display, Debug}, rc::Rc, ops::{Add, Not, Sub, Neg}, cmp, cell::RefCell};
use std::mem;
use cast::*;

pub type ValueIndexResult = Result<RantValue, ValueIndexError>;
pub type ValueKeyResult = Result<RantValue, ValueKeyError>;
pub type ValueIndexSetResult = Result<(), ValueIndexError>;
pub type ValueKeySetResult = Result<(), ValueKeyError>;

/// Rant variable value.
#[derive(Clone)]
pub enum RantValue {
  String(String),
  Float(f64),
  Integer(i64),
  Boolean(bool),
  Function(Rc<RantFunction>),
  List(Rc<RefCell<RantList>>),
  Map(Rc<RefCell<RantMap>>),
  Empty,
}

#[derive(Debug)]
pub enum ValueIndexError {
  OutOfRange,
  CannotIndexType(&'static str),
  CannotSetIndexOnType(&'static str),
}

#[derive(Debug)]
pub enum ValueKeyError {
  KeyNotFound,
  CannotKeyType(&'static str),
}

impl RantValue {
  pub fn nan() -> Self {
    RantValue::Float(f64::NAN)
  }

  #[inline]
  pub fn is_none(&self) -> bool {
    matches!(self, RantValue::Empty)
  }
}

impl Default for RantValue {
  fn default() -> Self {
    RantValue::Empty
  }
}

/// Semantic wrapper around a Vec<T> for use in variadic argument sets.
pub(crate) struct VarArgs<T>(Vec<T>);

impl<T> VarArgs<T> {
  pub fn new(args: Vec<T>) -> Self {
    Self(args)
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
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunctionInterface {
  /// Represents a foreign function as a wrapper function accepting a variable number of arguments.
  Foreign(Rc<dyn Fn(&mut VM, Vec<RantValue>) -> RantResult<()>>),
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
      (RantValue::List(a), RantValue::List(b)) => RantValue::List(Rc::new(RefCell::new(RantList::from_iter(a.borrow().iter().cloned().chain(b.borrow().iter().cloned()))))),
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

#[allow(clippy::len_without_is_empty)]
impl RantValue {
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
      return Err(ValueIndexError::OutOfRange)
    }
    let index = index as usize;

    match self {
        RantValue::String(s) => {
          if index < s.len() {
            Ok(RantValue::String(s[index..index + 1].to_owned()))
          } else {
            Err(ValueIndexError::OutOfRange)
          }
        },
        RantValue::List(list) => {
          let list = list.borrow();
          if index < list.len() {
            Ok(list[index].clone())
          } else {
            Err(ValueIndexError::OutOfRange)
          }
        },
        _ => Err(ValueIndexError::CannotIndexType(self.type_name()))
    }
  }

  pub fn set_by_index(&mut self, index: i64, val: RantValue) -> ValueIndexSetResult {
    if index < 0 {
      return Err(ValueIndexError::OutOfRange)
    }

    let index = index as usize;

    match self {
      RantValue::List(list) => {
        let mut list = list.borrow_mut();

        if index < list.len() {
          list[index] = val;
          Ok(())
        } else {
          Err(ValueIndexError::OutOfRange)
        }
      },
      RantValue::Map(map) => {
        let mut map = map.borrow_mut();
        map.raw_set(index.to_string().as_str(), val);
        Ok(())
      },
      _ => Err(ValueIndexError::CannotSetIndexOnType(self.type_name()))
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
          Err(ValueKeyError::KeyNotFound)
        }
      },
      _ => Err(ValueKeyError::CannotKeyType(self.type_name()))
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
      _ => Err(ValueKeyError::CannotKeyType(self.type_name()))
    }
  }
}