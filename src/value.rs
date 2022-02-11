use crate::{RantFunction, RantString, lang::Slice, util};
use crate::{collections::*, TryFromRant, IntoRant, RantSelectorHandle};
use crate::runtime::*;
use crate::util::*;
use std::ops::{Deref};
use std::{fmt::{Display, Debug}, ops::{Add, Div, Mul, Neg, Not, Rem, Sub}, rc::Rc};
use std::error::Error;
use std::cmp::Ordering;
use cast::*;

pub const TYPENAME_STRING: &str = "string";
pub const TYPENAME_INT: &str = "int";
pub const TYPENAME_FLOAT: &str = "float";
pub const TYPENAME_BOOL: &str = "bool";
pub const TYPENAME_TUPLE: &str = "tuple";
pub const TYPENAME_LIST: &str = "list";
pub const TYPENAME_MAP: &str = "map";
pub const TYPENAME_FUNCTION: &str = "function";
pub const TYPENAME_SELECTOR: &str = "selector";
pub const TYPENAME_RANGE: &str = "range";
pub const TYPENAME_NOTHING: &str = "nothing";

const MAX_DISPLAY_STRING_DEPTH: usize = 4;

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

/// Implements `IntoRuntimeResult<T>` for a type.
macro_rules! impl_into_runtime_result {
  ($src_result_type:ty, $ok_type:ty, $err_type_variant:ident) => {
    impl IntoRuntimeResult<$ok_type> for $src_result_type {
      #[inline]
      fn into_runtime_result(self) -> RuntimeResult<$ok_type> {
        self.map_err(|err| RuntimeError {
          error_type: RuntimeErrorType::$err_type_variant(err),
          description: None,
          stack_trace: None,
        })
      }
    }
  };
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
/// The result type used by Rant value slice read operations.
pub type ValueSliceResult = Result<RantValue, SliceError>;
/// The result type used by Rant value slice write operations.
pub type ValueSliceSetResult = Result<(), SliceError>;

/// Type alias for `Rc<RantFunction>`
pub type RantFunctionHandle = Rc<RantFunction>;

/// Rant's "nothing" value.
pub struct RantNothing;

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
  /// The `list` type.
  List,
  /// The `tuple` type.
  Tuple,
  /// The `map` type.
  Map,
  /// The `selector` type.
  Selector,
  /// The `range` type.
  Range,
  /// The `nothing` type.
  Nothing
}

impl RantValueType {
  /// Gets a string slice representing the type.
  pub fn name(&self) -> &'static str {
    match self {
      Self::String =>      TYPENAME_STRING,
      Self::Float =>       TYPENAME_FLOAT,
      Self::Int =>         TYPENAME_INT,
      Self::Boolean =>     TYPENAME_BOOL,
      Self::Function =>    TYPENAME_FUNCTION,
      Self::List =>        TYPENAME_LIST,
      Self::Tuple =>       TYPENAME_TUPLE,
      Self::Map =>         TYPENAME_MAP,
      Self::Selector =>    TYPENAME_SELECTOR,
      Self::Range =>       TYPENAME_RANGE,
      Self::Nothing =>     TYPENAME_NOTHING,
    }
  }
}

impl Display for RantValueType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name())
  }
}

/// A dynamically-typed Rant value.
///
/// ## Cloning
///
/// Calling `clone()` on a by-ref Rant value type (such as `list`) will only clone its handle; both copies will point to the same contents.
/// 
/// If you want to shallow-copy a by-ref value, use the `shallow_copy` method instead.
#[derive(Clone)]
pub enum RantValue {
  /// A Rant value of type `string`. Passed by-value.
  String(RantString),
  /// A Rant value of type `float`. Passed by-value.
  Float(f64),
  /// A Rant value of type `int`. Passed by-value.
  Int(i64),
  /// A Rant value of type `bool`. Passed by-value.
  Boolean(bool),
  /// A Rant value of type `function`. Passed by-reference.
  Function(RantFunctionHandle),
  /// A Rant value of type `list`. Passed by-reference.
  List(RantListHandle),
  /// A Rant value of type `tuple`. Passed by-reference.
  Tuple(RantTupleHandle),
  /// A Rant value of type `map`. Passed by-reference.
  Map(RantMapHandle),
  /// A Rant value of type `range`. Passed by-value.
  Range(RantRange),
  /// A Rant value of type `selector`. Passed by-value.
  Selector(RantSelectorHandle),
  /// A Rant unit value of type `nothing`. Passed by-value.
  Nothing,
}

impl RantValue {
  /// Not a Number (NaN).
  pub const NAN: Self = Self::Float(f64::NAN);
  /// Positive infinity.
  pub const INFINITY: Self = Self::Float(f64::INFINITY);
  /// Negative infinity.
  pub const NEG_INFINITY: Self = Self::Float(f64::NEG_INFINITY);
  /// The lowest possible finite value for the `float` type.
  pub const MIN_FLOAT: Self = Self::Float(f64::MIN);
  /// The highest possible finite value for the `float` type.
  pub const MAX_FLOAT: Self = Self::Float(f64::MAX);
  /// The smallest possible `float` value greater than zero.
  pub const EPSILON: Self = Self::Float(f64::EPSILON);
  /// The lowest possible value for the `int` type.
  pub const MIN_INT: Self = Self::Int(i64::MIN);
  /// The highest possible value for the `int` type.
  pub const MAX_INT: Self = Self::Int(i64::MAX);

  /// Returns true if the value is of type `nothing`.
  #[inline]
  pub fn is_nothing(&self) -> bool {
    matches!(self, RantValue::Nothing)
  }

  /// Returns true if the value is NaN (Not a Number).
  #[inline]
  pub fn is_nan(&self) -> bool {
    if let Self::Float(f) = self {
      f64::is_nan(*f)
    } else {
      false
    }
  }

  /// Returns true if the value is callable (e.g. a function).
  #[inline]
  pub fn is_callable(&self) -> bool {
    matches!(self, Self::Function(..))
  }
}

#[allow(clippy::len_without_is_empty)]
impl RantValue {
  /// Interprets this value as a boolean value according to Rant's truthiness rules.
  ///
  /// Types are converted as follows:
  /// 1. `bool` returns itself.
  /// 2. `int` returns `true` for any non-zero value; otherwise, `false`.
  /// 3. `float` returns `true` for any [normal](https://en.wikipedia.org/wiki/Normal_number_(computing)) value; otherwise, `false`.
  /// 4. `empty` returns `false`.
  /// 5. Collections that can be zero-length (`string`, `list`, `map`, `range`) return `true` if their length is nonzero; otherwise, `false`.
  /// 6. All other types return `true`.
  #[inline]
  pub fn to_bool(&self) -> bool {
    match self {
      Self::Boolean(b) => *b,
      Self::String(s) => !s.is_empty(),
      Self::Float(n) => n.is_normal(),
      Self::Int(n) => *n != 0,
      Self::Function(_) => true,
      Self::List(l) => !l.borrow().is_empty(),
      Self::Tuple(_) => true,
      Self::Map(m) => !m.borrow().is_empty(),
      Self::Range(r) => !r.is_empty(),
      Self::Selector(_) => true,
      Self::Nothing => false,
    }
  }

  /// Converts to a Rant `bool` value.
  #[inline]
  pub fn into_bool_value(self) -> Self {
    Self::Boolean(self.to_bool())
  }

  /// Converts to a Rant `int` value (or `empty` if the conversion fails).
  #[inline]
  pub fn into_int_value(self) -> Self {
    match self {
      Self::Int(_) => self,
      Self::Float(n) => Self::Int(n as i64),
      Self::String(s) => {
        match s.as_str().parse() {
          Ok(n) => Self::Int(n),
          Err(_) => Self::Nothing,
        }
      },
      Self::Boolean(b) => Self::Int(bi64(b)),
      _ => Self::Nothing
    }
  }

  /// Converts to a Rant `float` value (or `empty` if the conversion fails).
  #[inline]
  pub fn into_float_value(self) -> Self {
    match self {
      Self::Float(_) => self,
      Self::Int(n) => Self::Float(n as f64),
      Self::String(s) => {
        match s.as_str().parse() {
          Ok(n) => Self::Float(n),
          Err(_) => Self::Nothing,
        }
      },
      Self::Boolean(b) => Self::Float(bf64(b)),
      _ => Self::Nothing
    }
  }

  /// Converts to a Rant `string` value.
  #[inline]
  pub fn into_string_value(self) -> Self {
    match self {
      Self::String(_) => self,
      _ => Self::String(self.to_string().into())
    }
  }

  /// Converts to a Rant `list` value.
  #[inline]
  pub fn into_list_value(self) -> Self {
    Self::List(match self {
      Self::Tuple(tuple) => tuple.to_rant_list(),
      Self::String(s) => s.to_rant_list(),
      Self::Range(range) => range.to_rant_list(),
      list @ Self::List(_) => return list,
      _ => return RantValue::Nothing,
    }.into_handle())
  }

  /// Converts to a Rant `tuple` value.
  #[inline]
  pub fn into_tuple_value(self) -> Self {
    Self::Tuple(match self {
      Self::String(s) => s.to_rant_tuple(),
      Self::List(list) => list.borrow().to_rant_tuple(),
      Self::Range(range) => range.to_rant_tuple(),
      tuple @ RantValue::Tuple(_) => return tuple,
      _ => return RantValue::Nothing,
    }.into_handle())
  }

  /// Gets the length of the value.
  #[inline]
  pub fn len(&self) -> usize {
    match self {
      // Length of string is character count
      Self::String(s) => s.len(),
      // Length of list is element count
      Self::List(lst) => lst.borrow().len(),
      // Length of range is element count
      Self::Range(range) => range.len(),
      // Length of map is element count
      Self::Map(map) => map.borrow().raw_len(),
      // Length of tuple is element count
      Self::Tuple(tuple) => tuple.len(),
      // Treat everything else as length 1, since all other value types are primitives
      _ => 1
    }
  }

  /// Returns true if the length of the value is 0.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  #[inline]
  pub fn reversed(&self) -> Self {
    match self {
      Self::String(s) => RantValue::String(s.reversed()),
      Self::Tuple(tuple) => RantValue::Tuple(tuple.iter().rev().collect::<RantTuple>().into_handle()),
      Self::List(list) => RantValue::List(list.borrow().iter().rev().cloned().collect::<RantList>().into_handle()),
      Self::Range(range) => RantValue::Range(range.reversed()),
      _ => self.clone(),
    }
  }

  /// Returns a shallow copy of the value.
  #[inline]
  pub fn shallow_copy(&self) -> Self {
    match self {
      Self::List(list) => RantValue::List(list.cloned()),
      Self::Map(map) => RantValue::Map(map.cloned()),
      Self::Tuple(tuple) => RantValue::Tuple(tuple.cloned()),
      Self::Selector(special) => RantValue::Selector(special.clone()),
      _ => self.clone(),
    }
  }

  /// Gets the Rant type associated with the value.
  #[inline]
  pub fn get_type(&self) -> RantValueType {
    match self {
      Self::String(_) =>     RantValueType::String,
      Self::Float(_) =>      RantValueType::Float,
      Self::Int(_) =>        RantValueType::Int,
      Self::Boolean(_) =>    RantValueType::Boolean,
      Self::Function(_) =>   RantValueType::Function,
      Self::List(_) =>       RantValueType::List,
      Self::Tuple(_) =>      RantValueType::Tuple,
      Self::Map(_) =>        RantValueType::Map,
      Self::Range(_) =>      RantValueType::Range,
      Self::Selector(_) =>    RantValueType::Selector,
      Self::Nothing =>         RantValueType::Nothing,
    }
  }
  
  /// Gets the type name of the value.
  #[inline]
  pub fn type_name(&self) -> &'static str {
    self.get_type().name()
  }

  #[inline]
  fn get_uindex(&self, index: i64) -> Option<usize> {
    let uindex = if index < 0 {
      self.len() as i64 + index
    } else {
      index
    };

    if uindex < 0 || uindex >= self.len() as i64 {
      None
    } else {
      Some(uindex as usize)
    }
  }

  #[inline]
  fn get_ubound(&self, index: i64) -> Option<usize> {
    let uindex = if index < 0 {
      self.len() as i64 + index
    } else {
      index
    };

    if uindex < 0 || uindex > self.len() as i64 {
      None
    } else {
      Some(uindex as usize)
    }
  }

  #[inline]
  fn get_uslice(&self, slice: &Slice) -> Option<(Option<usize>, Option<usize>)> {
    match slice {
      Slice::Full => Some((None, None)),
      Slice::From(i) => Some((Some(self.get_ubound(*i)?), None)),
      Slice::To(i) => Some((None, Some(self.get_ubound(*i)?))),
      Slice::Between(l, r) => Some((Some(self.get_ubound(*l)?), Some(self.get_ubound(*r)?))),
    }
  }

  pub fn slice_get(&self, slice: &Slice) -> ValueSliceResult {
    let (slice_from, slice_to) = self.get_uslice(slice).ok_or(SliceError::OutOfRange)?;

    match self {
      Self::String(s) => Ok(Self::String(s.to_slice(slice_from, slice_to).ok_or(SliceError::OutOfRange)?)),
      Self::Range(range) => {
        Ok(Self::Range(range.sliced(slice_from, slice_to).unwrap()))
      },
      Self::List(list) => {
        let list = list.borrow();
        match (slice_from, slice_to) {
          (None, None) => Ok(self.shallow_copy()),
          (None, Some(to)) => Ok(Self::List((&list[..to]).iter().cloned().collect::<RantList>().into_handle())),
          (Some(from), None) => Ok(Self::List((&list[from..]).iter().cloned().collect::<RantList>().into_handle())),
          (Some(from), Some(to)) => {
            let (from, to) = util::minmax(from, to);
            Ok(Self::List((&list[from..to]).iter().cloned().collect::<RantList>().into_handle()))
          }
        }
      },
      Self::Tuple(tuple) => {
        match (slice_from, slice_to) {
          (None, None) => Ok(self.shallow_copy()),
          (None, Some(to)) => Ok(Self::Tuple((&tuple[..to]).iter().cloned().collect::<RantTuple>().into_handle())),
          (Some(from), None) => Ok(Self::Tuple((&tuple[from..]).iter().cloned().collect::<RantTuple>().into_handle())),
          (Some(from), Some(to)) => {
            let (from, to) = util::minmax(from, to);
            Ok(Self::Tuple((&tuple[from..to]).iter().cloned().collect::<RantTuple>().into_handle()))
          }
        }
      }
      other => Err(SliceError::CannotSliceType(other.get_type()))
    }
  }

  pub fn slice_set(&mut self, slice: &Slice, val: RantValue) -> ValueSliceSetResult {
    let (slice_from, slice_to) = self.get_uslice(slice).ok_or(SliceError::OutOfRange)?;

    match (self, &val) {
      (Self::List(dst_list), Self::List(src_list)) => {
        let src_list = src_list.borrow();
        let mut dst_list = dst_list.borrow_mut();
        let src = src_list.iter().cloned();
        match (slice_from, slice_to) {
          (None, None) => {
            dst_list.splice(.., src);
          },
          (None, Some(to)) => {
            dst_list.splice(..to, src);
          },
          (Some(from), None) => {
            dst_list.splice(from.., src);
          },
          (Some(from), Some(to)) => {
            let (from, to) = util::minmax(from, to);
            dst_list.splice(from..to, src);
          }
        }
        Ok(())
      },
      (Self::List(dst_list), Self::Tuple(src_tuple)) => {
        let mut dst_list = dst_list.borrow_mut();
        let src = src_tuple.iter().cloned();
        match (slice_from, slice_to) {
          (None, None) => {
            dst_list.splice(.., src);
          },
          (None, Some(to)) => {
            dst_list.splice(..to, src);
          },
          (Some(from), None) => {
            dst_list.splice(from.., src);
          },
          (Some(from), Some(to)) => {
            let (from, to) = util::minmax(from, to);
            dst_list.splice(from..to, src);
          }
        }
        Ok(())
      },
      (Self::List(_), other) => Err(SliceError::UnsupportedSpliceSource { src: RantValueType::List, dst: other.get_type() }),
      (dst, _src) => Err(SliceError::CannotSetSliceOnType(dst.get_type()))
    }
  }

  /// Indicates whether the value can be indexed into.
  #[inline]
  pub fn is_indexable(&self) -> bool {
    matches!(self, Self::String(_) | Self::List(_) | Self::Range(_) | Self::Tuple(_))
  }

  /// Attempts to get a value by index.
  pub fn index_get(&self, index: i64) -> ValueIndexResult {
    let uindex = self.get_uindex(index).ok_or(IndexError::OutOfRange)?;

    match self {
      Self::String(s) => {
        if let Some(s) = s.grapheme_at(uindex) {
          Ok(Self::String(s))
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      Self::List(list) => {
        let list = list.borrow();
        if uindex < list.len() {
          Ok(list[uindex].clone())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      Self::Range(range) => {
        if let Some(item) = range.get(uindex) {
          Ok(Self::Int(item))
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      Self::Tuple(tuple) => {
        if uindex < tuple.len() {
          Ok(tuple[uindex].clone())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      _ => Err(IndexError::CannotIndexType(self.get_type()))
    }
  }

  /// Attempts to set a value by index.
  pub fn index_set(&mut self, index: i64, val: RantValue) -> ValueIndexSetResult {
    let uindex = self.get_uindex(index).ok_or(IndexError::OutOfRange)?;

    match self {
      Self::List(list) => {
        let mut list = list.borrow_mut();

        if uindex < list.len() {
          list[uindex] = val;
          Ok(())
        } else {
          Err(IndexError::OutOfRange)
        }
      },
      Self::Map(map) => {
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
      Self::Map(map) => {
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
      Self::Map(map) => {
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
    Self::Nothing
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
      error_type: RuntimeErrorType::ValueError(err),
      description: None,
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
impl_into_runtime_result!(ValueIndexResult, RantValue, IndexError);
impl_into_runtime_result!(ValueIndexSetResult, (), IndexError);

impl Display for IndexError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      IndexError::OutOfRange => write!(f, "value index is out of range"),
      IndexError::CannotIndexType(t) => write!(f, "cannot read index on value of type '{}'", t),
      IndexError::CannotSetIndexOnType(t) => write!(f, "cannot write index on value of type '{}'", t),
    }
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
impl_into_runtime_result!(ValueKeyResult, RantValue, KeyError);
impl_into_runtime_result!(ValueKeySetResult, (), KeyError);

impl Display for KeyError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
        KeyError::KeyNotFound(k) => write!(f, "key not found: '{}'", k),
        KeyError::CannotKeyType(t) => write!(f, "cannot key value of type '{}'", t),
    }
  }
}

/// Error produced by slicing a RantValue.
#[derive(Debug)]
pub enum SliceError {
  /// Slice is out of range.
  OutOfRange,
  /// Tried to slice with an unsupported bound type.
  UnsupportedSliceBoundType(RantValueType),
  /// Type cannot be sliced.
  CannotSliceType(RantValueType),
  /// Type cannot be spliced.
  CannotSetSliceOnType(RantValueType),
  /// Type cannot be spliced with the specified source type.
  UnsupportedSpliceSource { src: RantValueType, dst: RantValueType },
}

impl_error_default!(SliceError);
impl_into_runtime_result!(ValueSliceResult, RantValue, SliceError);
impl_into_runtime_result!(ValueSliceSetResult, (), SliceError);

impl Display for SliceError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SliceError::OutOfRange => write!(f, "slice is out of range"),
      SliceError::UnsupportedSliceBoundType(t) => write!(f, "cannot use '{}' value as slice bound", t),
      SliceError::CannotSliceType(t) => write!(f, "cannot slice '{}' value", t),
      SliceError::CannotSetSliceOnType(t) => write!(f, "cannot set slice on '{}' value", t),
      SliceError::UnsupportedSpliceSource { src, dst } => write!(f, "cannot splice {} into {}", dst, src),
    }
  }
}

/// Represents a dynamically-typed Rant number.
/// 
/// Implements `TryFromRant` and can therefore be used on native functions to accept any number type (`int` or `float`), 
/// while preserving the original type.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum RantNumber {
  /// Rant `int` value.
  Int(i64),
  /// Rant `float` value.
  Float(f64)
}

impl TryFromRant for RantNumber {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    match val {
      RantValue::Int(i) => Ok(RantNumber::Int(i)),
      RantValue::Float(f) => Ok(RantNumber::Float(f)),
      other => Err(ValueError::InvalidConversion {
        from: other.type_name(),
        to: "[number]",
        message: None,
      })
    }
  }

  fn is_optional_param_type() -> bool {
    false
  }
}

/// Filter type that represents any indexable (ordered) Rant collection type.
/// 
/// Use on native functions to accept any ordered collection type. Derefs to `RantValue`.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct RantOrderedCollection(RantValue);

impl Deref for RantOrderedCollection {
  type Target = RantValue;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl TryFromRant for RantOrderedCollection {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    if val.is_indexable() {
      Ok(Self(val))
    } else {
      Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "[ordered collection]",
        message: Some(format!("type '{}' is not an ordered collection type", val.type_name())),
      })
    }
  }
}

impl Debug for RantValue {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      Self::String(s) => write!(f, "{}", s),
      Self::Float(n) => write!(f, "{}", n),
      Self::Int(n) => write!(f, "{}", n),
      Self::Boolean(b) => write!(f, "{}", if *b { "@true" }  else { "@false" }),
      Self::Function(func) => write!(f, "[function({:?})]", func.body),
      Self::List(l) => write!(f, "[list({})]", l.borrow().len()),
      Self::Tuple(t) => write!(f, "[tuple({})]", t.len()),
      Self::Map(m) => write!(f, "[map({})]", m.borrow().raw_len()),
      Self::Range(range) => write!(f, "{}", range),
      Self::Selector(special) => write!(f, "[special({:?})]", special),
      Self::Nothing => write!(f, "[empty]"),
    }
  }
}

fn get_display_string(value: &RantValue, max_depth: usize) -> String {
  match value {
    RantValue::String(s) => s.to_string(),
    RantValue::Float(f) => format!("{}", f),
    RantValue::Int(i) => format!("{}", i),
    RantValue::Boolean(b) => (if *b { "@true" } else { "@false" }).to_string(),
    RantValue::Function(f) => format!("[function({:?})]", f.body),
    RantValue::List(list) => {
      let mut buf = String::new();
      let mut is_first = true;
      buf.push_str("(:");
      if !list.borrow().is_empty() {
        buf.push(' ');
      }
      if max_depth > 0 {
        for val in list.borrow().iter() {
          if is_first {
            is_first = false;
          } else {
            buf.push_str("; ");
          }
          buf.push_str(&get_display_string(val, max_depth - 1));
        }
      } else {
        buf.push_str("...");
      }
      buf.push(')');
      buf
    },
    RantValue::Tuple(tuple) => {
      let mut buf = String::new();
      let mut is_first = true;
      buf.push('(');
      if max_depth > 0 {
        for val in tuple.iter() {
          if is_first {
            is_first = false;
          } else {
            buf.push_str("; ");
          }
          buf.push_str(&get_display_string(val, max_depth - 1));
        }
      } else {
        buf.push_str("...");
      }
      if tuple.len() == 1 {
        buf.push(';');
      }
      buf.push(')');
      buf
    },
    RantValue::Map(map) => {
      let mut buf = String::new();
      let mut is_first = true;
      buf.push_str("(::");
      if !map.borrow().is_empty() {
        buf.push(' ');
      }
      if max_depth > 0 {
        let map = map.borrow();
        for key in map.raw_keys() {
          let key_string = key.to_string();
          if let Some(val) = map.raw_get(&key_string) {
            if is_first {
              is_first = false;
            } else {
              buf.push_str("; ");
            }
            buf.push_str(&format!("{} = {}", key_string, get_display_string(val, max_depth - 1)));
          }
        }
      } else {
        buf.push_str("...");
      }
      buf.push(')');
      buf
    },
    RantValue::Selector(_) => "[special]".to_owned(),
    RantValue::Range(range) => range.to_string(),
    RantValue::Nothing => (if max_depth < MAX_DISPLAY_STRING_DEPTH { "<>" } else { "" }).to_owned(),
  }
}

impl Display for RantValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", get_display_string(self, MAX_DISPLAY_STRING_DEPTH))
  }
}

impl PartialEq for RantValue {
  fn eq(&self, other: &Self) -> bool {
    match (self, other) {
      (Self::Nothing, Self::Nothing) => true,
      (Self::String(a), Self::String(b)) => a == b,
      (Self::Int(a), Self::Int(b)) => a == b,
      (Self::Int(a), Self::Float(b)) => *a as f64 == *b,
      (Self::Float(a), Self::Float(b)) => a == b,
      (Self::Float(a), Self::Int(b)) => *a == *b as f64,
      (Self::Boolean(a), Self::Boolean(b)) => a == b,
      (Self::Range(ra), Self::Range(rb)) => ra == rb,
      (Self::List(a), Self::List(b)) => a.eq(b),
      (Self::Tuple(a), Self::Tuple(b)) => a.eq(b),
      (Self::Map(a), Self::Map(b)) => a.eq(b),
      (Self::Selector(a), Self::Selector(b)) => a == b,
      _ => false
    }
  }
}

impl Eq for RantValue {}

impl PartialOrd for RantValue {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match (self, other) {
      (Self::Nothing, _) | (_, Self::Nothing) => None,
      (Self::Int(a), Self::Int(b)) => a.partial_cmp(b),
      (Self::Float(a), Self::Float(b)) => a.partial_cmp(b),
      (Self::Float(a), Self::Int(b)) => a.partial_cmp(&(*b as f64)),
      (Self::Int(a), Self::Float(b)) => (&(*a as f64)).partial_cmp(b),
      (Self::String(a), Self::String(b)) => a.partial_cmp(b),
      (a, b) => if a == b { Some(Ordering::Equal) } else { None }
    }
  }
}

impl Not for RantValue {
  type Output = Self;
  fn not(self) -> Self::Output {
    RantValue::Boolean(!self.to_bool())
  }
}

impl Neg for RantValue {
  type Output = Self;
  fn neg(self) -> Self::Output {
    match self {
      Self::Int(a) => Self::Int(a.saturating_neg()),
      Self::Float(a) => Self::Float(-a),
      Self::Boolean(a) => Self::Int(-bi64(a)),
      _ => self
    }
  }
}

impl Add for RantValue {
  type Output = Self;
  fn add(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Self::Nothing, Self::Nothing) => Self::Nothing,
      (lhs, Self::Nothing) => lhs,
      (Self::Nothing, rhs) => rhs,
      (Self::Int(a), Self::Int(b)) => Self::Int(a.saturating_add(b)),
      (Self::Int(a), Self::Float(b)) => Self::Float(f64(a) + b),
      (Self::Int(a), Self::Boolean(b)) => Self::Int(a.saturating_add(bi64(b))),
      (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
      (Self::Float(a), Self::Int(b)) => Self::Float(a + f64(b)),
      (Self::Float(a), Self::Boolean(b)) => Self::Float(a + bf64(b)),
      (Self::String(a), Self::String(b)) => Self::String(a + b),
      (Self::String(a), rhs) => Self::String(a + rhs.to_string().into()),
      (Self::Boolean(a), Self::Boolean(b)) => Self::Int(bi64(a) + bi64(b)),
      (Self::Boolean(a), Self::Int(b)) => Self::Int(bi64(a).saturating_add(b)),
      (Self::Boolean(a), Self::Float(b)) => Self::Float(bf64(a) + b),
      (Self::Tuple(a), Self::Tuple(b)) => (a + b).into_rant(),
      (Self::Tuple(a), Self::List(b)) => (a + b).into_rant(),
      (Self::List(a), Self::List(b)) => (a + b).into_rant(),
      (Self::List(a), Self::Tuple(b)) => (a + b).into_rant(),
      (Self::Map(a), Self::Map(b)) => {
        let mut map = RantMap::new();
        for (k, v) in a.borrow().raw_pairs_internal() {
          map.raw_set(k, v.clone());
        }
        for (k, v) in b.borrow().raw_pairs_internal() {
          map.raw_set(k, v.clone());
        }
        map.into_rant()
      },
      (lhs, rhs) => Self::String(RantString::from(format!("{}{}", lhs, rhs)))
    }
  }
}

impl Sub for RantValue {
  type Output = Self;
  fn sub(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Self::Nothing, Self::Nothing) => Self::Nothing,
      (lhs, Self::Nothing) => lhs,
      (Self::Nothing, rhs) => -rhs,
      (Self::Int(a), Self::Int(b)) => Self::Int(a.saturating_sub(b)),
      (Self::Int(a), Self::Float(b)) => Self::Float((a as f64) - b),
      (Self::Int(a), Self::Boolean(b)) => Self::Int(a - bi64(b)),
      (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
      (Self::Float(a), Self::Int(b)) => Self::Float(a - (b as f64)),
      (Self::Float(a), Self::Boolean(b)) => Self::Float(a - bf64(b)),
      (Self::Boolean(a), Self::Boolean(b)) => Self::Int(bi64(a) - bi64(b)),
      (Self::Boolean(a), Self::Int(b)) => Self::Int(bi64(a).saturating_sub(b)),
      (Self::Boolean(a), Self::Float(b)) => Self::Float(bf64(a) - b),
      _ => Self::NAN
    }
  }
}

impl Mul for RantValue {
  type Output = Self;
  fn mul(self, rhs: Self) -> Self::Output {
    match (self, rhs) {
      (Self::Nothing, _) | (_, Self::Nothing) => Self::Nothing,
      (Self::Int(a), Self::Int(b)) => Self::Int(a.saturating_mul(b)),
      (Self::Int(a), Self::Float(b)) => Self::Float((a as f64) * b),
      (Self::Int(a), Self::Boolean(b)) => Self::Int(a * bi64(b)),
      (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
      (Self::Float(a), Self::Int(b)) => Self::Float(a * (b as f64)),
      (Self::Float(a), Self::Boolean(b)) => Self::Float(a * bf64(b)),
      (Self::Boolean(a), Self::Boolean(b)) => Self::Int(bi64(a) * bi64(b)),
      (Self::Boolean(a), Self::Int(b)) => Self::Int(bi64(a) * b),
      (Self::Boolean(a), Self::Float(b)) => Self::Float(bf64(a) * b),
      (Self::String(a), Self::Int(b)) => Self::String(a.as_str().repeat(clamp(b, 0, i64::MAX) as usize).into()),
      _ => Self::NAN
    }
  }
}

impl Div for RantValue {
  type Output = ValueResult<Self>;
  fn div(self, rhs: Self) -> Self::Output {
    Ok(match (self, rhs) {
      (Self::Nothing, _) | (_, Self::Nothing) => Self::Nothing,
      (_, Self::Int(0)) | (_, Self::Boolean(false)) => return Err(ValueError::DivideByZero),
      (Self::Int(a), Self::Int(b)) => Self::Int(a / b),
      (Self::Int(a), Self::Float(b)) => Self::Float((a as f64) / b),
      (Self::Int(a), Self::Boolean(b)) => Self::Int(a / bi64(b)),
      (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
      (Self::Float(a), Self::Int(b)) => Self::Float(a / (b as f64)),
      (Self::Float(a), Self::Boolean(b)) => Self::Float(a / bf64(b)),
      (Self::Boolean(a), Self::Boolean(b)) => Self::Int(bi64(a) / bi64(b)),
      (Self::Boolean(a), Self::Int(b)) => Self::Int(bi64(a) / b),
      (Self::Boolean(a), Self::Float(b)) => Self::Float(bf64(a) / b),
      _ => Self::NAN
    })
  }
}

impl Rem for RantValue {
  type Output = ValueResult<Self>;
  fn rem(self, rhs: Self) -> Self::Output {
    Ok(match (self, rhs) {
      (Self::Nothing, _) | (_, Self::Nothing) => Self::Nothing,
      (_, Self::Int(0)) | (_, Self::Boolean(false)) => return Err(ValueError::DivideByZero),
      (Self::Int(a), Self::Int(b)) => Self::Int(a % b),
      (Self::Int(a), Self::Float(b)) => Self::Float((a as f64) % b),
      (Self::Int(a), Self::Boolean(b)) => Self::Int(a % bi64(b)),
      _ => Self::NAN
    })
  }
}

impl RantValue {
  /// Raises `self` to the `exponent` power.
  #[inline]
  pub fn pow(self, exponent: Self) -> ValueResult<Self> {
    match (self, exponent) {
      (Self::Int(lhs), Self::Int(rhs)) => {
        if rhs >= 0 {
          cast::u32(rhs)
            .map_err(|_| ValueError::Overflow)
            .and_then(|rhs| 
              lhs
              .checked_pow(rhs)
              .ok_or(ValueError::Overflow)
            )
            .map(Self::Int)
        } else {
          Ok(Self::Float((lhs as f64).powf(rhs as f64)))
        }
      },
      (Self::Int(lhs), Self::Float(rhs)) => {
        Ok(Self::Float((lhs as f64).powf(rhs)))
      },
      (Self::Float(lhs), Self::Int(rhs)) => {
        Ok(Self::Float(lhs.powf(rhs as f64)))
      },
      (Self::Float(lhs), Self::Float(rhs)) => {
        Ok(Self::Float(lhs.powf(rhs)))
      },
      _ => Ok(Self::Nothing)
    }
  }

  /// Calculates the absolute value.
  #[inline]
  pub fn abs(self) -> ValueResult<Self> {
    match self {
      Self::Int(i) => i.checked_abs().map(Self::Int).ok_or(ValueError::Overflow),
      Self::Float(f) => Ok(Self::Float(f.abs())),
      _ => Ok(self)
    }
  }

  /// Calculates the logical AND.
  #[inline]
  pub fn and(self, rhs: RantValue) -> Self {
    let truth_lhs = self.to_bool();
    if !truth_lhs {
      self
    } else {
      rhs
    }
  }

  /// Calculates the logical OR.
  #[inline]
  pub fn or(self, rhs: RantValue) -> Self {
    let truth_lhs = self.to_bool();
    if truth_lhs {
      self
    } else {
      rhs
    }
  }

  /// Calculates the logical XOR.
  #[inline]
  pub fn xor(self, rhs: RantValue) -> Self {
    let truth_lhs = self.to_bool();
    let truth_rhs = rhs.to_bool();
    RantValue::Boolean(truth_lhs ^ truth_rhs)
  }
}