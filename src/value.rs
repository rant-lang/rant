use crate::{runtime::VM, syntax::RST, RantResult};
use crate::{RantMap, util::*};
use std::collections::HashMap;
use std::{fmt::{Display, Debug}, rc::Rc, ops::{Add, Not, Sub, Neg}, cmp};
use cast::*;

/// Rant variable value.
#[derive(Clone)]
pub enum RantValue {
    String(String),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Function(Rc<RantClosure>),
    List(Rc<Vec<RantValue>>),
    Map(Rc<RantMap>),
    None
}

impl RantValue {
    pub fn nan() -> Self {
        RantValue::Float(f64::NAN)
    }
}

impl Default for RantValue {
    fn default() -> Self {
        RantValue::None
    }
}

/// Semantic wrapper around a Vec<T> for use in variadic argument sets.
pub(crate) struct VarArgs<T>(Vec<T>);

impl<T> VarArgs<T> {
    pub fn new(args: Vec<T>) -> Self {
        Self(args)
    }
}

/// Closure type used to implement all Rant functions.
#[derive(Debug)]
pub struct RantClosure {
    func: RantFunction,
    locals: Option<RantMap>,
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunction {
    /// Represents a foreign function as a wrapper function accepting a variable number of arguments.
    Foreign(Rc<dyn FnMut(&mut VM, Vec<RantValue>) -> RantResult<()>>),
    /// Represents a user function as an RST.
    User(Rc<RST>)
}

impl Debug for RantFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RantFunction::Foreign(func) => write!(f, "{:?}", Rc::as_ptr(func)),
            RantFunction::User(func) => write!(f, "{:?}", Rc::as_ptr(func))
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
            RantValue::Function(func) => write!(f, "[function: {:?}]", func),
            RantValue::List(_) => write!(f, "[list]"),
            RantValue::Map(_) => write!(f, "[map]"),
            RantValue::None => write!(f, "<>"),
        }
    }
}

impl Display for RantValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RantValue::String(s) => write!(f, "{}", s),
            RantValue::Integer(n) => write!(f, "{}", n),
            RantValue::Float(n) => write!(f, "{}", n),
            RantValue::Boolean(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            RantValue::Function(func) => write!(f, "[function: {:?}]", func),
            RantValue::List(l) => write!(f, "[list({})]", l.len()),
            RantValue::Map(m) => write!(f, "[map({})]", m.len()),
            RantValue::None => Ok(())
        }
    }
}

impl PartialEq for RantValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RantValue::None, RantValue::None) => true,
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
            (RantValue::None, _) | (_, RantValue::None) => None,
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
            RantValue::None => RantValue::Boolean(true),
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
            (RantValue::None, RantValue::None) => RantValue::None,
            (lhs, RantValue::None) => lhs,
            (RantValue::None, rhs) => rhs,
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
            (RantValue::List(a), RantValue::List(b)) => RantValue::List(Rc::new(a.iter().cloned().chain(b.iter().cloned()).collect())),
            (lhs, rhs) => RantValue::String(format!("{}{}", lhs, rhs))
        }
    }
}

impl Sub for RantValue {
    type Output = RantValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (RantValue::None, RantValue::None) => RantValue::None,
            (lhs, RantValue::None) => lhs,
            (RantValue::None, rhs) => -rhs,
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
            RantValue::List(lst) => lst.len(),
            // Length of map is element count
            RantValue::Map(map) => map.len(),
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
            RantValue::None =>          "none"
        }
    }
}