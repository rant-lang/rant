use crate::{vm::VM, compiler::rst::RST};
use std::collections::HashMap;
use std::{fmt::Debug, rc::Rc, ops::Not};

/// Rant variable value.
#[derive(Clone)]
pub enum RantValue<'a> {
    String(String),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Function(Rc<RantClosure<'a>>),
    List(Rc<Vec<RantValue<'a>>>),
    Map(Rc<RantMap<'a>>),
    None
}

/// Closure type used to implement all Rant functions.
#[derive(Debug)]
pub struct RantClosure<'a> {
    func: RantFunction<'a>,
    locals: Option<RantMap<'a>>
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunction<'a> {
    Native(Rc<dyn FnMut(&VM, Vec<RantValue>)>),
    User(Rc<RST<'a>>)
}

impl Debug for RantFunction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RantFunction::Native(func) => write!(f, "{:?}", Rc::as_ptr(func)),
            RantFunction::User(func) => write!(f, "{:?}", Rc::as_ptr(func))
        }
    }
}

impl Debug for RantValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            RantValue::String(str) => write!(f, "{}", str),
            RantValue::Float(n) => write!(f, "{}", n),
            RantValue::Integer(n) => write!(f, "{}", n),
            RantValue::Boolean(b) => write!(f, "{}", b),
            RantValue::Function(func) => write!(f, "(function: {:?})", func),
            RantValue::List(_) => write!(f, "(list)"),
            RantValue::Map(_) => write!(f, "(map)"),
            RantValue::None => write!(f, "<>")
        }
    }
}

impl PartialEq for RantValue<'_> {
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

impl Not for RantValue<'_> {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            RantValue::None => RantValue::Boolean(true),
            RantValue::Boolean(b) => RantValue::Boolean(!b),
            _ => self
        }
    }
}

impl RantValue<'_> {
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

    pub fn coerce_string(&self) -> RantValue {
        todo!()
    }
}

#[derive(Debug)]
pub struct RantMap<'a> {
    map: HashMap<&'a str, RantValue<'a>>,
    prototype: Option<Rc<RantMap<'a>>>
}

impl RantMap<'_> {
    pub fn len(&self) -> usize {
        self.map.len()
    }
}