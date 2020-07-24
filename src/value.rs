use crate::{runtime::VM, compiler::syntax::RST, RantResult, RantError};
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
    func: RantFunction,
    locals: Option<RantMap<'a>>,
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunction {
    Native(Rc<dyn FnMut(&VM, Vec<RantValue>)>),
    User(Rc<RST>)
}

impl Debug for RantFunction {
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

pub trait ToRant {
    fn to_rant<'a>(self) -> RantValue<'a>;
}

pub trait FromRant: Sized {
    fn from_rant<'a>(val: RantValue<'a>) -> RantResult<Self>;
}

macro_rules! rant_int_conversions {
    ($int_type: ty) => {
        impl ToRant for $int_type {
            fn to_rant<'a>(self) -> RantValue<'a> {
                RantValue::Integer(self as i64)
            }
        }
        impl FromRant for $int_type {
            fn from_rant<'a>(val: RantValue<'a>) -> RantResult<Self> {
                if let RantValue::Integer(i) = val {
                    return Ok(i as Self)
                }

                let src_type = val.type_name();
                let dest_type = stringify!{$int_type};
                Err(RantError::ValueConversionError {
                    from: src_type,
                    to: dest_type,
                    message: Some(format!("Rant type '{}' cannot be converted to native type '{}'.", src_type, dest_type))
                })
            }
        }
    };
    ($int_type: ty, $($int_type2: ty), +) => {
        rant_int_conversions! { $int_type }
        rant_int_conversions! { $($int_type2), + }
    };
}

rant_int_conversions! { u8, i8, u16, i16, u32, i32, u64, i64, isize, usize }