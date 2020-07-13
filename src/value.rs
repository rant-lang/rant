use crate::compiler::rst::RST;
use std::collections::HashMap;

/// Rant variable value.
pub enum RantValue<'key, 'a> {
    String(&'a str),
    Float(f64),
    Integer(i64),
    Boolean(bool),
    Function(RST<'a>),
    List(Vec<RantValue<'key, 'a>>),
    Map(HashMap<&'key str, RantValue<'key, 'a>>),
    None
}