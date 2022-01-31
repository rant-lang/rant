//! The Rant standard library.

#![allow(unused_variables)]
#![allow(clippy::unnecessary_wraps)]

use std::rc::Rc;
use crate::*;
use crate::runtime::*;
use crate::convert::*;
use crate::convert::TryIntoRant;

mod assertion;
mod block;
mod boolean;
mod collections;
mod compare;
mod control;
mod convert;
mod format;
mod general;
mod generate;
mod math;
mod proto;
mod strings;
mod verify;

use self::{
  assertion::*, block::*, boolean::*, collections::*, 
  compare::*, control::*, convert::*, format::*, 
  general::*, generate::*, math::*, proto::*, 
  strings::*, verify::*
};

pub(crate) type RantStdResult = Result<(), RuntimeError>;

#[macro_export]
macro_rules! runtime_error {
  ($err_type:expr) => {{
    return Err(RuntimeError {
      error_type: $err_type,
      description: None,
      stack_trace: None,
    })
  }};
  ($err_type:expr, $msg:literal) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: Some($msg.to_owned()),
      stack_trace: None,
    })
  };
  ($err_type:expr, $msg_fmt:literal, $($msg_fmt_args:expr),+) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: Some(format!($msg_fmt, $($msg_fmt_args),+)),
      stack_trace: None,
    })
  };
}

pub(crate) fn load_stdlib(context: &mut Rant)
{
  macro_rules! load_func {
    ($fname:ident) => {{
      let func = $fname.as_rant_func();
      context.set_global_force(stringify!($fname), RantValue::Function(Rc::new(func)), true);
    }};
    ($fname:ident, $id:literal) => {{
      let func = $fname.as_rant_func();
      context.set_global_force($id, RantValue::Function(Rc::new(func)), true);
    }};
  }

  macro_rules! load_funcs {
    ($($fname:ident $(as $id:expr)?),+) => {
      $(load_func!($fname$(, $id)?);)+
    };
  }

  load_funcs!(
    // General functions
    alt, call, cat, data, either, len, get_type as "type", seed, tap, print, range, require, irange, fork, unfork, try_ as "try",

    // Assertion functions
    assert as "assert", assert_not as "assert-not", assert_eq as "assert-eq", assert_neq as "assert-neq",

    // Formatting functions
    ws_fmt as "ws-fmt", 
    num_fmt as "num-fmt",
    num_fmt_system as "num-fmt-system", 
    num_fmt_alt as "num-fmt-alt",
    num_fmt_padding as "num-fmt-padding",
    num_fmt_precision as "num-fmt-precision",
    num_fmt_upper as "num-fmt-upper",
    num_fmt_endian as "num-fmt-endian",
    num_fmt_sign as "num-fmt-sign",
    num_fmt_infinity as "num-fmt-infinity",
    num_fmt_group_sep as "num-fmt-group-sep",
    num_fmt_decimal_sep as "num-fmt-decimal-sep",

    // Attribute functions
    if_ as "if", elseif as "elseif", else_ as "else", mksel, rep, sel, sep, mut_ as "mut",

    // Attribute frame stack functions
    reset_attrs as "reset-attrs",

    // Block state functions
    step, step_index as "step-index", step_count as "step-count",

    // Boolean functions
    and, not, or, xor,

    // Comparison functions
    eq, neq, gt, lt, ge, le,

    // Verification functions
    is_string as "is-string", is_int as "is-int", is_float as "is-float", 
    is_number as "is-number", is_bool as "is-bool", is_empty as "is-empty", is_nan as "is-nan",
    is_odd as "is-odd", is_even as "is-even", is_factor as "is-factor",
    is_between as "is-between", is_some as "is-some", is,

    // Math functions
    abs, add, sub, mul, div, mul_add as "mul-add", mod_ as "mod", neg, pow, recip, 
    clamp, min, max, floor, ceil, frac,
    asin, sin, acos, cos, atan, atan2, tan, sqrt, 

    // Conversion functions
    to_int as "to-int", to_float as "to-float", to_string as "to-string", to_bool as "to-bool", to_list as "to-list", to_tuple as "to-tuple",

    // Generator functions
    alpha, dig, digh, dignz, maybe, pick, pick_sparse as "pick-sparse",
    rand, randf, rand_list as "rand-list", randf_list as "randf-list", rand_list_sum as "rand-list-sum",

    // Prototype functions
    proto, set_proto as "set-proto",

    // Collection functions
    assoc, augment, augment_self as "augment-self", augment_thru as "augment-thru", chunks, clear, list, tuple, has, keys, index_of as "index-of", insert, last_index_of as "last-index-of", 
    nlist, remove, rev, sift_self as "sift-self", sift_thru as "sift-thru", sift, squish_self as "squish-self", squish_thru as "squish-thru", squish, take, translate, values,
    filter, join, map, sort_self as "sort-self", sort_thru as "sort-thru", sort, shuffle_self as "shuffle-self", shuffle_thru as "shuffle-thru", shuffle, sum,
    list_push as "push", list_pop as "pop", oxford_join as "oxford-join", zip,

    // String functions
    lower, upper, seg, split, lines, indent, string_replace as "string-replace",

    // Error functions
    error
  );

  // Constants
  context.set_global_force("RANT_VERSION", RantValue::String(RANT_LANG_VERSION.into()), true);
  context.set_global_force("BUILD_VERSION", RantValue::String(BUILD_VERSION.into()), true);
  context.set_global_force("EPSILON", RantValue::EPSILON, true);
  context.set_global_force("MIN_FLOAT", RantValue::MIN_FLOAT, true);
  context.set_global_force("MAX_FLOAT", RantValue::MAX_FLOAT, true);
  context.set_global_force("MIN_INT", RantValue::MIN_INT, true);
  context.set_global_force("MAX_INT", RantValue::MAX_INT, true);
  context.set_global_force("INFINITY", RantValue::INFINITY, true);
  context.set_global_force("NEG_INFINITY", RantValue::NEG_INFINITY, true);
  context.set_global_force("NAN", RantValue::NAN, true);
}