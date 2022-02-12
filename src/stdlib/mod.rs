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
  compare::*, convert::*, format::*, 
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

pub fn load_stdlib(context: &mut Rant)
{
  macro_rules! load_func {
    ($fname:ident) => {{
      let func = $fname.into_rant_func();
      let name = stringify!($fname).trim_end_matches('_').replace("_", "-");
      context.set_global_force(name.as_str(), RantValue::Function(Rc::new(func)), true);
    }};
    ($fname:ident, $id:literal) => {{
      let func = $fname.into_rant_func();
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
    alt, call, cat, either, len, type_, seed, tap, print, range, require, irange, fork, unfork, try_,

    // Data source functions
    ds_request, ds_query_sources,

    // Assertion functions
    assert, assert_not, assert_eq, assert_neq,

    // Formatting functions
    ws_fmt, 
    num_fmt, num_fmt_system, num_fmt_alt, num_fmt_padding, num_fmt_precision, num_fmt_upper, 
    num_fmt_endian, num_fmt_sign, num_fmt_infinity, num_fmt_group_sep, num_fmt_decimal_sep,

    // Attribute functions
    if_, elseif, else_, mksel, rep, sel, sep, mut_, sel_skip, sel_freeze, sel_frozen,

    // Attribute frame stack functions
    reset_attrs,

    // Block state functions
    step, step_index, step_count,

    // Boolean functions
    and, not, or, xor,

    // Comparison functions
    eq, neq, gt, lt, ge, le,

    // Verification functions
    is_string, is_int, is_float, is_number, is_bool, is_nothing, is_nan, is_odd, is_even, is_factor, is_between, is_some, is,

    // Math functions
    abs, add, sub, mul, div, mul_add, mod_, neg, pow, recip, 
    clamp, min, max, floor, ceil, frac,
    asin, sin, acos, cos, atan, atan2, tan, sqrt, 

    // Conversion functions
    to_int, to_float, to_string, to_bool, to_list, to_tuple,

    // Generator functions
    alpha, dig, digh, dignz, maybe, pick, pickn, pick_sparse,
    rand, randf, rand_list, randf_list, rand_list_sum,

    // Prototype functions
    proto, set_proto,

    // Collection functions
    assoc, augment, augment_self, augment_thru, chunks, clear, fill_self, fill_thru, has, index_of, insert, keys, last_index_of, list,
    nlist, remove, rev, sift_self, sift_thru, sift, squish_self, squish_thru, squish, take, translate, values,
    filter, join, map, sort_self, sort_thru, sort, shuffle_self, shuffle_thru, shuffle, sum, tuple,
    push, pop, oxford_join, zip,

    // String functions
    lower, upper, seg, split, lines, indent, string_replace, trim,

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