//! The Rant standard library.

#![allow(unused_variables)]

use std::rc::Rc;
use crate::*;
use crate::runtime::*;
use crate::convert::*;
use crate::convert::ToRant;
use std::{cmp::Ordering, mem, iter::FromIterator};
use lang::PrintFlag;
use resolver::{SelectorMode, Reps, Selector};
use format::WhitespaceNormalizationMode;

pub(crate) type RantStdResult = Result<(), RuntimeError>;

macro_rules! runtime_error {
  ($err_type:expr, $msg:literal) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: $msg.to_owned(),
      stack_trace: None,
    })
  };
  ($err_type:expr, $msg_fmt:literal, $($msg_fmt_args:expr),+) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: format!($msg_fmt, $($msg_fmt_args),+),
      stack_trace: None,
    })
  };
}

/// `[$alt: a (any); b+ (any)]`
///
/// Prints the first argument that isn't an `empty`.
fn alt(vm: &mut VM, (a, mut b): (RantValue, RequiredVarArgs<RantValue>)) -> RantStdResult {
  if !a.is_empty() {
    vm.cur_frame_mut().write_value(a);
    Ok(())
  } else {
    for val in b.drain(..) {
      if !val.is_empty() {
        vm.cur_frame_mut().write_value(val);
        break
      }
    }
    Ok(())
  }
}

fn whitespace_fmt(vm: &mut VM, (mode, custom): (Option<String>, Option<RantValue>)) -> RantStdResult {
  if let Some(mode) = mode.as_deref() {
    let mode = match mode {
      "default" =>    WhitespaceNormalizationMode::Default,
      "ignore-all" => WhitespaceNormalizationMode::IgnoreAll,
      "verbatim" =>   WhitespaceNormalizationMode::Verbatim,
      "custom" =>     WhitespaceNormalizationMode::Custom(custom.unwrap_or(RantValue::Empty)),
      bad_mode => runtime_error!(RuntimeErrorType::ArgumentError, "invalid whitespace normalization mode: '{}'", bad_mode),
    };
    vm.cur_frame_mut().use_output_mut(move |output| output.format_mut().ws_norm_mode = mode);
  } else {
    let mode = vm.cur_frame().use_output(|output| output.format().ws_norm_mode.clone()).unwrap_or_default();
    let frame = vm.cur_frame_mut();
    match mode {
      WhitespaceNormalizationMode::Custom(custom_val) => {
        frame.write_value(custom_val);
      },
      other => frame.write_frag(match other {
        WhitespaceNormalizationMode::Default =>   "default",
        WhitespaceNormalizationMode::IgnoreAll => "ignore-all",
        WhitespaceNormalizationMode::Verbatim =>  "verbatim",
        WhitespaceNormalizationMode::Custom(_) => unreachable!(),
      })
    }
  }
  Ok(())
}

/// `[$either: cond (bool); a (any); b (any)]`
///
/// Prints `a` if `cond` is true, or `b` otherwise.
fn either(vm: &mut VM, (cond, a, b): (bool, RantValue, RantValue)) -> RantStdResult {
  let val = if cond { a } else { b };
  vm.cur_frame_mut().write_value(val);
  Ok(())
}

/// `[$copy: val (any)]`
///
/// Returns a copy of a value.
fn copy(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(val.shallow_copy());
  Ok(())
}

/// `[$nop]`
///
/// Does absolutely nothing. Intended for use as a default/placeholder callback.
fn nop(vm: &mut VM, _: VarArgs<RantEmpty>) -> RantStdResult {
  Ok(())
}

/// `$[seed]`
///
/// Prints the RNG seed currently in use.
fn seed(vm: &mut VM, _: ()) -> RantStdResult {
  let signed_seed = unsafe {
    mem::transmute::<u64, i64>(vm.rng().seed())
  };
  let frame = vm.cur_frame_mut();
  frame.write_value(RantValue::Integer(signed_seed));
  Ok(())
}

/// [$add: lhs (any); rhs (any)]
///
/// Adds two values.
fn add(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs + rhs);
  Ok(())
}

/// [$mul: lhs (any); rhs (any)]
///
/// Multiplies two values.
fn mul(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * rhs);
  Ok(())
}

/// [$mul-add: lhs (any); mhs (any); rhs (any)]
///
/// Multiplies two values, then adds a third value to the result.
fn mul_add(vm: &mut VM, (lhs, mhs, rhs): (RantValue, RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * mhs + rhs);
  Ok(())
}

/// [$sub: lhs (any); rhs (any)]
///
/// Subtracts one value from another.
fn sub(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs - rhs);
  Ok(())
}

/// [$div: lhs (any); rhs (any)]
///
/// Divides one number by another.
fn div(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs / rhs).into_runtime_result()?);
  Ok(())
}

/// [$mod: lhs (any); rhs (any)]
///
/// Gets the modulus of two values.
fn mod_(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs % rhs).into_runtime_result()?);
  Ok(())
}

/// [$neg: val (any)]
///
/// Negates a value.
fn neg(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(-val);
  Ok(())
}

/// [$recip: val (any)]
///
/// Gets the reciproval of a value.
fn recip(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value((RantValue::Float(1.0) / val).into_runtime_result()?);
  Ok(())
}

/// [$is-odd: val (integer)]
///
/// Returns true if `val` is odd.
fn is_odd(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 != 0));
  Ok(())
}

/// [$is-even: val (integer)]
///
/// Returns true if `val` is even.
fn is_even(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 == 0));
  Ok(())
}

/// [$is-factor: value (integer); factor (integer)]
///
/// Returns true if `value` is divisible by `factor`.
fn is_factor(vm: &mut VM, (value, factor): (i64, i64)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(factor != 0 && value % factor == 0));
  Ok(())
}

fn and(vm: &mut VM, (lhs, rhs, extra): (bool, bool, VarArgs<bool>)) -> RantStdResult {
  let result = (lhs && rhs) && extra.iter().all(|b| *b);
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

fn or(vm: &mut VM, (lhs, rhs, extra): (bool, bool, VarArgs<bool>)) -> RantStdResult {
  let result = (lhs || rhs) || extra.iter().any(|b| *b);
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

fn not(vm: &mut VM, val: bool) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(!val));
  Ok(())
}

fn xor(vm: &mut VM, (lhs, rhs): (bool, bool)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(lhs ^ rhs));
  Ok(())
}

fn eq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a == b));
  Ok(())
}

fn neq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a != b));
  Ok(())
}

fn lt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a < b));
  Ok(())
}

fn gt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a > b));
  Ok(())
}

fn le(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a <= b));
  Ok(())
}

fn ge(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a >= b));
  Ok(())
}

fn is_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::String));
  Ok(())
}

fn is_integer(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Integer));
  Ok(())
}

fn is_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Float));
  Ok(())
}

fn is_number(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(matches!(value.get_type(), RantValueType::Integer | RantValueType::Float)));
  Ok(())
}

fn is_empty(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.is_empty()));
  Ok(())
}

fn is_bool(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Boolean));
  Ok(())
}

fn is_nan(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.is_nan()));
  Ok(())
}

fn len(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Integer(val.len() as i64));
  Ok(())
}

fn get_type(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_frag(val.type_name());
  Ok(())
}

fn num(vm: &mut VM, (a, b): (i64, i64)) -> RantStdResult {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Integer(n));
  Ok(())
}

fn numf(vm: &mut VM, (a, b): (f64, f64)) -> RantStdResult {
  let n = vm.rng().next_f64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Float(n));
  Ok(())
}

fn hex(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = (&b"0123456789abcdef")[rng.next_usize(16)] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn dec(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = (&b"0123456789")[rng.next_usize(10)] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn maybe(vm: &mut VM, p: Option<f64>) -> RantStdResult {
  let b = vm.rng().next_bool(p.unwrap_or(0.5));
  vm.cur_frame_mut().write_value(RantValue::Boolean(b));
  Ok(())
}

fn pick(vm: &mut VM, list: RantValue) -> RantStdResult {
  let index = vm.rng().next_usize(list.len());
  let item = list.index_get(index as i64).into_runtime_result()?;
  vm.cur_frame_mut().write_value(item);
  Ok(())
}

fn join(vm: &mut VM, (sep, list): (RantValue, Vec<RantValue>)) -> RantStdResult {
  let mut is_first = true;
  let frame = vm.cur_frame_mut();
  for val in list {
    if is_first {
      is_first = false;
    } else {
      frame.write_value(sep.clone());
    }
    frame.write_value(val);
  }
  Ok(())
}

fn clear(vm: &mut VM, collection: RantValue) -> RantStdResult {
  match collection {
    RantValue::List(list) => list.borrow_mut().clear(),
    RantValue::Map(map) => map.borrow_mut().clear(),
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "value passed to [clear] must be a collection type");
    }
  }
  Ok(())
}

fn keys(vm: &mut VM, map: RantMapRef) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(map.borrow().raw_keys()))));
  Ok(())
}

fn list_push(vm: &mut VM, (list, value): (RantListRef, RantValue)) -> RantStdResult {
  list.borrow_mut().push(value);
  Ok(())
}

fn list_pop(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let value = list.borrow_mut().pop().unwrap_or(RantValue::Empty);
  vm.cur_frame_mut().write_value(value);
  Ok(())
}

fn insert(vm: &mut VM, (collection, value, pos): (RantValue, RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Insert into list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize > list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.insert(index, value);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot insert into list by '{}' index", non_index.type_name());
    },
    // Insert into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-set function
      map.raw_set(key.as_str(), value);
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot insert into a non-collection");
    }
  }
  Ok(())
}

fn remove(vm: &mut VM, (collection, pos): (RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Remove from list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize >= list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.remove(index);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot remove from list by '{}' index", non_index.type_name());
    },
    // Remove from into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-remove function
      map.raw_remove(key.as_str());
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot remove from a non-collection");
    }
  }
  Ok(())
}

fn take(vm: &mut VM, (collection, pos): (RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Take from list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize >= list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.remove(index);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot take from list by '{}' index", non_index.type_name());
    },
    // Remove from into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-remove function
      if let Some(val) = map.raw_take(key.as_str()) {
        vm.cur_frame_mut().write_value(val);
      } else {
        runtime_error!(RuntimeErrorType::KeyError(KeyError::KeyNotFound(key.to_owned())), "tried to take non-existent key: '{}'", key);
      }
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot take from a non-collection");
    }
  }
  Ok(())
}

fn sorted(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list_copy = list.borrow().clone();
  list_copy.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_copy))));
  Ok(())
}

fn shuffle(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow_mut();
  let n = list.len();
  let rng = vm.rng();
  for i in 0..n {
    list.swap(i, rng.next_usize(n));
  }
  Ok(())
}

fn seg(vm: &mut VM, (s, seg_size): (String, usize)) -> RantStdResult {
  if seg_size > 0 {
    let mut segs = vec![];
    let len = s.len();
    let last_seg_len = len % seg_size;
    let n = len / seg_size + (last_seg_len % 2);
    if last_seg_len > 0 {
      for i in 0..n {
        if i == n - 1 {
          segs.push(s[i * seg_size .. i * seg_size + last_seg_len].to_owned());
        } else {
          segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
        }
      }
    } else {
      for i in 0..n {
        segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
      }
    }
    vm.cur_frame_mut().write_value(segs.to_rant().into_runtime_result()?);
  }
  Ok(())
}

fn split(vm: &mut VM, (s, at): (String, Option<String>)) -> RantStdResult {
  let list = if at.as_ref().map(|s| s.is_empty()).unwrap_or(true) {
    s.chars()
      .map(|c| c.to_string())
      .collect::<Vec<String>>()
  } else {
    s
      .split(at.unwrap().as_str())
      .map(|part| part.to_owned())
      .collect::<Vec<String>>()
  }.to_rant().into_runtime_result()?;

  vm.cur_frame_mut().write_value(list);
  Ok(())
}

fn lines(vm: &mut VM, s: String) -> RantStdResult {
  let lines = RantList::from_iter(s.lines().map(|line| RantValue::String(line.to_owned())));
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(lines))));
  Ok(())
}

fn indent(vm:  &mut VM, (text, indent): (String, String)) -> RantStdResult {
  let frame = vm.cur_frame_mut();
  let mut first = true;
  for line in text.lines() {
    if first {
      first = false;
    } else {
      frame.write_frag("\n");
    }
    frame.write_frag(indent.as_str());
    frame.write_frag(line);
  }
  Ok(())
}

fn proto(vm: &mut VM, map: RantMapRef) -> RantStdResult {
  vm.cur_frame_mut().write_value(map.borrow().proto().map_or(RantValue::Empty, RantValue::Map));
  Ok(())
}

fn set_proto(vm: &mut VM, (map, proto): (RantMapRef, Option<RantMapRef>)) -> RantStdResult {
  map.borrow_mut().set_proto(proto);
  Ok(())
}

fn has_key(vm: &mut VM, (map, key): (RantMapRef, String)) -> RantStdResult {
  let result = map.borrow().raw_has_key(key.as_str());
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

fn to_int(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_int());
  Ok(())
}

fn to_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_float());
  Ok(())
}

fn to_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_string());
  Ok(())
}

fn upper(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_uppercase().as_str());
  Ok(())
}

fn lower(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_lowercase().as_str());
  Ok(())
}

fn call(vm: &mut VM, (func, args): (RantFunctionRef, Option<Vec<RantValue>>)) -> RantStdResult {
  vm.push_val(RantValue::Function(Rc::clone(&func)))?;
  let argc = args.as_ref().map(|args| args.len()).unwrap_or(0);
  if let Some(mut args) = args {
    for arg in args.drain(..).rev() {
      vm.push_val(arg)?;
    }
  }
  vm.cur_frame_mut().push_intent_front(Intent::Call { argc, flag: PrintFlag::None });
  Ok(())
}

fn if_(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_if(condition);
  Ok(())
}

fn else_if(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else_if(condition);
  Ok(())
}

fn else_(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else();
  Ok(())
}

fn resolve(vm: &mut VM, value: RantValue) -> RantStdResult {
  if let RantValue::Block(block) = value {
    vm.push_block(block.as_ref(), block.flag)?;
    Ok(())
  } else {
    Err(RuntimeError {
      error_type: RuntimeErrorType::ValueError(
        ValueError::InvalidConversion {
          from: value.type_name(),
          to: "block",
          message: None,
        }
      ),
      description: "value must be a block".to_owned(),
      stack_trace: None,
    })
  }
}

fn rep(vm: &mut VM, reps: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().reps = match reps {
    RantValue::Integer(n) => Reps::Finite(n.max(0) as usize),
    RantValue::String(s) => match s.as_str() {
      "all" => Reps::All,
      "forever" => Reps::Infinite,
      _ => return Err(RuntimeError {
        error_type: RuntimeErrorType::ArgumentError,
        description: format!("unknown repetition mode: '{}'", s),
        stack_trace: None,
      })
    },
    _ => return Err(RuntimeError {
      error_type: RuntimeErrorType::ArgumentError,
      description: format!("value of type '{}' cannot be used as repetition value", reps.type_name()),
      stack_trace: None,
    })
  };
  Ok(())
}

fn sep(vm: &mut VM, separator: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().separator = separator;
  Ok(())
}

fn step_index(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_index());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

fn step(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

fn step_count(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_count());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

fn mksel(vm: &mut VM, mode: SelectorMode) -> RantStdResult {
  let selector = Rc::new(RefCell::new(Selector::new(mode)));
  let special = RantSpecial::Selector(selector);
  vm.cur_frame_mut().write_value(RantValue::Special(special));
  Ok(())
}

fn sel(vm: &mut VM, selector: Option<RantValue>) -> RantStdResult {
  vm.resolver_mut().attrs_mut().selector = match selector {
    Some(RantValue::Special(RantSpecial::Selector(selector))) => {
      Some(Rc::clone(&selector))
    },
    Some(val) => {
      return Err(RuntimeError {
        error_type: RuntimeErrorType::ValueError(ValueError::InvalidConversion {
          from: val.type_name(),
          to: "selector",
          message: None,
        }),
        description: "value is not a selector".to_owned(),
        stack_trace: None,
      })
    },
    None => None,
  };
  Ok(())
}

fn push_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().push_attrs();
  Ok(())
}

fn pop_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().pop_attrs();
  Ok(())
}

fn count_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().count_attrs();
  Ok(())
}

fn reset_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().reset_attrs();
  Ok(())
}

fn get(vm: &mut VM, key: String) -> RantStdResult {
  let val = vm.get_local(key.as_str())?;
  vm.cur_frame_mut().write_value(val);
  Ok(())
}

pub(crate) fn load_stdlib(globals: &mut RantMap)
{
  macro_rules! load_func {
    ($fname:ident) => {{
      let func = $fname.as_rant_func();
      globals.raw_set(stringify!($fname), RantValue::Function(Rc::new(func)));
    }};
    ($fname:ident, $id:literal) => {{
      let func = $fname.as_rant_func();
      globals.raw_set($id, RantValue::Function(Rc::new(func)));
    }};
  }

  macro_rules! load_funcs {
    ($($fname:ident $(as $id:expr)?),+) => {
      $(load_func!($fname$(, $id)?);)+
    };
  }

  load_funcs!(
    // General functions
    alt, call, either, len, get_type as "type", seed, nop, resolve,

    // Formatting functions
    whitespace_fmt as "whitespace-fmt",

    // Block attribute functions
    if_ as "if", else_if as "else-if", else_ as "else", mksel, rep, sel, sep,

    // Attribute frame stack functions
    push_attrs as "push-attrs", pop_attrs as "pop-attrs", count_attrs as "count-attrs", reset_attrs as "reset-attrs",

    // Block state functions
    step, step_index as "step-index", step_count as "step-count",

    // Boolean functions
    and, not, or, xor,

    // Comparison functions
    eq, neq, gt, lt, ge, le,

    // Verification functions
    is_string as "is-string", is_integer as "is-integer", is_float as "is-float", 
    is_number as "is-number", is_bool as "is-bool", is_empty as "is-empty", is_nan as "is-nan",

    // Math functions
    add, sub, mul, div, mul_add as "mul-add", mod_ as "mod", neg, recip, is_odd as "is-odd", is_even as "is-even", is_factor as "is-factor",

    // Conversion functions
    to_int as "int", to_float as "float", to_string as "string",

    // Generator functions
    dec, hex, maybe, num, numf,

    // Prototype functions
    proto, set_proto as "set-proto",

    // Collection functions
    clear, keys, has_key as "has-key", insert, remove, take,

    // List functions
    pick, join, sorted, shuffle,
    list_push as "push", list_pop as "pop",

    // String functions
    lower, upper, seg, split, lines, indent,

    // Dynamic Variable Access functions
    get
  );
}