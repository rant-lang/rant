//! The Rant standard library.

#![allow(unused_variables)]

use std::rc::Rc;
use crate::*;
use crate::runtime::*;
use crate::convert::*;
use crate::convert::ToRant;
use std::{cmp::Ordering, mem, iter::FromIterator};
use lang::{PrintFlag};
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

/// `[$whitespace-fmt: mode? (string); custom-value? (any)]`
///
/// Gets or sets the whitespace normalization mode for the current scope.
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

/// `[$seed]`
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

/// `$[fork: seed? (string|integer)]`
///
/// Forks the RNG with the specified seed.
fn fork(vm: &mut VM, seed: Option<RantValue>) -> RantStdResult {
  let rng = match seed {
    Some(RantValue::Integer(i)) => vm.rng().fork_i64(i),
    Some(RantValue::String(s)) => vm.rng().fork_str(&s),
    Some(other) => runtime_error!(RuntimeErrorType::ArgumentError, "seeding fork with '{}' value is not supported", other.type_name()),
    None => vm.rng().fork_random(),
  };
  vm.push_rng(Rc::new(rng));
  Ok(())
}

/// `$[unfork]`
///
/// Unforks the RNG down one level.
fn unfork(vm: &mut VM, _: ()) -> RantStdResult {
  if vm.pop_rng().is_none() {
    runtime_error!(RuntimeErrorType::InvalidOperation, "cannot unfork root seed");
  }
  Ok(())
}

/// `[$add: lhs (any); rhs (any)]`
///
/// Adds two values.
fn add(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs + rhs);
  Ok(())
}

/// `[$mul: lhs (any); rhs (any)]`
///
/// Multiplies two values.
fn mul(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * rhs);
  Ok(())
}

/// `[$mul-add: lhs (any); mhs (any); rhs (any)]`
///
/// Multiplies two values, then adds a third value to the result.
fn mul_add(vm: &mut VM, (lhs, mhs, rhs): (RantValue, RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * mhs + rhs);
  Ok(())
}

/// `[$sub: lhs (any); rhs (any)]`
///
/// Subtracts one value from another.
fn sub(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs - rhs);
  Ok(())
}

/// `[$div: lhs (any); rhs (any)]`
///
/// Divides one number by another.
fn div(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs / rhs).into_runtime_result()?);
  Ok(())
}

/// `[$mod: lhs (any); rhs (any)]`
///
/// Gets the modulus of two values.
fn mod_(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs % rhs).into_runtime_result()?);
  Ok(())
}

/// `[$neg: val (any)]`
///
/// Negates a value.
fn neg(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(-val);
  Ok(())
}

/// `[$recip: val (any)]`
///
/// Gets the reciproval of a value.
fn recip(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value((RantValue::Float(1.0) / val).into_runtime_result()?);
  Ok(())
}

/// `[$floor: val (integer|float)]`
///
/// Gets the largest integer that is less than or equal to the specified value.
fn floor(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.floor()),
    RantValue::Integer(i) => RantValue::Integer(i),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "cannot use floor function on '{}' value")
  };
  vm.cur_frame_mut().write_value(val_result);
  Ok(())
}

/// `[$ceil: val (integer|float)]`
///
/// Gets the smallest integer that is greater than or equal to the specified value.
fn ceil(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.ceil()),
    RantValue::Integer(i) => RantValue::Integer(i),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "cannot use ceil function on '{}' value")
  };
  vm.cur_frame_mut().write_value(val_result);
  Ok(())
}

/// `[$frac: val (float)]`
///
/// Gets the fractional part of the specified float value.
fn frac(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.fract()),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "cannot use frac function on '{}' value")
  };
  vm.cur_frame_mut().write_value(val_result);
  Ok(())
}

/// `[$is-odd: val (integer)]`
///
/// Returns true if `val` is odd.
fn is_odd(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 != 0));
  Ok(())
}

/// `[$is-even: val (integer)]`
///
/// Returns true if `val` is even.
fn is_even(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 == 0));
  Ok(())
}

/// `[$is-factor: value (integer); factor (integer)]`
///
/// Returns true if `value` is divisible by `factor`.
fn is_factor(vm: &mut VM, (value, factor): (i64, i64)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(factor != 0 && value % factor == 0));
  Ok(())
}

/// `[$and: lhs (bool); rhs (bool); extra* (bool)]`
///
/// Returns the logical AND of the operands.
fn and(vm: &mut VM, (lhs, rhs, extra): (bool, bool, VarArgs<bool>)) -> RantStdResult {
  let result = (lhs && rhs) && extra.iter().all(|b| *b);
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

/// `[$or: lhs (bool); rhs (bool); extra* (bool)]`
///
/// Returns the logical OR of the operands.
fn or(vm: &mut VM, (lhs, rhs, extra): (bool, bool, VarArgs<bool>)) -> RantStdResult {
  let result = (lhs || rhs) || extra.iter().any(|b| *b);
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

/// `[$not: val (bool)]`
///
/// Gets the inverse of the operand.
fn not(vm: &mut VM, val: bool) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(!val));
  Ok(())
}

/// `$xor: lhs (bool); rhs (bool)]`
///
/// Retirms the logical XOR of the operands.
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

fn rand(vm: &mut VM, (a, b): (i64, i64)) -> RantStdResult {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Integer(n));
  Ok(())
}

fn randf(vm: &mut VM, (a, b): (f64, f64)) -> RantStdResult {
  let n = vm.rng().next_f64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Float(n));
  Ok(())
}

fn rand_list(vm: &mut VM, (a, b, n): (i64, i64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Integer(rng.next_i64(a, b)));
  }
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  Ok(())
}

fn randf_list(vm: &mut VM, (a, b, n): (f64, f64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Float(rng.next_f64(a, b)));
  }
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  Ok(())
}

fn alpha(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn digh(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"0123456789abcdef";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn dig(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"0123456789";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn dignz(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"123456789";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn shred(vm: &mut VM, (value, n, variance): (RantValue, i64, Option<f64>)) -> RantStdResult {
  if n <= 0 {
    return Err(RuntimeError {
      error_type: RuntimeErrorType::ArgumentError,
      description: "shred count must be greater than zero".to_owned(),
      stack_trace: None,
    })
  }

  let rng = vm.rng();
  
  match value {
    RantValue::Integer(m) => {
      let mut shreds = vec![];
      let variance = variance.unwrap_or_default().abs() as i64;
      let quotient = m / n;
      let remainder = m % n;

      let (head_chunk, tail_chunk) = (quotient + remainder, quotient);

      // Populate chunks
      for i in 0..n {
        shreds.push(if i == 0 { head_chunk } else { tail_chunk });
      }

      // Redistribute chunk size randomly
      for i in 0..n {
        let shift = rng.next_i64(0, variance + 1);
        let cell = shreds.get_mut(i as usize).unwrap();
        *cell -= shift;
        let cell = shreds.get_mut(((i + 1) % n) as usize).unwrap();
        *cell += shift;
      }

      vm.cur_frame_mut().write_value(shreds.to_rant().into_runtime_result()?);
    },
    RantValue::Float(m) => {
      let mut shreds = vec![];
      let variance = variance.unwrap_or_default().abs() as f64;
      let nf = n as f64;
      let quotient = m / nf;
      let remainder = m % nf;

      let (head_chunk, tail_chunk) = (quotient + remainder, quotient);

      // Populate chunks
      for i in 0..n {
        shreds.push(if i == 0 { head_chunk } else { tail_chunk });
      }

      // Redistribute chunk size randomly
      for i in 0..n {
        let shift = rng.next_f64(0.0, variance);
        let cell = shreds.get_mut(i as usize).unwrap();
        *cell -= shift;
        let cell = shreds.get_mut(((i + 1) % n) as usize).unwrap();
        *cell += shift;
      }

      vm.cur_frame_mut().write_value(shreds.to_rant().into_runtime_result()?);
    },
    other => {
      return Err(RuntimeError {
        error_type: RuntimeErrorType::ArgumentError,
        description: format!("cannot shred '{}' value", other.type_name()),
        stack_trace: None,
      })
    }
  }

  Ok(())
}

fn squish(vm: &mut VM, (list, target_size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut();

  if target_size == 0 {
    runtime_error!(RuntimeErrorType::ArgumentError, "cannot squish to a target size of 0");
  }

  if list.len() <= target_size || list.len() < 2 {
    return Ok(())
  }

  let rng = vm.rng();
  while list.len() > target_size {
    let n = list.len();
    let left_index = rng.next_usize(n - 1);
    let right_index = left_index + 1;
    let left_val = list.get(left_index).unwrap().clone();
    let right_val = list.remove(right_index);
    list[left_index] = left_val + right_val;
  }

  Ok(())
}

fn squished(vm: &mut VM, (list, target_size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut().clone();

  if target_size == 0 {
    runtime_error!(RuntimeErrorType::ArgumentError, "cannot squish to a target size of 0");
  }

  let rng = vm.rng();
  while list.len() > target_size {
    let n = list.len();
    let left_index = rng.next_usize(n - 1);
    let right_index = (left_index + 1) % n;
    let left_val = list.get(left_index).unwrap().clone();
    let right_val = list.remove(right_index);
    list[left_index] = left_val + right_val;
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));

  Ok(())
}

fn filter(vm: &mut VM, (list, predicate): (RantListRef, RantFunctionRef)) -> RantStdResult {
  let list_ref = list.borrow();
  if list_ref.is_empty() {
    vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_ref.clone()))));
    return Ok(())
  }

  fn _iterate_filter(vm: &mut VM, src: RantListRef, mut dest: RantList, index: usize, predicate: RantFunctionRef) -> RuntimeResult<()> {
    let src_ref = src.borrow();

    // Check predicate result from last iteration
    if index > 0 {
      match vm.pop_val()? {
        RantValue::Boolean(passed) => dest.push(src_ref.get(index - 1).cloned().unwrap_or_default()),
        other => runtime_error!(RuntimeErrorType::TypeError, "filter callback expected to return 'bool' value, but returned '{}' instead", other.type_name())
      }
    }

    // Check if filtering finished
    if index >= src_ref.len() {
      vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(dest))));
      return Ok(())
    }

    let src_clone = Rc::clone(&src);
    let predicate_arg = src_ref.get(index).cloned().unwrap_or_default();
    let predicate_clone = Rc::clone(&predicate);

    // Prepare next iteration
    vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
      _iterate_filter(vm, src_clone, dest, index + 1, predicate)?;
      Ok(())
    })));

    // Prepare predicate call for current iteration
    vm.push_val(RantValue::Function(predicate_clone))?;
    vm.push_val(predicate_arg)?;
    vm.cur_frame_mut().push_intent_front(Intent::Call {
      argc: 1,
      flag: PrintFlag::None,
      override_print: true,
    });

    Ok(())
  }

  let list_clone = Rc::clone(&list);
  vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
    _iterate_filter(vm, list_clone, RantList::new(), 0, predicate)?;
    Ok(())
  })));

  Ok(())
}

fn map(vm: &mut VM, (list, map_func): (RantListRef, RantFunctionRef)) -> RantStdResult {
  let list_ref = list.borrow();
  if list_ref.is_empty() {
    vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_ref.clone()))));
    return Ok(())
  }

  fn _iterate_map(vm: &mut VM, src: RantListRef, mut dest: RantList, index: usize, map_func: RantFunctionRef) -> RuntimeResult<()> {
    let src_ref = src.borrow();

    // Add result from last iteration to destination list
    if index > 0 {
      dest.push(vm.pop_val()?);
    }

    // Check if mapping finished
    if index >= src_ref.len() {
      vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(dest))));
      return Ok(())
    }

    let src_clone = Rc::clone(&src);
    let map_func_arg = src_ref.get(index).cloned().unwrap_or_default();
    let map_func_clone = Rc::clone(&map_func);

    // Prepare next iteration
    vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
      _iterate_map(vm, src_clone, dest, index + 1, map_func)?;
      Ok(())
    })));

    // Prepare predicate call for current iteration
    vm.push_val(RantValue::Function(map_func_clone))?;
    vm.push_val(map_func_arg)?;
    vm.cur_frame_mut().push_intent_front(Intent::Call {
      argc: 1,
      flag: PrintFlag::None,
      override_print: true,
    });

    Ok(())
  }

  let list_clone = Rc::clone(&list);
  vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
    _iterate_map(vm, list_clone, RantList::new(), 0, map_func)?;
    Ok(())
  })));

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

#[allow(clippy::needless_range_loop)]
fn oxford_join(vm: &mut VM, (comma, conj, comma_conj, list): (RantValue, RantValue, RantValue, Vec<RantValue>)) -> RantStdResult {
  let frame = vm.cur_frame_mut();
  let n = list.len();
  for i in 0..n {
    match (i, n, i == n - 1) {
      (0, ..) | (_, 1, _) => {},
      (1, 2, _) => frame.write_value(conj.clone()),
      (.., false) => frame.write_value(comma.clone()),
      (.., true) => frame.write_value(comma_conj.clone()),
    }
    frame.write_value(list[i].clone());
  }
  Ok(())
}

fn sum(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(())
  }

  let mut iter = list.iter().cloned();
  let mut sum = iter.next().unwrap();
  
  for val in iter {
    sum = sum + val;
  }

  vm.cur_frame_mut().write_value(sum);

  Ok(())
}

fn min(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(());
  }

  let mut iter = list.iter();
  let mut min = iter.next().unwrap();

  for val in iter {
    if val < min {
      min = val;
    }
  }

  vm.cur_frame_mut().write_value(min.clone());

  Ok(())
}

fn max(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(());
  }

  let mut iter = list.iter();
  let mut max = iter.next().unwrap();

  for val in iter {
    if val > max {
      max = val;
    }
  }

  vm.cur_frame_mut().write_value(max.clone());

  Ok(())
}

fn shuffled(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow().clone();
  if list.is_empty() {
    return Ok(());
  }

  let n = list.len();
  let rng = vm.rng();

  for i in 0..n {
    let swap_index = rng.next_usize(n);
    list.swap(i, swap_index);
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  
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

fn assoc(vm: &mut VM, (keys, values): (RantListRef, RantListRef)) -> RantStdResult {
  let keys = keys.borrow();
  let values = values.borrow();
  if keys.len() != values.len() {
    runtime_error!(RuntimeErrorType::ArgumentError, "assoc: key and value counts don't match");
  }

  let mut map = RantMap::new();
  for (key, val) in keys.iter().zip(values.iter()) {
    map.raw_set(key.to_string().as_ref(), val.clone());
  }

  vm.cur_frame_mut().write_value(RantValue::Map(Rc::new(RefCell::new(map))));

  Ok(())
}

fn translate(vm: &mut VM, (list, map): (RantListRef, RantMapRef)) -> RantStdResult {
  let list = list.borrow();
  let map = map.borrow();

  let translated: RantList = list
    .iter()
    .map(|val| map.raw_get(val.to_string().as_ref()).cloned().unwrap_or_else(|| val.clone()))
    .collect();

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(translated))));

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

fn sift(vm: &mut VM, (list, size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut();
  if list.len() <= size {
    return Ok(())
  }

  let rng = vm.rng();
  while list.len() > size {
    let remove_index = rng.next_usize(list.len());
    list.remove(remove_index);
  }

  Ok(())
}

fn sifted(vm: &mut VM, (list, size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow().clone();

  let rng = vm.rng();
  while list.len() > size {
    let remove_index = rng.next_usize(list.len());
    list.remove(remove_index);
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));

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

fn sort(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow_mut();
  list.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
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
  if list.is_empty() {
    return Ok(())
  }

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
  vm.cur_frame_mut().push_intent_front(Intent::Call { argc, flag: PrintFlag::None, override_print: false });
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

fn break_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.interrupt_repeater(val, false)?;
  Ok(())
}

fn continue_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.interrupt_repeater(val, true)?;
  Ok(())
}

fn return_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.func_return(val)?;
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
    RantValue::Integer(n) => Reps::Repeat(n.max(0) as usize),
    RantValue::String(s) => match s.as_str() {
      "once" => Reps::Once,
      "all" => Reps::All,
      "forever" => Reps::RepeatForever,
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
  let val = vm.get_var_value(key.as_str(), lang::AccessPathKind::Local)?;
  vm.cur_frame_mut().write_value(val);
  Ok(())
}

fn error(vm: &mut VM, msg: Option<String>) -> RantStdResult {
  const DEFAULT_ERROR_MESSAGE: &str = "user error";
  Err(RuntimeError {
    error_type: RuntimeErrorType::UserError,
    description: msg.unwrap_or_else(|| DEFAULT_ERROR_MESSAGE.to_owned()),
    stack_trace: None,
  })
}

fn require(vm: &mut VM, module_path: String) -> RantStdResult {
  // Get name of module from path
  if let Some(module_name) = 
    PathBuf::from(&module_path)
    .with_extension("")
    .file_name()
    .map(|name| name.to_str())
    .flatten()
    .map(|name| name.to_owned())
  {
    // Check if module is cached
    if let Some(RantValue::Map(module_cache_ref)) = vm.context().get_global(crate::MODULES_CACHE_KEY) {
      if let Some(RantValue::Map(cached_module)) = module_cache_ref.borrow().raw_get(&module_name) {
        vm.cur_frame_mut().push_intent_front(Intent::LoadModule { module_name });
        vm.push_val(RantValue::Map(Rc::clone(cached_module)))?;
        return Ok(())
      }
    }

    // If not cached, attempt to load it from file
    let caller_origin = Rc::clone(&vm.cur_frame().origin());
    let module_pgm = vm.context_mut().try_load_module(&module_path, caller_origin).into_runtime_result()?;
    vm.cur_frame_mut().push_intent_front(Intent::LoadModule { module_name });
    vm.push_frame(Rc::clone(&module_pgm.root), true)?;
    Ok(())
  } else {
    runtime_error!(RuntimeErrorType::ArgumentError, "module name is missing from path");
  }
}

pub(crate) fn load_stdlib(context: &mut Rant)
{
  macro_rules! load_func {
    ($fname:ident) => {{
      let func = $fname.as_rant_func();
      context.set_global(stringify!($fname), RantValue::Function(Rc::new(func)));
    }};
    ($fname:ident, $id:literal) => {{
      let func = $fname.as_rant_func();
      context.set_global($id, RantValue::Function(Rc::new(func)));
    }};
  }

  macro_rules! load_funcs {
    ($($fname:ident $(as $id:expr)?),+) => {
      $(load_func!($fname$(, $id)?);)+
    };
  }

  load_funcs!(
    // General functions
    alt, call, either, len, get_type as "type", seed, nop, resolve, fork, unfork,

    // Formatting functions
    whitespace_fmt as "whitespace-fmt",

    // Block attribute / control flow functions
    break_ as "break", continue_ as "continue", if_ as "if", else_if as "else-if", else_ as "else", 
    mksel, rep, return_ as "return", sel, sep,

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
    alpha, dig, digh, dignz, maybe, rand, randf, rand_list as "rand-list", randf_list as "randf-list", shred,

    // Prototype functions
    proto, set_proto as "set-proto",

    // Collection functions
    assoc, clear, keys, has_key as "has-key", insert, remove, sift, sifted, squish, squished, take, translate,

    // List functions
    pick, filter, join, map, sort, sorted, shuffle, shuffled, sum, min, max,
    list_push as "push", list_pop as "pop", oxford_join as "oxford-join",

    // String functions
    lower, upper, seg, split, lines, indent,

    // Dynamic Variable Access functions
    get,

    // Error functions
    error
  );

  // Load [require] function if requested
  if context.options.enable_require {
    load_func!(require);
  }

  // Miscellaneous
  context.set_global("RANT_VERSION", RantValue::String(RANT_VERSION.to_owned()));
}