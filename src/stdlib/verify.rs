use super::*;
use crate::util;


/// `[$is-odd: val (int)]`
///
/// Returns true if `val` is odd.
pub(crate) fn is_odd(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 != 0));
  Ok(())
}

/// `[$is-even: val (int)]`
///
/// Returns true if `val` is even.
pub(crate) fn is_even(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(val % 2 == 0));
  Ok(())
}

/// `[$is-factor: value (int); factor (int)]`
///
/// Returns true if `value` is divisible by `factor`.
pub(crate) fn is_factor(vm: &mut VM, (value, factor): (i64, i64)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(factor != 0 && value % factor == 0));
  Ok(())
}

pub(crate) fn is_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::String));
  Ok(())
}

pub(crate) fn is_int(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Int));
  Ok(())
}

pub(crate) fn is_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Float));
  Ok(())
}

pub(crate) fn is_number(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(matches!(value.get_type(), RantValueType::Int | RantValueType::Float)));
  Ok(())
}

pub(crate) fn is_between(vm: &mut VM, (value, a, b): (RantValue, RantValue, RantValue)) -> RantStdResult {
  let (a, b) = util::minmax(a, b);
  let result = value >= a && value <= b;
  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}

pub(crate) fn is(vm: &mut VM, (value, type_name): (RantValue, String)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.type_name() == type_name));
  Ok(())
}

pub(crate) fn is_empty(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.is_empty()));
  Ok(())
}

pub(crate) fn is_some(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(!value.is_empty()));
  Ok(())
}

pub(crate) fn is_bool(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.get_type() == RantValueType::Boolean));
  Ok(())
}

pub(crate) fn is_nan(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(value.is_nan()));
  Ok(())
}