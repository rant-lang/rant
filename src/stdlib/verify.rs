use super::*;
use crate::util;


/// `[$is-odd: val (int)]`
///
/// Returns true if `val` is odd.
pub fn is_odd(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write(val % 2 != 0);
  Ok(())
}

/// `[$is-even: val (int)]`
///
/// Returns true if `val` is even.
pub fn is_even(vm: &mut VM, val: i64) -> RantStdResult {
  vm.cur_frame_mut().write(val % 2 == 0);
  Ok(())
}

/// `[$is-factor: value (int); factor (int)]`
///
/// Returns true if `value` is divisible by `factor`.
pub fn is_factor(vm: &mut VM, (value, factor): (i64, i64)) -> RantStdResult {
  vm.cur_frame_mut().write(factor != 0 && value % factor == 0);
  Ok(())
}

pub fn is_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.get_type() == RantValueType::String);
  Ok(())
}

pub fn is_int(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.get_type() == RantValueType::Int);
  Ok(())
}

pub fn is_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.get_type() == RantValueType::Float);
  Ok(())
}

pub fn is_number(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(matches!(value.get_type(), RantValueType::Int | RantValueType::Float));
  Ok(())
}

pub fn is_between(vm: &mut VM, (value, a, b): (RantValue, RantValue, RantValue)) -> RantStdResult {
  let (a, b) = util::minmax(a, b);
  let result = value >= a && value <= b;
  vm.cur_frame_mut().write(result);
  Ok(())
}

pub fn is(vm: &mut VM, (value, type_name): (RantValue, String)) -> RantStdResult {
  vm.cur_frame_mut().write(value.type_name() == type_name);
  Ok(())
}

pub fn is_nothing(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.is_nothing());
  Ok(())
}

pub fn is_some(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(!value.is_nothing());
  Ok(())
}

pub fn is_bool(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.get_type() == RantValueType::Boolean);
  Ok(())
}

pub fn is_nan(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.is_nan());
  Ok(())
}