use super::*;

/// `[$add: lhs (any); rhs (any)]`
///
/// Adds two values.
pub(crate) fn add(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs + rhs);
  Ok(())
}

pub(crate) fn clamp(vm: &mut VM, (value, a, b): (RantValue, RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(util::clamp(value, a, b));
  Ok(())
}

/// `[$mul: lhs (any); rhs (any)]`
///
/// Multiplies two values.
pub(crate) fn mul(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * rhs);
  Ok(())
}

/// `[$mul-add: lhs (any); mhs (any); rhs (any)]`
///
/// Multiplies two values, then adds a third value to the result.
pub(crate) fn mul_add(vm: &mut VM, (lhs, mhs, rhs): (RantValue, RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * mhs + rhs);
  Ok(())
}

/// `[$sub: lhs (any); rhs (any)]`
///
/// Subtracts one value from another.
pub(crate) fn sub(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs - rhs);
  Ok(())
}

/// `[$div: lhs (any); rhs (any)]`
///
/// Divides one number by another.
pub(crate) fn div(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs / rhs).into_runtime_result()?);
  Ok(())
}

/// `[$mod: lhs (any); rhs (any)]`
///
/// Gets the modulus of two values.
pub(crate) fn mod_(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs % rhs).into_runtime_result()?);
  Ok(())
}

/// `[$neg: val (any)]`
///
/// Negates a value.
pub(crate) fn neg(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(-val);
  Ok(())
}

/// `[$recip: val (any)]`
///
/// Gets the reciproval of a value.
pub(crate) fn recip(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value((RantValue::Float(1.0) / val).into_runtime_result()?);
  Ok(())
}

/// `[$floor: val (integer|float)]`
///
/// Gets the largest integer that is less than or equal to the specified value.
pub(crate) fn floor(vm: &mut VM, val: RantValue) -> RantStdResult {
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
pub(crate) fn ceil(vm: &mut VM, val: RantValue) -> RantStdResult {
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
pub(crate) fn frac(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.fract()),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "cannot use frac function on '{}' value")
  };
  vm.cur_frame_mut().write_value(val_result);
  Ok(())
}