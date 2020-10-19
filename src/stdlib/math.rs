use cervine::Cow;

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

/// `[$floor: val (int|float)]`
///
/// Gets the largest integer that is less than or equal to the specified value.
pub(crate) fn floor(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.floor()),
    RantValue::Int(i) => RantValue::Int(i),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "cannot use floor function on '{}' value")
  };
  vm.cur_frame_mut().write_value(val_result);
  Ok(())
}

/// `[$ceil: val (int|float)]`
///
/// Gets the smallest integer that is greater than or equal to the specified value.
pub(crate) fn ceil(vm: &mut VM, val: RantValue) -> RantStdResult {
  let val_result = match val {
    RantValue::Float(f) => RantValue::Float(f.ceil()),
    RantValue::Int(i) => RantValue::Int(i),
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

/// `[$abs: num]`
///
/// Calculates the absolute value of `num`.
pub(crate) fn abs(vm: &mut VM, num: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(num.abs().into_runtime_result()?);
  Ok(())
}

/// `[$pow: lhs (int|float); rhs (int|float)]`
///
/// Raises `lhs` to the `rhs` power.
pub(crate) fn pow(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs.pow(rhs).into_runtime_result()?);
  Ok(())
}

/// `[$sin: x (float)]`
///
/// Calculates the sine of `x` (in radians).
pub(crate) fn sin(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.sin()));
  Ok(())
}

/// `[$asin: x (float)]`
///
/// Calculates the arcsine (in radians) of `x`.
pub(crate) fn asin(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.asin()));
  Ok(())
}

/// `[$cos: x (float)]`
///
/// Calculates the cosine of `x` (in radians).
pub(crate) fn cos(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.cos()));
  Ok(())
}

/// `[$acos: x (float)]`
///
/// Calculates the arccosine (in radians) of `x`.
pub(crate) fn acos(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.acos()));
  Ok(())
}

/// `[$tan: x (float)]`
///
/// Calculates the tangent of `x` (in radians).
pub(crate) fn tan(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.tan()));
  Ok(())
}

/// `[$atan: x (float)]`
///
/// Calculates the arctangent (in radians) of `x`.
pub(crate) fn atan(vm: &mut VM, x: f64) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(x.atan()));
  Ok(())
}

/// `[$atan: y (float); x (float)]`
///
/// Calculates the four-quadrant arctangent (in radians) of `y / x`.
pub(crate) fn atan2(vm: &mut VM, (y, x): (f64, f64)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Float(y.atan2(x)));
  Ok(())
}

/// `[sqrt: num (int|float)]`
///
/// Calculates the square root of `num`.
pub(crate) fn sqrt(vm: &mut VM, num: RantValue) -> RantStdResult {
  let result = match num {
    RantValue::Int(i) => RantValue::Float((i as f64).sqrt()),
    RantValue::Float(f) => RantValue::Float(f.sqrt()),
    other => runtime_error!(RuntimeErrorType::ArgumentError, "can't take square root of type '{}'", other.type_name())
  };
  vm.cur_frame_mut().write_value(result);
  Ok(())
}

pub(crate) fn min(vm: &mut VM, values: RequiredVarArgs<RantValue>) -> RantStdResult {
  if values.is_empty() {
    return Ok(())
  }

  let iter = values.iter();
  let mut min = None;

  for val in iter {
    let flat_val = match val {
      RantValue::List(list_ref) => {
        Some(Cow::Owned(util::min_rant_value(list_ref.borrow().iter()).cloned().unwrap_or_default()))
      },
      other => Some(Cow::Borrowed(other)),
    };

    if min.is_none() || flat_val < min {
      min = flat_val;
    }
  }

  vm.cur_frame_mut().write_value(match min.unwrap() {
    Cow::Owned(val) => val,
    Cow::Borrowed(val) => val.clone(),
  });

  Ok(())
}

pub(crate) fn max(vm: &mut VM, values: RequiredVarArgs<RantValue>) -> RantStdResult {
  if values.is_empty() {
    return Ok(())
  }

  let iter = values.iter();
  let mut max = None;

  for val in iter {
    let flat_val = match val {
      RantValue::List(list_ref) => {
        Some(Cow::Owned(util::max_rant_value(list_ref.borrow().iter()).cloned().unwrap_or_default()))
      },
      other => Some(Cow::Borrowed(other)),
    };

    if max.is_none() || flat_val >= max {
      max = flat_val;
    }
  }

  vm.cur_frame_mut().write_value(match max.unwrap() {
    Cow::Owned(val) => val,
    Cow::Borrowed(val) => val.clone(),
  });

  Ok(())
}