use crate::RantValue;

/// Sorts `a` and `b` in ascending order and returns them as a `(min, max)` tuple.
#[inline]
pub fn minmax<T: PartialEq + PartialOrd>(a: T, b: T) -> (T, T) {
  if a <= b {
    return (a, b)
  }
  (b, a)
}

/// Clamps `val` between `a` and `b`.
#[inline]
pub fn clamp<T: PartialEq + PartialOrd>(val: T, a: T, b: T) -> T {
  let (min, max) = minmax(a, b);
  if val >= min && val <= max {
    val
  } else if val < min {
    min
  } else {
    max
  }
}

/// Clamps `val` between 0 and 1.
#[inline]
pub fn saturate(val: f64) -> f64 {
  if val < 0. {
    0.
  } else if val > 1. {
    1.
  } else {
    val
  }
}

/// Converts `true` to `1` and `false` to `0`.
#[inline(always)]
pub fn bi64(val: bool) -> i64 {
  if val { 1 } else { 0 }
}

/// Converts `true` to `1.0` and `false` to `0.0`.
#[inline(always)]
pub fn bf64(val: bool) -> f64 {
  if val { 1.0 } else { 0.0 }
}

#[inline]
pub fn max_rant_value<'a>(mut iter: impl Iterator<Item = &'a RantValue>) -> Option<&'a RantValue> {
  if let Some(mut max) = iter.next() {
    for val in iter {
      if val >= max {
        max = val;
      }
    }
    Some(max)
  } else {
    None
  }
}

#[inline]
pub fn min_rant_value<'a>(mut iter: impl Iterator<Item = &'a RantValue>) -> Option<&'a RantValue> {
  if let Some(mut min) = iter.next() {
    for val in iter {
      if val < min {
        min = val;
      }
    }
    Some(min)
  } else {
    None
  }
}