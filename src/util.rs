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

#[inline(always)]
pub fn bi64(val: bool) -> i64 {
  if val { 1 } else { 0 }
}

#[inline(always)]
pub fn bf64(val: bool) -> f64 {
  if val { 1.0 } else { 0.0 }
}

#[inline(always)]
pub fn bstr(val: bool) -> &'static str {
  if val { "true" } else { "false" }
}