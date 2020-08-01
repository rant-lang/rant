#[inline]
pub fn minmax<T: Eq + Ord>(a: T, b: T) -> (T, T) {
    if a <= b {
        return (a, b)
    }
    (b, a)
}

#[inline]
pub fn clamp<T: Eq + Ord>(val: T, a: T, b: T) -> T {
    let (min, max) = minmax(a, b);
    if val >= min && val <= max {
        val
    } else if val < min {
        min
    } else {
        max
    }
}

#[inline]
pub fn saturate(val: f64) -> f64 {
    if !val.is_normal() { return val }
    if val < 0. {
        0.
    } else if val > 1. {
        1.
    } else {
        val
    }
}