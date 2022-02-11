use std::{cmp::Ordering, fmt::Display};
use crate::{RantValue, util, RantList, RantTuple};

/// Represents Rant's `range` type, which characterizes a closed range of integers with an exclusive end bound. 
/// 
/// Includes a `step` value which specifies how far apart adjacent values in the range should be.
/// If the size of the range isn't evenly divisible by `step`, the ending step will be smaller.
#[derive(Debug, Clone, PartialEq)]
pub struct RantRange {
  start: i64,
  end: i64,
  step: i64,
}

impl RantRange {
  #[inline]
  pub fn new(start: i64, end: i64, abs_step: u64) -> Self {
    let abs_step = if abs_step == 0 {
      1
    } else {
      abs_step
    };

    Self {
      start,
      end,
      step: if end < start {
        -(abs_step as i64)
      } else {
        abs_step as i64
      },
    }
  }
}

impl Default for RantRange {
  fn default() -> Self {
    Self {
      start: 0,
      end: 0,
      step: 1,
    }
  }
}

impl Display for RantRange {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let comparison = if self.start < self.end {
      "<"
    } else {
      ">"
    };
    let op = if self.start < self.end { '+' } else { '-' };
    write!(f, "[range({} {} ", self.start, op)?;
    if self.step > 1 {
      write!(f, "{}", self.step)?;
    }
    write!(f, "x {} {})]", comparison, self.end)?;
    Ok(())
  }
}

impl RantRange {
  /// Gets the start bound of the range.
  #[inline]
  pub fn start(&self) -> i64 {
    self.start
  }

  /// Gets the end bound of the range.
  #[inline]
  pub fn end(&self) -> i64 {
    self.end
  }

  /// Gets the absolute step value of the range.
  #[inline]
  pub fn abs_step(&self) -> u64 {
    self.step.saturating_abs() as u64
  }

  /// Gets the signed step value of the range.
  #[inline]
  pub fn step(&self) -> i64 {
    self.step
  }

  /// Gets the absolute difference between the start and end bounds, ignoring the step size.
  #[inline(always)]
  pub fn abs_size(&self) -> usize {
    self.end.saturating_sub(self.start).saturating_abs() as usize
  }

  /// Gets the total number of steps in the range, taking into account the step size.
  #[inline]
  pub fn len(&self) -> usize {
    ((self.end - self.start) as f64 / self.step as f64).ceil() as usize
  }

  /// Gets a reversed copy of the range.
  #[inline]
  pub fn reversed(&self) -> Self {
    if self.start == self.end {
      return self.clone()
    }

    let shift: i64 = if self.start < self.end { -1 } else { 1 };

    Self {
      start: self.end + shift,
      end: self.start + shift,
      step: -self.step,
    }
  }

  /// Indicates whether there are no steps in the range.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.start == self.end || self.abs_step() as usize > self.abs_size()
  }

  /// Gets the nth value in the range.
  #[inline]
  pub fn get(&self, index: usize) -> Option<i64> {
    let offset = self.step * index as i64;
    (index < self.len()).then(|| self.start + offset)
  }

  #[inline]
  fn get_bound(&self, index: usize) -> Option<i64> {
    match index.cmp(&self.len()) {
      Ordering::Less => Some(self.start + self.step * index as i64),
      Ordering::Equal => Some(self.end),
      Ordering::Greater => None
    }
  }

  #[inline]
  pub fn sliced(&self, from: Option<usize>, to: Option<usize>) -> Option<Self> {
    let abs_step = self.abs_step();
    Some(match (from, to) {
      (None, None) => self.clone(),
      (None, Some(to)) => Self::new(self.get_bound(0)?, self.get_bound(to)?, abs_step),
      (Some(from), None) => Self::new(self.get_bound(from)?, self.get_bound(self.len())?, abs_step),
      (Some(from), Some(to)) => {
        let (from, to) = util::minmax(from, to);
        Self::new(self.get_bound(from)?, self.get_bound(to)?, abs_step)
      },
    })
  }

  /// Enumerates the values of the range and returns the results as a Rant `list` object.
  #[inline]
  pub fn to_rant_list(&self) -> RantList {
    let n = self.len();
    let mut list = RantList::new();

    for i in 0..n {
      if let Some(item) = self.get(i) {
        list.push(RantValue::Int(item));
      }
    }

    list
  }

  #[inline]
  pub fn to_rant_tuple(&self) -> RantTuple {
    let n = self.len();
    let mut items = Vec::with_capacity(n);

    for i in 0..n {
      if let Some(item) = self.get(i) {
        items.push(RantValue::Int(item));
      }
    }

    items.into()
  }
}