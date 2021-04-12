use std::{fmt::{Debug}, ops::Add, fmt::Display};

use once_cell::sync::OnceCell;
use smallvec::{SmallVec};
use unicode_segmentation::UnicodeSegmentation;

use crate::{FromRant, InternalString, IntoRant, RantList, RantValue, ValueError, util};

type Graphemes = SmallVec<[(usize, usize); 1]>;

/// Represents Rant's `string` type.
#[derive(Debug)]
pub struct RantString {
  raw: InternalString,
  graphemes: OnceCell<Option<Graphemes>>,
}

impl RantString {
  /// Creates a new, empty string.
  #[inline]
  pub fn new() -> Self {
    Default::default()
  }

  #[inline]
  fn from_str(s: &str) -> Self {
    Self {
      raw: InternalString::from(s),
      .. Default::default()
    }
  }

  #[inline]
  fn from_char(c: char) -> Self {
    let mut s = InternalString::new();
    s.push(c);
    Self {
      raw: s,
      .. Default::default()
    }
  }
}

impl RantString {
  #[inline]
  fn graphemes(&self) -> &Graphemes {
    self.graphemes.get_or_init(|| 
      Some(UnicodeSegmentation::grapheme_indices(self.raw.as_str(), true)
      .map(|(i, slice)| (i, i + slice.len()))
      .collect::<Graphemes>())
    ).as_ref().unwrap()
  }

  /// Creates a copy of the string with the graphemes in reverse order.
  #[inline]
  pub fn reversed(&self) -> Self {
    let mut buf = InternalString::new();
    for i in (0..self.len()).rev() {
      if let Some(g) = self.grapheme_at(i) {
        buf.push_str(g.as_str());
      }
    }

    Self {
      raw: buf,
      .. Default::default()
    }
  }

  /// Gets a reference to the string as a string slice.
  #[inline]
  pub fn as_str(&self) -> &str {
    self.raw.as_str()
  }

  /// Gets the grapheme string at the specified index.
  #[inline]
  pub fn grapheme_at(&self, index: usize) -> Option<RantString> {
    if index >= self.len() {
      return None
    }

    let (start, end) = self.graphemes()[index];
    Some(RantString::from(&self.raw[start..end]))
  }

  /// Splits the string into individual graphemes and returns them as a Rant list.
  #[inline]
  pub fn to_rant_list(&self) -> RantList {
    let n = self.len();
    let mut list = RantList::with_capacity(n);
    for i in 0..n {
      let c = self.grapheme_at(i).unwrap();
      list.push(RantValue::String(c));
    }
    list
  }

  /// Gets the string at the specified slice.
  pub fn to_slice(&self, start: Option<usize>, end: Option<usize>) -> Option<RantString> {
    let graphemes = self.graphemes();
    let len = graphemes.len();

    // Bounds checks
    if let Some(start) = start {
      if start > len {
        return None
      }
    }

    if let Some(end) = end {
      if end > len {
        return None
      }
    }

    Some(match (start, end) {
      (None, None) => self.clone(),
      (None, Some(end)) => {
        let raw_end = if end < len {
          graphemes[end].0
        } else {
          self.raw.len()
        };
        Self::from(&self.raw[..raw_end])
      },
      (Some(start), None) => {
        let raw_start = graphemes[start].0;
        Self::from(&self.raw[raw_start..])
      },
      (Some(start), Some(end)) => {
        let (start, end) = util::minmax(start, end);
        if start == end {
          return Some(Self::default())
        }
        let raw_start = graphemes[start].0;
        let raw_end = if end < len {
          graphemes[end].0
        } else {
          self.raw.len()
        };
        Self::from(&self.raw[raw_start..raw_end])
      }
    })
  }
}

impl Clone for RantString {
  #[inline]
  fn clone(&self) -> Self {
    Self {
      raw: self.raw.clone(),
      .. Default::default()
    }
  }
}

impl Default for RantString {
  fn default() -> Self {
    Self {
      raw: Default::default(),
      graphemes: Default::default(),
    }
  }
}

impl RantString {
  #[inline]
  pub fn len(&self) -> usize {
    self.graphemes().len()
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.graphemes().is_empty()
  }
}

impl From<InternalString> for RantString {
  fn from(s: InternalString) -> Self {
    Self::from_str(s.as_str())
  }
}

impl From<&str> for RantString {
  fn from(s: &str) -> Self {
    Self::from_str(s)
  }
}

impl From<String> for RantString {
  fn from(s: String) -> Self {
    Self::from_str(&s)
  }
}

impl From<&String> for RantString {
  fn from(s: &String) -> Self {
    Self::from_str(&s)
  }
}

impl From<char> for RantString {
  fn from(c: char) -> Self {
    Self::from_char(c)
  }
}

impl Add for RantString {
  type Output = RantString;

  fn add(self, rhs: Self) -> Self::Output {
    Self {
      raw: self.raw + rhs.raw,
      .. Default::default()
    }
  }
}

impl Display for RantString {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", &self.raw)
  }
}

impl PartialEq for RantString {
  fn eq(&self, other: &Self) -> bool {
    self.raw == other.raw
  }
}

impl PartialOrd for RantString {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    self.raw.partial_cmp(&other.raw)
  }
}

impl FromRant for RantString {
  fn from_rant(val: RantValue) -> Result<Self, ValueError> {
    if let RantValue::String(s) = val.into_rant_string() {
      return Ok(s)
    }
    unreachable!()
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl IntoRant for RantString {
  fn into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::String(self))
  }
}