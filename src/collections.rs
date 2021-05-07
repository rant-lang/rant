use std::{borrow::Cow, cell::RefCell, cmp::Ordering, fmt::Display, iter::FromIterator, ops::{DerefMut, Deref}, rc::Rc};
use crate::{InternalString, RantValue, util};
use fnv::FnvHashMap;

const LIST_INLINE_SIZE: usize = 2;

/// Type alias for `Rc<RefCell<RantMap>>`
pub type RantMapRef = Rc<RefCell<RantMap>>;

/// Type alias for `Rc<RefCell<RantList>>`
pub type RantListRef = Rc<RefCell<RantList>>;

/// Represents Rant's `list` type, which stores an ordered collection of values.
#[derive(Debug, Clone, PartialEq)]
pub struct RantList(Vec<RantValue>);

impl RantList {
  /// Creates an empty RantList.
  pub fn new() -> Self {
    Self(vec![])
  }

  /// Creates an empty RantList with the specified initial capacity.
  pub fn with_capacity(capacity: usize) -> Self {
    Self(Vec::with_capacity(capacity))
  }

  #[inline(always)]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[inline(always)]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

impl From<Vec<RantValue>> for RantList {
  fn from(list: Vec<RantValue>) -> Self {
    Self(list)
  }
}

impl Default for RantList {
  fn default() -> Self {
    Self::new()
  }
}

impl Deref for RantList {
  type Target = Vec<RantValue>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl DerefMut for RantList {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl FromIterator<RantValue> for RantList {
  fn from_iter<T: IntoIterator<Item = RantValue>>(iter: T) -> Self {
    let mut list = Self::new();
    for item in iter {
      list.push(item);
    }
    list
  }
}

impl IntoIterator for RantList {
  type Item = RantValue;
  type IntoIter = std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

/// Represents Rant's `map` type, which stores a collection of key-value pairs.
/// Map keys are always strings.
#[derive(Debug, Clone)]
pub struct RantMap {
  /// The physical contents of the map
  map: FnvHashMap<InternalString, RantValue>,
  /// The prototype of the map
  proto: Option<RantMapRef>
}

impl RantMap {
  pub fn new() -> Self {
    Self {
      map: Default::default(),
      proto: None
    }
  }

  #[inline]
  pub fn clear(&mut self) {
    self.map.clear();
  }

  #[inline]
  pub fn raw_len(&self) -> usize {
    self.map.len()
  }
  
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.map.is_empty()
  }

  #[inline]
  pub fn proto(&self) -> Option<RantMapRef> {
    self.proto.clone()
  }

  #[inline]
  pub fn extend<M: Deref<Target = RantMap>>(&mut self, other: M)
  {
    for (k, v) in other.map.iter() {
      self.map.insert(k.clone(), v.clone());
    }
  }

  #[inline]
  pub fn set_proto(&mut self, proto: Option<RantMapRef>) {
    self.proto = proto;
  }

  #[inline]
  pub fn raw_set(&mut self, key: &str, val: RantValue) {
    self.map.insert(InternalString::from(key), val);
  }

  #[inline]
  pub fn raw_remove(&mut self, key: &str) {
    self.map.remove(key);
  }

  #[inline]
  pub fn raw_take(&mut self, key: &str) -> Option<RantValue> {
    self.map.remove(key)
  }

  #[inline]
  pub fn raw_get(&self, key: &str) -> Option<&RantValue> {
    self.map.get(key)
  }

  #[inline]
  pub fn get(&self, key: &str) -> Option<Cow<RantValue>> {
    // Check if the member is in the map itself
    if let Some(member) = self.raw_get(key) {
      return Some(Cow::Borrowed(member))
    }

    // Climb the prototype chain to see if the member is in one of them
    let mut next_proto = self.proto.as_ref().map(Rc::clone);
    while let Some(cur_proto) = next_proto {
      let cur_proto_ref = cur_proto.borrow();
      if let Some(proto_member) = cur_proto_ref.raw_get(key) {
        return Some(Cow::Owned(proto_member.clone()));
      }
      next_proto = cur_proto_ref.proto.as_ref().map(Rc::clone);
    }
    None
  }

  #[inline]
  pub fn raw_has_key(&self, key: &str) -> bool {
    self.map.contains_key(key)
  }

  #[inline]
  pub fn raw_keys(&self) -> RantList {
    self.map.keys().map(|k| RantValue::String(k.as_str().into())).collect()
  }

  #[inline]
  pub(crate) fn raw_pairs_internal(&self) -> impl Iterator<Item = (&'_ str, &'_ RantValue)> {
    self.map.iter().map(|(k, v)| (k.as_str(), v))
  }
}

impl Default for RantMap {
  fn default() -> Self {
    RantMap::new()
  }
}

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
  pub fn to_list(&self) -> RantList {
    let n = self.len();
    let mut list = RantList::new();

    for i in 0..n {
      if let Some(item) = self.get(i) {
        list.push(RantValue::Int(item));
      }
    }

    list
  }
}