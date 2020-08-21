use std::{rc::Rc, ops::{DerefMut, Deref}, iter::FromIterator};
use crate::{RantString, RantValue};
use fnv::FnvHashMap;

const DEFAULT_MAP_CAPACITY: usize = 16;
const DEFAULT_LIST_CAPACITY: usize = 16;

/// Represents Rant's `list` type, which stores an ordered collection of values.
#[derive(Debug)]
pub struct RantList(Vec<RantValue>);

impl RantList {
  pub fn new() -> Self {
    Self(Vec::with_capacity(DEFAULT_LIST_CAPACITY))
  }

  pub fn with_capacity(capacity: usize) -> Self {
    Self(Vec::with_capacity(capacity))
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

/// Represents Rant's `map` type, which stores a collection of key-value pairs.
/// Map keys are always strings.
#[derive(Debug)]
pub struct RantMap {
  /// The physical contents of the map
  map: FnvHashMap<RantString, RantValue>,
  /// The prototype of the map
  proto: Option<Rc<RantMap>>
}

impl RantMap {
  pub fn new() -> Self {
    Self {
      map: FnvHashMap::with_capacity_and_hasher(DEFAULT_MAP_CAPACITY, Default::default()),
      proto: None
    }
  }

  pub fn raw_len(&self) -> usize {
    self.map.len()
  }
  
  pub fn is_empty(&self) -> bool {
    self.map.is_empty()
  }

  #[inline]
  pub fn raw_set(&mut self, key: &str, val: RantValue) {
    self.map.insert(RantString::from(key), val);
  }

  #[inline]
  pub fn raw_get<'a>(&'a self, key: &str) -> Option<&'a RantValue> {
    self.map.get(key)
  }

  #[inline]
  pub fn raw_has_key(&self, key: &str) -> bool {
    self.map.contains_key(key)
  }

  pub fn raw_keys(&self) -> RantList {
    RantList::from_iter(self.map.keys().map(|k| RantValue::String(k.to_string())))
  }
}

impl Default for RantMap {
  fn default() -> Self {
    RantMap::new()
  }
}