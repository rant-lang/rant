use std::{borrow::Cow, cell::RefCell, ops::Deref, rc::Rc};
use crate::{InternalString, RantValue, RantList};
use fnv::FnvHashMap;

/// Reference handle for a Rant map
#[derive(Debug, Clone)]
pub struct RantMapHandle(Rc<RefCell<RantMap>>);

impl RantMapHandle {
  pub fn cloned(&self) -> Self {
    Self(Rc::new(RefCell::new((*self.0.borrow()).clone())))
  }
}

impl PartialEq for RantMapHandle {
  fn eq(&self, other: &Self) -> bool {
    self.0.as_ptr() == other.0.as_ptr()
  }
}

impl From<RantMap> for RantMapHandle {
  #[inline]
  fn from(map: RantMap) -> Self {
    Self(Rc::new(RefCell::new(map)))
  }
}

impl Deref for RantMapHandle {
  type Target = RefCell<RantMap>;
  #[inline]
  fn deref(&self) -> &Self::Target {
    self.0.as_ref()
  }
}

/// Represents Rant's `map` type, which stores a mutable collection of key-value pairs.
/// Map keys are always strings.
#[derive(Debug, Clone)]
pub struct RantMap {
  /// The physical contents of the map
  map: FnvHashMap<InternalString, RantValue>,
  /// The prototype of the map
  proto: Option<RantMapHandle>
}

impl RantMap {
  pub fn new() -> Self {
    Self {
      map: Default::default(),
      proto: None
    }
  }

  #[inline]
  pub fn into_handle(self) -> RantMapHandle {
    RantMapHandle::from(self)
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
  pub fn proto(&self) -> Option<RantMapHandle> {
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
  pub fn set_proto(&mut self, proto: Option<RantMapHandle>) {
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
    let mut next_proto = self.proto.as_ref().map(RantMapHandle::clone);
    while let Some(cur_proto) = next_proto {
      let cur_proto_ref = cur_proto.borrow();
      if let Some(proto_member) = cur_proto_ref.raw_get(key) {
        return Some(Cow::Owned(proto_member.clone()));
      }
      next_proto = cur_proto_ref.proto.as_ref().map(RantMapHandle::clone);
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
  pub fn raw_values(&self) -> RantList {
    self.map.values().cloned().collect()
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