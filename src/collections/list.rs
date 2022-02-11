use std::{cell::RefCell, iter::FromIterator, ops::{DerefMut, Deref, Add}, rc::Rc};
use crate::{RantValue, RantTuple, RantTupleHandle};


/// Reference handle for a Rant list
#[derive(Debug, Clone, PartialEq)]
pub struct RantListHandle(Rc<RefCell<RantList>>);

impl RantListHandle {
  /// Makes a copy of the underlying list and returns a handle containing it.
  pub fn cloned(&self) -> Self {
    Self(Rc::new(RefCell::new((*self.0.borrow()).clone())))
  }
}

impl From<RantList> for RantListHandle {
  #[inline]
  fn from(list: RantList) -> Self {
    Self(Rc::new(RefCell::new(list)))
  }
}

impl Deref for RantListHandle {
  type Target = RefCell<RantList>;
  #[inline]
  fn deref(&self) -> &Self::Target {
    self.0.as_ref()
  }
}

/// Represents Rant's `list` type, which stores an ordered, mutable collection of values.
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

  #[inline]
  pub fn into_handle(self) -> RantListHandle {
    RantListHandle::from(self)
  }

  #[inline]
  pub fn into_rant_tuple(self) -> RantTuple {
    RantTuple::from(self.0)
  }

  #[inline]
  pub fn to_rant_tuple(&self) -> RantTuple {
    RantTuple::from(self.0.clone())
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

impl Add for RantList {
  type Output = RantList;

  fn add(self, rhs: RantList) -> Self::Output {
    self.into_iter().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantList> for RantList {
  type Output = RantList;

  fn add(self, rhs: &RantList) -> Self::Output {
    self.into_iter().chain(rhs.iter().cloned()).collect::<RantList>()
  }  
}

impl Add<RantTuple> for RantList {
  type Output = RantList;

  fn add(self, rhs: RantTuple) -> Self::Output {
    self.into_iter().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantTuple> for RantList {
  type Output = RantList;

  fn add(self, rhs: &RantTuple) -> Self::Output {
    self.into_iter().chain(rhs.iter().cloned()).collect::<RantList>()
  }
}

impl Add<RantList> for &RantList {
  type Output = RantList;

  fn add(self, rhs: RantList) -> Self::Output {
    self.iter().cloned().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantList> for &RantList {
  type Output = RantList;

  fn add(self, rhs: &RantList) -> Self::Output {
    self.iter().cloned().chain(rhs.iter().cloned()).collect::<RantList>()
  }
}

impl Add<RantTuple> for &RantList {
  type Output = RantList;

  fn add(self, rhs: RantTuple) -> Self::Output {
    self.iter().cloned().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantTuple> for &RantList {
  type Output = RantList;

  fn add(self, rhs: &RantTuple) -> Self::Output {
    self.iter().cloned().chain(rhs.iter().cloned()).collect::<RantList>()
  }
}

impl Add for RantListHandle {
  type Output = RantListHandle;

  fn add(self, rhs: Self) -> Self::Output {
    (&*self.borrow() + &*rhs.borrow()).into_handle()
  }
}

impl Add<RantTupleHandle> for RantListHandle {
  type Output = RantListHandle;

  fn add(self, rhs: RantTupleHandle) -> Self::Output {
    (&*self.borrow() + &*rhs).into_handle()
  }
}