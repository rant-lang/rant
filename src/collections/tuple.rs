use std::{iter::FromIterator, ops::{Deref, Add}, rc::Rc};
use crate::{RantValue, RantList, RantListHandle};

/// Reference handle for a Rant tuple
#[derive(Debug, Clone, PartialEq)]
pub struct RantTupleHandle(Rc<RantTuple>);

impl RantTupleHandle {
  /// Makes a copy of the underlying tuple and returns a handle containing it.
  pub fn cloned(&self) -> Self {
    Self(Rc::new((*self.0).clone()))
  }
}

impl From<RantTuple> for RantTupleHandle {
  #[inline]
  fn from(tuple: RantTuple) -> Self {
    Self(Rc::new(tuple))
  }
}

impl Deref for RantTupleHandle {
  type Target = RantTuple;
  #[inline]
  fn deref(&self) -> &Self::Target {
    self.0.as_ref()
  }
}

/// Represents Rant's `tuple` type, which stores an ordered, immutable collection of values.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct RantTuple(Vec<RantValue>);

impl RantTuple {
  #[inline]
  pub fn new() -> Self {
    Self(vec![])
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.0.len()
  }

  #[inline]
  pub fn into_handle(self) -> RantTupleHandle {
    RantTupleHandle::from(self)
  }

  #[inline]
  pub fn to_rant_list(&self) -> RantList {
    RantList::from(self.0.clone())
  }

  #[inline]
  pub fn into_rant_list(self) -> RantList {
    RantList::from(self.0)
  }
}

impl From<Vec<RantValue>> for RantTuple {
  fn from(values: Vec<RantValue>) -> Self {
    Self(values)
  }
}

impl Deref for RantTuple {
  type Target = Vec<RantValue>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<'a> FromIterator<&'a RantValue> for RantTuple {
  fn from_iter<T: IntoIterator<Item = &'a RantValue>>(iter: T) -> Self {
    let vec: Vec<RantValue> = iter.into_iter().cloned().collect();
    Self(vec)
  }
}

impl FromIterator<RantValue> for RantTuple {
  fn from_iter<T: IntoIterator<Item = RantValue>>(iter: T) -> Self {
    let vec: Vec<RantValue> = iter.into_iter().collect();
    Self(vec)
  }
}

impl IntoIterator for RantTuple {
  type Item = RantValue;
  type IntoIter = std::vec::IntoIter<Self::Item>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl Add for RantTuple {
  type Output = RantTuple;

  fn add(self, rhs: Self) -> Self::Output {
    self.into_iter().chain(rhs.into_iter()).collect::<RantTuple>()
  }
}

impl Add<&RantTuple> for RantTuple {
  type Output = RantTuple;

  fn add(self, rhs: &RantTuple) -> Self::Output {
    self.into_iter().chain(rhs.iter().cloned()).collect::<RantTuple>()
  }
}

impl Add<RantList> for RantTuple {
  type Output = RantList;

  fn add(self, rhs: RantList) -> Self::Output {
    self.into_iter().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantList> for RantTuple {
  type Output = RantList;

  fn add(self, rhs: &RantList) -> Self::Output {
    self.into_iter().chain(rhs.iter().cloned()).collect::<RantList>()
  }
}

impl Add<RantTuple> for &RantTuple {
  type Output = RantTuple;

  fn add(self, rhs: RantTuple) -> Self::Output {
    self.iter().cloned().chain(rhs.into_iter()).collect::<RantTuple>()
  }
}

impl Add<&RantTuple> for &RantTuple {
  type Output = RantTuple;

  fn add(self, rhs: &RantTuple) -> Self::Output { 
    self.iter().cloned().chain(rhs.iter().cloned()).collect::<RantTuple>()
  }
}

impl Add<RantList> for &RantTuple {
  type Output = RantList;

  fn add(self, rhs: RantList) -> Self::Output {
    self.iter().cloned().chain(rhs.into_iter()).collect::<RantList>()
  }
}

impl Add<&RantList> for &RantTuple {
  type Output = RantList;

  fn add(self, rhs: &RantList) -> Self::Output {
    self.iter().cloned().chain(rhs.iter().cloned()).collect::<RantList>()
  }
}

impl Add for RantTupleHandle {
  type Output = RantTupleHandle;

  fn add(self, rhs: Self) -> Self::Output {
    (&*self + &*rhs).into_handle()
  }
}

impl Add<RantListHandle> for RantTupleHandle {
  type Output = RantListHandle;

  fn add(self, rhs: RantListHandle) -> Self::Output {
    (&*self + &*rhs.borrow()).into_handle()
  }
}