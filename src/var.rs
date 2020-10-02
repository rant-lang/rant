use std::{rc::Rc, cell::RefCell, mem};

use crate::RantValue;

/// Represents a Rant variable of one of two flavors: **by-value** or **by-reference**.
///
/// ## Cloning
/// The `Clone` implementation for this type does not copy any data in `ByRef` variants; it only copies the reference.
/// If you want to make a copy of the data in a `Local` regardless of its variant, use the `.cloned()` method.
#[derive(Debug)]
pub enum RantVar {
  ByVal(RantValue),
  ByRef(Rc<RefCell<RantValue>>)
}

impl Default for RantVar {
  fn default() -> Self {
    RantVar::ByVal(RantValue::Empty)
  }
}

impl Clone for RantVar {
  /// Creates a copy of the `Local`, preserving references.
  fn clone(&self) -> Self {
    match self {
      RantVar::ByVal(val) => RantVar::ByVal(val.clone()),
      RantVar::ByRef(val_ref) => RantVar::ByRef(Rc::clone(val_ref)),
    }
  }
}

impl RantVar {
  /// Creates a copy of the `Local` and any data inside of it, even if it is by-ref.
  #[inline]
  pub fn cloned(&self) -> Self {
    match self {
      RantVar::ByVal(val) => RantVar::ByVal(val.clone()),
      RantVar::ByRef(val_ref) => RantVar::ByRef(Rc::new(RefCell::new(val_ref.borrow().clone()))),
    }
  }

  #[inline]
  pub fn is_by_val(&self) -> bool {
    matches!(self, RantVar::ByVal(_))
  }
  
  #[inline]
  pub fn is_by_ref(&self) -> bool {
    matches!(self, RantVar::ByRef(_))
  }
  
  #[inline]
  pub fn make_by_ref(&mut self) {
    if self.is_by_ref() { return }
    if let RantVar::ByVal(val) = mem::take(self) {
      *self = RantVar::ByRef(Rc::new(RefCell::new(val)));
    }
  }
  
  #[inline]
  pub fn write(&mut self, value: RantValue) {
    match self {
      RantVar::ByVal(val) => *val = value,
      RantVar::ByRef(val_ref) => {
        val_ref.replace(value);
      },
    }
  }
  
  // #[inline]
  // pub fn value_ref(&self) -> impl Deref<Target = RantValue> + '_ {
  //   match self {
  //     RantVar::ByVal(val) => cervine::Cow::Borrowed(val),
  //     RantVar::ByRef(val_ref) => cervine::Cow::Owned(val_ref.borrow()),
  //   }
  // }

  #[inline]
  pub fn value_cloned(&self) -> RantValue {
    match self {
      RantVar::ByVal(val) => val.clone(),
      RantVar::ByRef(val_ref) => val_ref.borrow().clone(),
    }
  }
}