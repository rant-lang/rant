use std::{cell::RefCell, rc::Rc};
use crate::{random::RantRng, RantValue};

pub type SelectorRef = Rc<RefCell<Selector>>;

/// Manages block execution behavior ("resolution").
pub struct Resolver {
  rng: Rc<RantRng>,
  attr_stack: Vec<AttributeFrame>,
}

pub struct BlockState {
  attrs: AttributeFrame,
  index: usize,
  iter_count: usize,
}

pub enum Reps {
  /// Repeat forever.
  Infinite,
  /// Iterate as many times as there are elements in the block.
  All,
  /// Iterate a specific number of times.
  Finite(usize)
}

impl Resolver {
  pub fn new(rng: &Rc<RantRng>) -> Self {
    Self {
      rng: rng.clone(),
      attr_stack: Default::default(),
    }
  }
}

impl Resolver {
  
}

/// A full set of block attributes.
pub struct AttributeFrame {
  cond_val: bool,
  reps: Reps,
  sep: RantValue,
  selector: Option<SelectorRef>,
}

impl Default for AttributeFrame {
  fn default() -> Self {
    Self {
      cond_val: true,
      reps: Reps::Finite(1),
      sep: RantValue::Empty,
      selector: None,
    }
  }
}

#[derive(Debug)]
pub struct Selector {
  mode: SelectorMode,

}

#[derive(Debug)]
pub enum SelectorMode {
  Forward,
  Reverse,
  Deck,
  CycleDeck,
  Ping,
  Pong,
  NoDouble,
}