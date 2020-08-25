use std::{cell::RefCell, rc::Rc, mem};
use crate::{random::RantRng, RantValue, lang::{Sequence, Block, PrintFlag}};

pub type SelectorRef = Rc<RefCell<Selector>>;

/// The number of attribute frames you can put on the stack before the runtime goes up in smoke.
const DEFAULT_MAX_ATTR_FRAMES: usize = 127;

/// Manages block execution behavior ("resolution").
pub struct Resolver {
  rng: Rc<RantRng>,
  base_attrs: AttributeFrame,
  attr_override_stack: Vec<AttributeFrame>,
  block_stack: Vec<BlockState>,
}

/// Stores state information for a block that is currently being resolved.
pub struct BlockState {
  elements: Rc<Vec<Rc<Sequence>>>,
  flag: PrintFlag,
  rng: Rc<RantRng>,
  attrs: AttributeFrame,
  iter_count: usize,
  total_reps: usize,
}

impl BlockState {
  pub fn next_element(&mut self) -> Option<Rc<Sequence>> {
    if !self.is_done() {
      self.iter_count += 1;
      let next_index = self.rng.next_usize(self.elements.len());
      Some(Rc::clone(&self.elements[next_index]))
    } else {
      None
    }
  }

  #[inline]
  pub(crate) fn is_done(&self) -> bool {
    !self.attrs.cond_val || (!self.attrs.reps.is_infinite() && self.iter_count >= self.total_reps)
  }

  #[inline]
  pub fn flag(&self) -> PrintFlag {
    self.flag
  }
}

pub enum Reps {
  /// Repeat forever.
  Infinite,
  /// Iterate as many times as there are elements in the block.
  All,
  /// Iterate a specific number of times.
  Finite(usize)
}

impl Reps {
  #[inline(always)]
  pub fn is_infinite(&self) -> bool {
    matches!(self, Reps::Infinite)
  }

  #[inline(always)]
  pub fn is_all(&self) -> bool {
    matches!(self, Reps::All)
  }

  pub fn get_rep_count_for(&self, block: &Block) -> usize {
    match self {
      Reps::Infinite => 0,
      Reps::All => block.len(),
      Reps::Finite(n) => *n,
    }
  }
}

impl Resolver {
  pub fn new(rng: &Rc<RantRng>) -> Self {
    Self {
      rng: rng.clone(),
      base_attrs: Default::default(),
      attr_override_stack: vec![Default::default()],
      block_stack: Default::default(),
    }
  }
}

impl Resolver {
  /// Adds a new block state to the block stack.
  pub fn push_block(&mut self, block: &Block, flag: PrintFlag) {
    let attrs = self.take_attrs();
    let state = BlockState {
      elements: Rc::clone(&block.elements),
      flag: PrintFlag::prioritize(block.flag, flag),
      iter_count: 0,
      rng: Rc::clone(&self.rng),
      total_reps: attrs.reps.get_rep_count_for(block),
      attrs,
    };
    // Since blocks are associated with call stack frames, there is no need to check the stack size here
    self.block_stack.push(state);
  }

  /// Removes the active block state from the block stack.
  #[inline]
  pub fn pop_block(&mut self) -> Option<BlockState> {
    self.block_stack.pop()
  }

  /// Gets a reference to the active block state.
  #[inline]
  pub fn active_block(&self) -> Option<&BlockState> {
    self.block_stack.last()
  }

  /// Gets a mutable reference to the active block state.
  #[inline]
  pub fn active_block_mut(&mut self) -> Option<&mut BlockState> {
    self.block_stack.last_mut()
  }

  /// Takes the topmost attribute frame for use elsewhere and replaces it with a default one.
  pub fn take_attrs(&mut self) -> AttributeFrame {
    if self.attr_override_stack.is_empty() {
      mem::take(&mut self.base_attrs)
    } else {
      mem::take(&mut self.attr_override_stack.last_mut().unwrap())
    }
  }

  pub fn push_attrs(&mut self) {
    // TODO: Limit attr frame stack size
    self.attr_override_stack.push(Default::default())
  }

  pub fn pop_attrs(&mut self) -> Option<AttributeFrame> {
    self.attr_override_stack.pop()
  }

  pub fn count_attrs(&self) -> usize {
    self.attr_override_stack.len() + 1
  }

  #[inline]
  pub fn attrs(&self) -> &AttributeFrame {
    if self.attr_override_stack.is_empty() {
      &self.base_attrs
    } else {
      self.attr_override_stack.last().unwrap()
    }
  }

  #[inline]
  pub fn attrs_mut(&mut self) -> &mut AttributeFrame {
    if self.attr_override_stack.is_empty() {
      &mut self.base_attrs
    } else {
      self.attr_override_stack.last_mut().unwrap()
    }
  }
}

/// A full set of block attributes.
pub struct AttributeFrame {
  pub cond_val: bool,
  pub reps: Reps,
  pub sep: RantValue,
  pub selector: Option<SelectorRef>,
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