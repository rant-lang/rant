use std::{mem, ops::Index, rc::Rc};
use crate::{RantFunction, RantFunctionInterface, RantFunctionHandle, RantValue, lang::{Block, BlockElement, BlockProtection}, rng::RantRng, runtime_error, RantSelectorHandle, SelectorError};
use smallvec::SmallVec;
use super::{RuntimeError, RuntimeErrorType, RuntimeResult, StackFrameFlavor};

/// The number of attribute frames you can put on the stack before the runtime goes up in smoke.
const MAX_ATTR_FRAMES: usize = 255;
const BLOCK_STACK_INLINE_COUNT: usize = 4;

/// Manages block selection and execution behavior.
pub struct Resolver {
  rng: Rc<RantRng>,
  base_attrs: AttributeFrame,
  attr_override_stack: Vec<AttributeFrame>,
  block_stack: SmallVec<[BlockState; BLOCK_STACK_INLINE_COUNT]>,
}

/// Emitted by the resolver to indicate the current action performed by a block.
pub enum BlockAction {
  /// Run a sequence from an element.
  Element(Rc<BlockElement>),
  /// Call the mutator function and pass in the current element as a callback.
  MutateElement { elem: Rc<BlockElement>, elem_func: RantFunctionHandle, mutator_func: RantFunctionHandle },
  /// Run the separator.
  Separator(RantValue),
}

#[derive(Debug)]
pub struct Weights {
  weights: Vec<f64>,
  sum: f64,
}

impl Weights {
  #[inline]
  pub fn new(capacity: usize) -> Self {
    Self {
      weights: Vec::with_capacity(capacity),
      sum: 0.0,
    }
  }

  #[inline]
  pub fn push(&mut self, weight: f64) {
    let weight = if weight <= 0.0 || weight.is_normal() {
      weight.max(0.0)
    } else {
      1.0
    };
    self.weights.push(weight);
    self.sum += weight;
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.weights.len()
  }

  #[inline]
  pub fn sum(&self) -> f64 {
    self.sum
  }

  #[inline]
  pub fn is_zero(&self) -> bool {
    self.sum <= 0.0
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.weights.is_empty()
  }

  #[inline]
  pub fn as_slice(&self) -> &[f64] {
    self.weights.as_slice()
  }
}

impl Index<usize> for Weights {
  type Output = f64;

  #[inline]
  fn index(&self, index: usize) -> &Self::Output {
    &self.weights[index]
  }
}

/// Stores state information for a block that is currently being resolved.
#[derive(Debug)]
pub struct BlockState {
  /// The elements of the block.
  elements: Rc<Vec<Rc<BlockElement>>>,
  /// Element weights associated with the block
  weights: Option<Weights>,
  /// Flag to short-circuit the block
  force_stop: bool,
  /// The attributes associated with the block
  attrs: AttributeFrame,
  /// How many steps have been run so far
  cur_steps: usize,
  /// How many steps in total the block will run
  total_steps: usize,
  /// Indicates whether the previous step was eligible to be followed by a separator.
  prev_step_separated: bool,
  /// Protection level of the block.
  protection: Option<BlockProtection>,
}

impl BlockState {
  #[inline]
  pub fn next_element(&mut self, rng: &RantRng) -> Result<Option<BlockAction>, SelectorError> {
    if self.is_done() || self.elements.is_empty() || self.weights.as_ref().map_or(false, |weights| weights.is_zero()) { 
      return Ok(None) 
    }

    // Check if element or separator is next
    if self.cur_steps == 0 || self.prev_step_separated {
      self.prev_step_separated = false;
      self.cur_steps += 1;

      // TODO: Allow customization of default selector
      let next_index = self.attrs.selector.as_ref().map_or_else(
        // Default block selection behavior
        || {
          Ok(if let Some(weights) = &self.weights {
            rng.next_usize_weighted(self.elements.len(), weights.as_slice(), weights.sum)
          } else {
            rng.next_usize(self.elements.len())
          })
        }, 
        // Selector behavior
        |sel| sel.borrow_mut().select(self.elements.len(), rng)
      )?;

      let next_elem = Rc::clone(&self.elements[next_index]);
      let next_elem_seq = Rc::clone(&next_elem.main);

      // If the mutator function is set, generate mutated elements
      if let Some(mutator_func) = self.attrs.mutator.as_ref() {
        let elem_func = RantFunction {
          captured_vars: vec![],
          min_arg_count: 0,
          vararg_start_index: 0,
          params: Rc::new(vec![]),
          body: RantFunctionInterface::User(next_elem_seq),
          flavor: Some(if self.is_repeater() { 
            StackFrameFlavor::RepeaterElement 
          } else { 
            StackFrameFlavor::BlockElement 
          })
        };
        return Ok(Some(BlockAction::MutateElement {
          elem: next_elem,
          mutator_func: Rc::clone(mutator_func),
          elem_func: Rc::new(elem_func),
        }))
      }

      Ok(Some(BlockAction::Element(next_elem)))
    } else {
      self.prev_step_separated = true;
      Ok(Some(BlockAction::Separator(self.attrs.separator.clone())))
    }
  }

  #[inline(always)]
  pub fn force_stop(&mut self) {
    self.force_stop = true;
  }

  #[inline]
  pub fn step_index(&self) -> usize {
    self.cur_steps - 1
  }

  #[inline]
  pub fn step(&self) -> usize {
    self.cur_steps
  }

  #[inline]
  pub fn step_count(&self) -> usize {
    self.total_steps
  }

  #[inline]
  pub fn is_repeater(&self) -> bool {
    matches!(self.attrs.reps, Reps::Repeat(_) | Reps::Forever | Reps::All)
  }

  #[inline]
  pub fn has_mutator(&self) -> bool {
    self.attrs.mutator.is_some()
  }

  /// Indicates whether the block has finished and should return.
  #[inline(always)]
  pub(crate) fn is_done(&self) -> bool {
    // Force-stop from break
    self.force_stop
    // Conditional value has evaluated to false
    || !self.attrs.condval.unwrap_or(true) 
    // Finite repetitions are exhausted
    || (!self.attrs.reps.is_infinite() && self.cur_steps >= self.total_steps)
  }
}

/// Provides values for the repetition attribute.
#[derive(Debug, Copy, Clone)]
pub enum Reps {
  /// Repeat forever.
  Forever,
  /// Iterate as many times as there are elements in the block.
  All,
  /// Iterate a specific number of times.
  Repeat(usize),
  // Resolve once.
  Once,
}

impl Reps {
  #[inline(always)]
  pub fn is_infinite(&self) -> bool {
    matches!(self, Reps::Forever)
  }

  #[inline(always)]
  pub fn is_all(&self) -> bool {
    matches!(self, Reps::All)
  }

  #[inline]
  pub fn get_rep_count_for(&self, block: &Block) -> usize {
    match self {
      Reps::Forever => 0,
      Reps::Once => 1,
      Reps::All => block.len(),
      Reps::Repeat(n) => *n,
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
  #[inline]
  pub fn push_block(&mut self, block: &Block, weights: Option<Weights>) -> RuntimeResult<()> {
    let attrs = match block.protection {
      Some(protection) => match protection {
        BlockProtection::Outer => {
          self.push_attrs()?;
          self.take_attrs()
        },
      }
      None => self.take_attrs()
    };

    let state = BlockState {
      elements: Rc::clone(&block.elements),
      weights,
      cur_steps: 0,
      total_steps: attrs.reps.get_rep_count_for(block),
      attrs,
      prev_step_separated: false,
      force_stop: false,
      protection: block.protection,
    };
    // Since blocks are associated with call stack frames, there is no need to check the stack size here
    self.block_stack.push(state);
    Ok(())
  }

  /// Gets the current size of the resolver's block stack.
  #[inline]
  pub fn block_stack_len(&self) -> usize {
    self.block_stack.len()
  }

  /// Removes the active block state from the block stack.
  #[inline]
  pub fn pop_block(&mut self) -> Option<BlockState> {
    let state = self.block_stack.pop();

    if let Some(protection) = state.as_ref().map(|s| s.protection).flatten() {
      match protection {
        BlockProtection::Outer => {
          self.pop_attrs();
        },
      }
    }

    state
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

  /// Gets a mutable reference to the active repeater state.
  #[inline]
  pub fn active_repeater_mut(&mut self) -> Option<&mut BlockState> {
    self.block_stack
      .iter_mut()
      .rev()
      .find(|b| b.is_repeater())
  }

  /// Gets a reference to the active repeater state.
  #[inline]
  pub fn active_repeater(&self) -> Option<&BlockState> {
    self.block_stack
      .iter()
      .rev()
      .find(|b| b.is_repeater())
  }

  /// Takes the topmost attribute frame for use elsewhere and replaces it with a default one.
  #[inline]
  pub fn take_attrs(&mut self) -> AttributeFrame {
    if self.attr_override_stack.is_empty() {
      let next_attr = AttributeFrame::propagate(&self.base_attrs);
      mem::replace(&mut self.base_attrs, next_attr)
    } else {
      let last_attr = self.attr_override_stack.last_mut().unwrap();
      let next_attr = AttributeFrame::propagate(last_attr);
      mem::replace(last_attr, next_attr)
    }
  }

  #[inline]
  pub fn reset_attrs(&mut self) {
    if self.attr_override_stack.is_empty() {
      mem::take(&mut self.base_attrs);
    } else {
      mem::take(self.attr_override_stack.last_mut().unwrap());
    }
  }

  pub fn push_attrs(&mut self) -> RuntimeResult<()> {
    if self.attr_override_stack.len() >= MAX_ATTR_FRAMES {
      runtime_error!(RuntimeErrorType::StackOverflow, "attribute frame stack has overflowed")
    }

    self.attr_override_stack.push(Default::default());
    Ok(())
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
#[derive(Debug)]
pub struct AttributeFrame {
  /// Conditional value returned from last [if]-like call
  pub condval: Option<bool>,
  /// Conditional value used by previous block
  pub prev_condval: Option<bool>,
  /// Indicates if the next attribute frame should receive the current condval
  pub skip_propagate_condval: bool,
  /// Repetition value
  pub reps: Reps,
  /// Separator value
  pub separator: RantValue,
  /// Active selector
  pub selector: Option<RantSelectorHandle>,
  /// Mutator function
  pub mutator: Option<RantFunctionHandle>,
}

impl AttributeFrame {
  /// Creates a new frame, propagating the condval of the specified frame if able.
  pub fn propagate(frame: &AttributeFrame) -> Self {
    Self {
      prev_condval: if frame.skip_propagate_condval { None } else { frame.condval },
      .. Default::default()
    }
  }

  #[inline]
  pub fn make_if(&mut self, cond_val: bool) {
    self.condval = Some(cond_val);
    // Propagate if false
    self.skip_propagate_condval = cond_val;
  }

  #[inline]
  pub fn make_else(&mut self) {
    self.condval = Some(self.prev_condval.map(|b| !b).unwrap_or(false));
    // Do not propagate condvals for else-clauses
    self.skip_propagate_condval = true;
  }

  #[inline]
  pub fn make_else_if(&mut self, cond_val: bool) {
    // Check if there's a condval propagated from a previous block
    let has_propagated_condval = self.prev_condval.is_none();
    // Previous condval must be false and current condval true for clause to run
    // If there is no previous condval, add false non-propagating condval
    self.condval = Some(self.prev_condval.map(|b| !b && cond_val).unwrap_or(false));
    // Only propagate condval if it is false
    self.skip_propagate_condval = cond_val || has_propagated_condval;
  }
}

impl Default for AttributeFrame {
  fn default() -> Self {
    Self {
      condval: None,
      prev_condval: None,
      skip_propagate_condval: false,
      reps: Reps::Once,
      separator: RantValue::Nothing,
      selector: None,
      mutator: None,
    }
  }
}