use std::{cell::RefCell, rc::Rc, ops::{DerefMut, Deref}};
use crate::{syntax::{Sequence, RST}, RantMap, RantString};
use super::output::OutputWriter;

const STACK_INITIAL_CAPACITY: usize = 16;

/// Thin wrapper around call stack vector
pub struct CallStack(Vec<Rc<RefCell<StackFrame>>>);

// Allows direct immutable access to internal stack vector
impl Deref for CallStack {
  type Target = Vec<Rc<RefCell<StackFrame>>>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

// Allows direct mutable access to internal stack vector
impl DerefMut for CallStack {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl Default for CallStack {
  fn default() -> Self {
    Self::new()
  }
}

impl CallStack {
  pub fn new() -> Self {
    Self(Vec::with_capacity(STACK_INITIAL_CAPACITY))
  }
}

pub struct StackFrame {
  /// Variables local to stack frame
  locals: RantMap,
  /// Node sequence being executed by the frame
  sequence: Rc<Sequence>,
  /// Program Counter (as index in sequence) for the current frame
  pc: usize,
  /// Has frame sequence started running?
  started: bool,
  /// Output for this stack frame
  output: Option<OutputWriter>,
}

impl StackFrame {
  pub fn new(sequence: Rc<Sequence>, locals: RantMap, has_output: bool) -> Self {
    Self {
      sequence,
      locals,
      output: if has_output { Some(Default::default()) } else { None },
      started: false,
      pc: 0,
    }
  }
}

impl StackFrame {
  pub fn seq_next(&mut self) -> Option<Rc<RST>> {
    if self.is_done() {
      return None
    }
    
    // Increment PC
    if self.started {
      self.pc += 1;
    } else {
      self.started = true;
    }
    
    self.sequence.get(self.pc).map(Rc::clone)
  }
  
  pub fn pc(&self) -> usize {
    self.pc
  }
}

impl StackFrame {
  #[inline(always)]
  fn is_done(&self) -> bool {
    self.pc >= self.sequence.len()
  }
  
  pub fn write_frag(&mut self, frag: &str) {
    if let Some(output) = self.output.as_mut() {
      output.write_frag(frag);
    }
  }
  
  pub fn write_ws(&mut self, ws: &str) {
    if let Some(output) = self.output.as_mut() {
      output.write_ws(ws);
    }
  }
  
  pub fn render_output(&mut self) -> Option<RantString> {
    self.output.take().map(|o| o.render())
  }
}