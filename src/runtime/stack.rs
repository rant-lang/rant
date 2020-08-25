use std::{rc::Rc, ops::{DerefMut, Deref}};
use std::{collections::VecDeque};
use crate::{lang::{Sequence, RST}, RantMap, RantValue, Rant};
use crate::runtime::*;
use super::{OutputBuffer, output::OutputWriter, Intent};

type CallStackVector = SmallVec<[StackFrame; super::CALL_STACK_INLINE_COUNT]>;

/// Thin wrapper around call stack vector
pub struct CallStack(CallStackVector);

// Allows direct immutable access to internal stack vector
impl Deref for CallStack {
  type Target = CallStackVector;
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
    Self(Default::default())
  }

  pub fn set_local(&mut self, context: &Rant, id: &str, val: RantValue) -> RuntimeResult<()> {
    // Check locals
    for frame in self.iter_mut().rev() {
      if frame.locals.raw_has_key(id) {
        frame.locals.raw_set(id, val);
        return Ok(())
      }
    }

    // Check globals
    let mut globals = context.globals.borrow_mut();
    if globals.raw_has_key(id) {
      globals.raw_set(id, val);
      return Ok(())
    }

    Err(RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: format!("variable '{}' not found", id),
    })
  }

  pub fn get_local(&self, context: &Rant, id: &str) -> RuntimeResult<RantValue> {
    // Check locals
    for frame in self.iter().rev() {
      if let Some(val) = frame.locals.raw_get(id) {
        return Ok(val.clone())
      }
    }

    // Check globals
    if let Some(val) = context.globals.borrow().raw_get(id) {
      return Ok(val.clone())
    }

    Err(RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: format!("variable '{}' not found", id),
    })
  }

  pub fn def_local(&mut self, context: &Rant, id: &str, val: RantValue) -> RuntimeResult<()> {
    if self.len() > 1 {
      if let Some(frame) = self.last_mut() {
        frame.locals.raw_set(id, val);
        return Ok(())
      }
    }

    // If there's only one frame on the stack, it means we're in the global scope of the program.
    context.globals.borrow_mut().raw_set(id, val);
    Ok(())
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
  /// Output for the frame
  output: Option<OutputWriter>,
  /// Intent queue for the frame
  intents: VecDeque<Intent>,
}

impl StackFrame {
  pub fn new(sequence: Rc<Sequence>, locals: RantMap, has_output: bool) -> Self {
    Self {
      sequence,
      locals,
      output: if has_output { Some(Default::default()) } else { None },
      started: false,
      pc: 0,
      intents: Default::default(),
    }
  }
}

impl StackFrame {
  #[inline]
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
  
  /// Gets the Program Counter (PC) for the frame.
  #[inline]
  pub fn pc(&self) -> usize {
    self.pc
  }

  /// Takes the next intent to be handled.
  #[inline]
  pub fn take_intent(&mut self) -> Option<Intent> {
    self.intents.pop_front()
  }

  /// Pushes an intent to the front of the queue so that it is handled next.
  #[inline]
  pub fn push_intent_front(&mut self, intent: Intent) {
    self.intents.push_front(intent);
  }

  /// Pushes an intent to the back of the queue so that it is handled last.
  #[inline]
  pub fn push_intent_back(&mut self, intent: Intent) {
    self.intents.push_back(intent);
  }
}

impl StackFrame {
  #[inline(always)]
  fn is_done(&self) -> bool {
    self.pc >= self.sequence.len()
  }
  
  #[inline]
  pub fn write_frag(&mut self, frag: &str) {
    if let Some(output) = self.output.as_mut() {
      output.write_frag(frag);
    }
  }
  
  #[inline]
  pub fn write_ws(&mut self, ws: &str) {
    if let Some(output) = self.output.as_mut() {
      output.write_ws(ws);
    }
  }

  #[inline]
  pub fn write_value(&mut self, val: RantValue) {
    if let Some(output) = self.output.as_mut() {
      output.write_buffer(OutputBuffer::Value(val));
    }
  }

  #[inline]
  pub fn render_output_value(&mut self) -> Option<RantValue> {
    self.output.take().map(|o| o.render_value())
  }
}