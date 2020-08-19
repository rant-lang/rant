use std::{cell::RefCell, rc::Rc, ops::{DerefMut, Deref}};
use crate::{lang::{Sequence, RST, Identifier}, RantMap, RantString, RantValue, RantResult, RantError, RuntimeErrorType, Rant};
use super::{OutputBuffer, output::OutputWriter, Intent};

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

  pub fn set_local(&mut self, context: &Rant, id: &str, val: RantValue) -> RantResult<()> {

    // Check locals
    for frame in self.iter_mut().rev() {
      let mut frame = frame.borrow_mut();
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

    Err(RantError::RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: Some(format!("variable '{}' not found", id)),
    })
  }

  pub fn get_local(&self, context: &Rant, id: &str) -> RantResult<RantValue> {
    // Check locals
    for frame in self.iter().rev() {
      let frame = frame.borrow();
      if let Some(val) = frame.locals.raw_get(id) {
        return Ok(val.clone())
      }
    }

    // Check globals
    if let Some(val) = context.globals.borrow().raw_get(id) {
      return Ok(val.clone())
    }

    Err(RantError::RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: Some(format!("variable '{}' not found", id))
    })
  }

  pub fn def_local(&mut self, context: &Rant, id: &str, val: RantValue) -> RantResult<()> {
    if self.len() > 1 {
      if let Some(frame) = self.last_mut() {
        let mut frame = frame.borrow_mut();
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
  /// Output for this stack frame
  output: Option<OutputWriter>,
  /// Current intent of the frame
  intent: Intent,
}

impl StackFrame {
  pub fn new(sequence: Rc<Sequence>, locals: RantMap, has_output: bool) -> Self {
    Self {
      sequence,
      locals,
      output: if has_output { Some(Default::default()) } else { None },
      started: false,
      pc: 0,
      intent: Intent::Default,
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

  pub fn intent(&self) -> &Intent {
    &self.intent
  }

  pub fn set_intent(&mut self, intent: Intent) {
    self.intent = intent;
  }

  pub fn reset_intent(&mut self) {
    self.intent = Intent::Default;
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
  
  #[inline]
  pub fn render_output_string(&mut self) -> Option<RantString> {
    self.output.take().map(|o| o.render_string())
  }
}