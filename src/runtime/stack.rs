use std::{rc::Rc};
use std::{collections::VecDeque};
use fnv::{FnvBuildHasher};
use quickscope::ScopeMap;
use crate::{lang::{Sequence, Rst}, RantValue, Rant};
use crate::runtime::*;
use super::{OutputBuffer, output::OutputWriter, Intent};

type CallStackVector = SmallVec<[StackFrame; super::CALL_STACK_INLINE_COUNT]>;

/// Represents a call stack and its associated locals.
pub struct CallStack {
  frames: CallStackVector,
  locals: ScopeMap<RantString, RantValue, FnvBuildHasher>,
}

// // Allows direct immutable access to internal stack vector
// impl Deref for CallStack {
//   type Target = CallStackVector;
//   fn deref(&self) -> &Self::Target {
//     &self.frames
//   }
// }

// // Allows direct mutable access to internal stack vector
// impl DerefMut for CallStack {
//   fn deref_mut(&mut self) -> &mut Self::Target {
//     &mut self.frames
//   }
// }

impl Default for CallStack {
  fn default() -> Self {
    Self::new()
  }
}

impl CallStack {
  #[inline]
  pub fn new() -> Self {
    Self {
      frames: Default::default(),
      locals: Default::default(),
    }
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.frames.is_empty()
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.frames.len()
  }

  #[inline]
  pub fn pop_frame(&mut self) -> Option<StackFrame> {
    if let Some(frame) = self.frames.pop() {
      self.locals.pop_layer();
      return Some(frame)
    }
    None
  }

  #[inline]
  pub fn push_frame(&mut self, frame: StackFrame) {
    self.locals.push_layer();
    self.frames.push(frame);
  }

  #[inline]
  pub fn top_mut(&mut self) -> Option<&mut StackFrame> {
    self.frames.last_mut()
  }

  #[inline]
  pub fn top(&self) -> Option<&StackFrame> {
    self.frames.last()
  }

  pub fn gen_stack_trace(&self) -> String {
    let mut trace = String::new();
    for frame in self.frames.iter().rev() {
      trace.push_str(format!("-> {}\n", frame).as_str());
    }
    trace
  }

  #[inline]
  pub fn set_local(&mut self, context: &Rant, id: &str, access: AccessPathKind, val: RantValue) -> RuntimeResult<()> {
    match access {
      AccessPathKind::Local => {
        if let Some(var) = self.locals.get_mut(id) {
          *var = val;
          return Ok(())
        }
      },
      AccessPathKind::Descope(n) => {
        if let Some(var) = self.locals.get_parent_mut(id, n) {
          *var = val;
          return Ok(())
        }
      },
      // Skip locals completely if it's a global accessor
      AccessPathKind::ExplicitGlobal => {}
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
      stack_trace: None,
    })
  }

  #[inline]
  pub fn get_local(&self, context: &Rant, id: &str, access: AccessPathKind) -> RuntimeResult<RantValue> {
    match access {
      AccessPathKind::Local => {
        if let Some(val) = self.locals.get(id) {
          return Ok(val.clone())
        }
      },
      AccessPathKind::Descope(n) => {
        if let Some(val) = self.locals.get_parent(id, n) {
          return Ok(val.clone())
        }
      },
      AccessPathKind::ExplicitGlobal => {},
    }    

    // Check globals
    if let Some(val) = context.globals.borrow().raw_get(id) {
      return Ok(val.clone())
    }

    Err(RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: format!("variable '{}' not found", id),
      stack_trace: None,
    })
  }

  #[inline]
  pub fn def_local(&mut self, context: &Rant, id: &str, access: AccessPathKind, val: RantValue) -> RuntimeResult<()> {
    match access {
      AccessPathKind::Local => {
        self.locals.define(RantString::from(id), val);
        return Ok(())
      },
      AccessPathKind::Descope(descope_count) => {
        self.locals.define_parent(RantString::from(id), val, descope_count);
        return Ok(())
      },
      AccessPathKind::ExplicitGlobal => {}
    }
    
    context.globals.borrow_mut().raw_set(id, val);
    Ok(())
  }
}

pub struct StackFrame {
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
  // Line/col for debug info
  debug_pos: (usize, usize),
  // Source name for debug info
  origin: Rc<RantString>,
}

impl StackFrame {
  pub fn new(sequence: Rc<Sequence>, has_output: bool, prev_output: Option<&OutputWriter>) -> Self {
    Self {
      origin: Rc::clone(&sequence.origin),
      sequence,
      output: if has_output { Some(OutputWriter::new(prev_output)) } else { None },
      started: false,
      pc: 0,
      intents: Default::default(),
      debug_pos: (0, 0),
    }
  }
}

impl StackFrame {
  #[inline]
  pub fn seq_next(&mut self) -> Option<Rc<Rst>> {
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

  #[inline]
  pub fn output(&self) -> Option<&OutputWriter> {
    self.output.as_ref()
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

  #[inline]
  pub fn use_output_mut<F: FnOnce(&mut OutputWriter)>(&mut self, func: F) {
    if let Some(output) = self.output.as_mut() {
      func(output);
    }
  }

  #[inline]
  pub fn use_output<'a, F: FnOnce(&'a OutputWriter) -> R, R>(&'a self, func: F) -> Option<R> {
    self.output.as_ref().map(|output| func(output))
  }

  /// Writes debug information to the current frame to be used in stack trace generation.
  #[inline]
  pub fn set_debug_info(&mut self, info: &DebugInfo) {
    match info {
      DebugInfo::Location { line, col } => self.debug_pos = (*line, *col),
    }
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
    if val.is_empty() {
      return
    }
    if let Some(output) = self.output.as_mut() {
      output.write_buffer(OutputBuffer::Value(val));
    }
  }

  #[inline]
  pub fn render_output_value(&mut self) -> Option<RantValue> {
    self.output.take().map(|o| o.render_value())
  }
}

impl Display for StackFrame {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[{}:{}:{}] in {}", 
      self.origin.as_ref(), 
      self.debug_pos.0, 
      self.debug_pos.1,
      self.sequence.name().map(|name| name.as_str()).unwrap_or("???"), 
    )
  }
}