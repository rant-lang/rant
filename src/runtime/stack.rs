use std::{rc::Rc};
use fnv::{FnvBuildHasher};
use quickscope::ScopeMap;
use crate::{lang::{Sequence, Expression}, RantValue, Rant};
use crate::runtime::*;
use super::{output::OutputWriter};

type CallStackVector<I> = SmallVec<[StackFrame<I>; super::CALL_STACK_INLINE_COUNT]>;

/// Represents a call stack and its associated locals.
pub struct CallStack<I> {
  frames: CallStackVector<I>,
  locals: ScopeMap<InternalString, RantVar, FnvBuildHasher>,
}

impl<I> Default for CallStack<I> {
  #[inline]
  fn default() -> Self {
    Self::new()
  }
}

impl<I> CallStack<I> {
  #[inline]
  pub(crate) fn new() -> Self {
    Self {
      frames: Default::default(),
      locals: Default::default(),
    }
  }

  /// Returns `true` if the stack is empty.
  #[inline]
  pub fn is_empty(&self) -> bool {
    self.frames.is_empty()
  }

  /// Gets the number of frames in the stack.
  #[inline]
  pub fn len(&self) -> usize {
    self.frames.len()
  }

  /// Removes the topmost frame from the stack and returns it.
  #[inline]
  pub fn pop_frame(&mut self) -> Option<StackFrame<I>> {
    if let Some(frame) = self.frames.pop() {
      if frame.has_scope {
        self.locals.pop_layer();
      }
      return Some(frame)
    }
    None
  }

  /// Adds a frame to the top of the stack.
  #[inline]
  pub fn push_frame(&mut self, frame: StackFrame<I>) {
    if frame.has_scope {
      self.locals.push_layer();
    }
    self.frames.push(frame);
  }

  /// Returns a mutable reference to the topmost frame in the stack.
  #[inline]
  pub fn top_mut(&mut self) -> Option<&mut StackFrame<I>> {
    self.frames.last_mut()
  }


  /// Returns a mutable reference to the frame `depth` frames below the top of the stack.
  #[inline]
  pub fn parent_mut(&mut self, depth: usize) -> Option<&mut StackFrame<I>> {
    self.frames.iter_mut().rev().nth(depth)
  }

  /// Returns a reference to the frame `depth` frames below the top of the stack.
  #[inline]
  pub fn parent(&self, depth: usize) -> Option<&StackFrame<I>> {
    self.frames.iter().rev().nth(depth)
  }

  /// Returna reference to the topmost frame in the stack.
  #[inline]
  pub fn top(&self) -> Option<&StackFrame<I>> {
    self.frames.last()
  }

  /// Generates a stack trace string from the current state of the stack.
  pub fn gen_stack_trace(&self) -> String {
    let mut trace = String::new();
    let mut last_frame_info: Option<(String, usize)> = None;
    for frame in self.frames.iter().rev() {
      let current_frame_string = frame.to_string();

      if let Some((last_frame_string, count)) = last_frame_info.take() {
        if current_frame_string == last_frame_string {
          last_frame_info = Some((last_frame_string, count + 1));
        } else {
          // spit out last repeated frame
          match count {
            1 => trace.push_str(&format!("-> {}\n", last_frame_string)),
            _ => trace.push_str(&format!("-> {} ({} frames)\n", last_frame_string, count)),
          }
          last_frame_info = Some((current_frame_string, 1));
        }
      } else {
        last_frame_info = Some((current_frame_string, 1));
      }
    }

    // emit bottom frame
    if let Some((last_frame_string, count)) = last_frame_info.take() {
      match count {
        1 => trace.push_str(&format!("-> {}", last_frame_string)),
        _ => trace.push_str(&format!("-> {} ({} frames)", last_frame_string, count)),
      }
    }

    trace
  }

  /// Sets a variable's value using the specified access type.
  #[inline]
  pub fn set_var_value(&mut self, context: &mut Rant, id: &str, access: VarAccessMode, val: RantValue) -> RuntimeResult<()> {
    match access {
      VarAccessMode::Local => {
        if let Some(var) = self.locals.get_mut(id) {
          if !var.write(val) {
            runtime_error!(RuntimeErrorType::InvalidAccess, "cannot reassign local constant '{}'", id);
          }
          return Ok(())
        }
      },
      VarAccessMode::Descope(n) => {
        if let Some(var) = self.locals.get_parent_mut(id, n) {
          if !var.write(val) {
            runtime_error!(RuntimeErrorType::InvalidAccess, "cannot reassign local constant '{}'", id);
          }
          return Ok(())
        }
      },
      // Skip locals completely if it's a global accessor
      VarAccessMode::ExplicitGlobal => {}
    }

    // Check globals
    if context.has_global(id) {
      if !context.set_global(id, val) {
        runtime_error!(RuntimeErrorType::InvalidAccess, "cannot reassign global constant '{}'", id);
      }
      return Ok(())
    }

    runtime_error!(RuntimeErrorType::InvalidAccess, "variable '{}' not found", id);
  }

  #[inline]
  pub fn get_var_depth(&self, context: &Rant, id: &str, access: VarAccessMode) -> RuntimeResult<usize> {
    match access {
      VarAccessMode::Local => {
        if let Some(d) = self.locals.depth_of(id) {
          return Ok(d)
        }
      },
      VarAccessMode::Descope(n) => {
        if let Some((_, d)) = self.locals.get_parent_depth(id, n) {
          return Ok(d)
        }
      },
      // Skip locals completely if it's a global accessor
      VarAccessMode::ExplicitGlobal => {}
    }

    // Check globals
    if context.has_global(id) {
      return Ok(self.locals.depth() - 1)
    }

    runtime_error!(RuntimeErrorType::InvalidAccess, "variable '{}' not found", id);
  }

  /// Gets a variable's value using the specified access type.
  #[inline]
  pub fn get_var_value(&self, context: &Rant, id: &str, access: VarAccessMode, prefer_function: bool) -> RuntimeResult<RantValue> {

    macro_rules! percolating_func_lookup {
      ($value_iter:expr) => {
        if let Some(mut vars) = $value_iter {
          if let Some(mut var) = vars.next() {
            // If the topmost value isn't callable, check the whole pile and then globals for something that is
            if !var.value_ref().is_callable() {
              if let Some(func_var) = vars
              .find(|v| v.value_ref().is_callable())
              .or_else(|| context.get_global_var(id).filter(|v| v.value_ref().is_callable())) 
              {
                var = func_var;
              }
            }
            return Ok(var.value_cloned())
          }
        }
      }
    }

    match access {
      VarAccessMode::Local => {
        // If the caller requested a function, perform function percolation
        if prefer_function {
          percolating_func_lookup!(self.locals.get_all(id));
        } else if let Some(var) = self.locals.get(id) {
          return Ok(var.value_cloned())
        }
      },
      VarAccessMode::Descope(n) => {
        if prefer_function {
          percolating_func_lookup!(self.locals.get_parents(id, n));
        } else if let Some(var) = self.locals.get_parent(id, n) {
          return Ok(var.value_cloned())
        }
      },
      VarAccessMode::ExplicitGlobal => {},
    }    

    // Check globals
    if let Some(val) = context.get_global(id) {
      return Ok(val)
    }

    Err(RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: Some(format!("{} '{}' not found", if prefer_function { "function" } else { "variable" }, id)),
      stack_trace: None,
    })
  }

  /// Gets a mutable reference to a variable.
  pub fn get_var_mut<'a>(&'a mut self, context: &'a mut Rant, id: &str, access: VarAccessMode) -> RuntimeResult<&'a mut RantVar> {
    match access {
      VarAccessMode::Local => {
        if let Some(var) = self.locals.get_mut(id) {
          return Ok(var)
        }
      },
      VarAccessMode::Descope(n) => {
        if let Some(var) = self.locals.get_parent_mut(id, n) {
          return Ok(var)
        }
      },
      VarAccessMode::ExplicitGlobal => {},
    }    

    // Check globals
    if let Some(var) = context.get_global_var_mut(id) {
      return Ok(var)
    }

    Err(RuntimeError {
      error_type: RuntimeErrorType::InvalidAccess,
      description: Some(format!("variable '{}' not found", id)),
      stack_trace: None,
    })
  }

  /// Defines a local variable of the specified name.
  ///
  /// ## Notes
  ///
  /// This function does not perform any identifier validation.
  pub fn def_local_var(&mut self, id: &str, var: RantVar) -> RuntimeResult<()> {
    self.locals.define(InternalString::from(id), var);
    Ok(())
  }

  /// Defines a variable of the specified name by-value.
  ///
  /// ## Notes
  ///
  /// This function does not perform any identifier validation.
  #[inline]
  pub fn def_var_value(&mut self, context: &mut Rant, id: &str, access: VarAccessMode, val: RantValue, is_const: bool) -> RuntimeResult<()> {
    match access {
      VarAccessMode::Local => {
        // Don't allow redefs of local constants
        if let Some(v) = self.locals.get(id) {
          if v.is_const() && self.locals.depth_of(id) == Some(0) {
            runtime_error!(RuntimeErrorType::InvalidAccess, "attempted to redefine local constant '{}'", id);
          }
        }

        let variable = if is_const { RantVar::ByValConst(val) } else { RantVar::ByVal(val) };
        self.locals.define(InternalString::from(id), variable);
        return Ok(())
      },
      VarAccessMode::Descope(descope_count) => {
        // Don't allow redefs of parent constants
        if let Some((v, vd)) = self.locals.get_parent_depth(id, descope_count) {
          if v.is_const() && vd == descope_count {
            runtime_error!(RuntimeErrorType::InvalidAccess, "attempted to redefine parent constant '{}'", id);
          }
        }

        let variable = if is_const { RantVar::ByValConst(val) } else { RantVar::ByVal(val) };
        self.locals.define_parent(InternalString::from(id), variable, descope_count);
        return Ok(())
      },
      VarAccessMode::ExplicitGlobal => {}
    }
    
    // Don't allow redefs of global constants
    if !(if is_const { context.set_global_const(id, val) } else { context.set_global(id, val) }) {
      runtime_error!(RuntimeErrorType::InvalidAccess, "attempted to redefine global constant '{}'", id);
    }

    Ok(())
  }

  /// Scans ("tastes") the stack from the top looking for the first occurrence of the specified frame flavor.
  /// Returns the top-relative index of the first occurrence, or `None` if no match was found or a stronger flavor was found first.
  #[inline]
  pub fn taste_for_first(&self, target_flavor: StackFrameFlavor) -> Option<usize> {
    for (frame_index, frame) in self.frames.iter().rev().enumerate() {
      if frame.flavor > target_flavor {
        return None
      } else if frame.flavor == target_flavor {
        return Some(frame_index)
      }
    }
    None
  }

  /// Scans ("tastes") the stack from the top looking for the first occurrence of the specified frame flavor.
  /// Returns the top-relative index of the first occurrence, or `None` if no match was found or another flavor was found first.
  #[inline]
  pub fn taste_for(&self, target_flavor: StackFrameFlavor) -> Option<usize> {
    for (frame_index, frame) in self.frames.iter().rev().enumerate() {
      if frame.flavor == target_flavor {
        return Some(frame_index)
      }
    }
    None
  }
}

/// Represents a call stack frame.
pub struct StackFrame<I> {
  /// Node sequence being executed by the frame
  sequence: Option<Rc<Sequence>>,
  /// Does this stack frame have a variable scope?
  has_scope: bool,
  /// Program Counter (as index in sequence) for the current frame
  pc: usize,
  /// Has frame sequence started running?
  /// TODO: Get rid of this field.
  started: bool,
  /// Output for the frame
  output: OutputWriter,
  /// Intent queue for the frame
  intents: Vec<I>,
  /// Line/col for debug info
  debug_pos: (usize, usize),
  /// Origin of sequence
  origin: Rc<RantProgramInfo>,
  /// A usage hint provided by the program element that created the frame.
  flavor: StackFrameFlavor,
}

impl<I> StackFrame<I> {
  #[inline]
  pub(crate) fn new(sequence: Rc<Sequence>, prev_output: Option<&OutputWriter>) -> Self {
    Self {
      origin: Rc::clone(&sequence.origin),
      sequence: Some(sequence),
      output: OutputWriter::new(prev_output),
      started: false,
      has_scope: true,
      pc: 0,
      intents: Default::default(),
      debug_pos: (0, 0),
      flavor: Default::default(),
    }
  }

  #[inline]
  pub(crate) fn with_extended_config(
    sequence: Option<Rc<Sequence>>,
    prev_output: Option<&OutputWriter>, 
    origin: Rc<RantProgramInfo>, 
    has_scope: bool,
    debug_pos: (usize, usize),
    flavor: StackFrameFlavor
  ) -> Self 
  {
    Self {
      origin,
      sequence,
      output: OutputWriter::new(prev_output),
      started: false,
      has_scope,
      pc: 0,
      intents: Default::default(),
      debug_pos,
      flavor,
    }
  }

  pub(crate) fn without_scope(self) -> Self {
    let mut frame = self;
    frame.has_scope = false;
    frame
  }

  #[inline(always)]
  pub(crate) fn with_flavor(self, flavor: StackFrameFlavor) -> Self {
    let mut frame = self;
    frame.flavor = flavor;
    frame
  }
}

impl<I> StackFrame<I> {
  #[inline]
  pub(crate) fn seq_next(&mut self) -> Option<Rc<Expression>> {
    if self.is_done() {
      return None
    }
    
    // Increment PC
    if self.started {
      self.pc += 1;
    } else {
      self.started = true;
    }
    
    self.sequence.as_ref().and_then(|seq| seq.get(self.pc).map(Rc::clone))
  }
  
  /// Gets the Program Counter (PC) for the frame.
  #[inline(always)]
  pub fn pc(&self) -> usize {
    self.pc
  }

  /// Gets the flavor of the frame.
  #[inline(always)]
  pub fn flavor(&self) -> StackFrameFlavor {
    self.flavor
  }

  #[inline(always)]
  pub fn output(&self) -> &OutputWriter {
    &self.output
  }

  #[inline(always)]
  pub fn output_mut(&mut self) -> &mut OutputWriter {
    &mut self.output
  }

  #[inline]
  pub fn render_and_reset_output(&mut self) -> RantValue {
    let mut other = OutputWriter::new(Some(&self.output));
    std::mem::swap(&mut self.output, &mut other);
    other.render_value()
  }

  #[inline(always)]
  pub fn origin(&self) -> &Rc<RantProgramInfo> {
    &self.origin
  }

  #[inline(always)]
  pub fn debug_pos(&self) -> (usize, usize) {
    self.debug_pos
  }

  #[inline]
  pub fn origin_name(&self) -> &str {
    self.origin.path
      .as_deref()
      .unwrap_or_else(|| 
        self.origin.name
          .as_deref()
          .unwrap_or(DEFAULT_PROGRAM_NAME)
      )
  }

  /// Pushes an intent to the top of the intent stack.
  #[inline(always)]
  pub fn push_intent(&mut self, intent: I) {
    self.intents.push(intent);
  }

  /// Removes the topmost intent from the intent stack and returns it.
  #[inline(always)]
  pub(crate) fn pop_intent(&mut self) -> Option<I> {
    self.intents.pop()
  }

  /// Writes debug information to the current frame to be used in stack trace generation.
  #[inline]
  pub fn set_debug_info(&mut self, info: &DebugInfo) {
    match info {
      DebugInfo::Location { line, col } => self.debug_pos = (*line, *col),
    }
  }
}

impl<I> StackFrame<I> {
  #[inline(always)]
  fn is_done(&self) -> bool {
    self.sequence.is_none() || self.pc >= self.sequence.as_ref().unwrap().len()
  }
  
  /// Writes a fragment to the frame's output.
  #[inline]
  pub fn write_frag(&mut self, frag: &str) {
    self.output.write_frag(frag);
  }
  
  /// Writes a whitespace string to the frame's output.
  #[inline]
  pub fn write_ws(&mut self, ws: &str) {
    self.output.write_ws(ws);
  }

  /// Writes a value to the frame's output.
  #[inline]
  pub fn write<T: IntoRant>(&mut self, val: T) {
    self.output.write_value(val.into_rant());
  }

  /// Consumes the frame's output and returns the final value generated by it.
  #[inline]
  pub fn into_output(self) -> RantValue {
    self.output.render_value()
  }
}

impl<I> Display for StackFrame<I> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[{}:{}:{}] in {}", 
      self.origin_name(), 
      self.debug_pos.0, 
      self.debug_pos.1,
      self.sequence.as_ref()
        .and_then(|seq| seq.name().map(|name| name.as_str()))
        .unwrap_or_else(|| self.flavor.name()), 
    )
  }
}

/// Hints at what kind of program element a specific stack frame represents.
///
/// The runtime can use this information to find where to unwind the call stack to on specific operations like breaking, returning, etc.
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum StackFrameFlavor {
  /// Nothing special.
  Original,
  /// Native function call.
  NativeCall,
  /// Frame is used for a block element.
  BlockElement,
  /// Frame is used for a repeater element.
  RepeaterElement,
  /// Frame is used for the body of a function.
  FunctionBody,
  /// Frame is used to evaluate a dynamic key.
  DynamicKeyExpression,
  /// Frame is used to evaluate a function argument.
  ArgumentExpression,
}

impl Default for StackFrameFlavor {
  fn default() -> Self {
    Self::Original
  }
}

impl StackFrameFlavor {
  fn name(&self) -> &'static str {
    match self {
      Self::Original => "sequence",
      Self::NativeCall => "native call",
      Self::BlockElement => "block element",
      Self::RepeaterElement => "repeater element",
      Self::FunctionBody => "function body",
      Self::DynamicKeyExpression => "dynamic key",
      Self::ArgumentExpression => "argument",
    }
  }
}