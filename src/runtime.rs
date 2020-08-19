use crate::{Rant, RantProgram, lang::*, RantResult, RantError, RuntimeErrorType, RantString, random::RantRng, RantValue, RantClosure};
use std::{rc::{Rc}, cell::{RefCell}, ops::{Deref}};
use resolver::Resolver;
pub use stack::*;
pub use output::*;

mod resolver;
mod output;
mod stack;

pub const MAX_STACK_SIZE: usize = 20000;

pub struct VM<'rant> {
  rng: Rc<RantRng>,
  engine: &'rant mut Rant,
  program: &'rant RantProgram,
  val_stack: Vec<RantValue>,
  call_stack: CallStack,
  resolver: Resolver
}

impl<'rant> VM<'rant> {
  pub fn new(rng: Rc<RantRng>, engine: &'rant mut Rant, program: &'rant RantProgram) -> Self {
    Self {
      resolver: Resolver::new(&rng),
      rng,
      engine,
      program,
      val_stack: Default::default(),
      call_stack: Default::default(),
    }
  }
}

/// Returns a `RantResult::Err(RantError::RuntimeError { .. })` from the current execution context with the specified error type and optional description.
macro_rules! runtime_error {
  ($err_type:ident) => {
    return Err(RantError::RuntimeError {
      error_type: RuntimeErrorType::$err_type,
      description: None
    })
  };
  ($err_type:ident, $desc:expr) => {
    return Err(RantError::RuntimeError {
      error_type: RuntimeErrorType::$err_type,
      description: Some($desc.to_string())
    })
  };
}

/// RSTs can assign intents to the current stack frame
/// to override the its usual behavior next time it's active.
#[derive(Debug)]
pub enum Intent {
  // Run frame sequence as normal.
  Default,
  // Take the pending output from last frame and print it.
  PrintLastOutput,
  // Invoke the following function using values from the stack.
  Invoke { func: Rc<RantClosure>, argc: usize, varg_start: Option<usize> },
  // Pop a value off the stack and assign it to a variable.
  SetVar { vname: Identifier },
  // Pop a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier },
}

impl Intent {
  /// Returns true if the intent is `Default`.
  fn is_default(&self) -> bool {
    matches!(self, Intent::Default)
  }
}

impl<'rant> VM<'rant> {
  /// Runs the program.
  #[inline]
  pub fn run(&mut self) -> RantResult<String> {
    //println!("RST: {:#?}", self.program.root);

    // Push the root RST onto the stack
    self.push_frame(self.program.root.clone(), true)?;
    
    // Run whatever is on the top of the call stack
    'from_the_top: while !self.is_stack_empty() {
      
      match self.cur_frame_mut().take_intent() {
        Intent::Default => {},
        Intent::PrintLastOutput => {
          let val = self.pop_val()?;
          self.cur_frame_mut().write_value(val);
        },
        Intent::SetVar { vname } => {
          let val = self.pop_val()?;
          self.set_local(vname.as_str(), val)?;
        },
        Intent::DefVar { vname } => {
          let val = self.pop_val()?;
          self.def_local(vname.as_str(), val)?;
        }
        _ => todo!()
      }
      
      // Run frame's sequence elements in order
      while let Some(rst) = &self.cur_frame_mut().seq_next() {
        match Rc::deref(rst) {
          RST::Fragment(frag) => self.cur_frame_mut().write_frag(frag),
          RST::Whitespace(ws) => self.cur_frame_mut().write_ws(ws),
          RST::Integer(n) => self.cur_frame_mut().write_value(RantValue::Integer(*n)),
          RST::Float(n) => self.cur_frame_mut().write_value(RantValue::Float(*n)),
          RST::EmptyVal => self.cur_frame_mut().write_value(RantValue::Empty),
          RST::Boolean(b) => self.cur_frame_mut().write_value(RantValue::Boolean(*b)),
          RST::Block(block) => {
            let elem = Rc::clone(&block.elements[self.rng.next_usize(block.len())]);
            let is_printing = block.flag != PrintFlag::Sink;
            if is_printing {
              self.cur_frame_mut().set_intent(Intent::PrintLastOutput);
            }
            self.push_frame(elem, is_printing)?;
            continue 'from_the_top;
          },
          RST::VarDef(vname, val_expr) => {
            if let Some(val_expr) = val_expr {
              self.cur_frame_mut().set_intent(Intent::DefVar { vname: vname.clone() });
              self.push_frame(Rc::clone(val_expr), true)?;
              continue 'from_the_top;
            } else {
              self.def_local(vname.as_str(), RantValue::Empty)?;
            }
          },
          RST::VarGet(path) => {
            if let Some(VarAccessComponent::Name(vname)) = path.get(0) {
              let val = self.get_local(vname)?;
              self.cur_frame_mut().write_value(val);
            } else {
              todo!()
            }
          },
          RST::VarSet(path, val) => {
            todo!()
          },
          RST::FuncDef(fdef) => {
            todo!()
          },
          _ => {}
        }
      }
      
      // Pop frame once its sequence is finished
      let mut last_frame = self.pop_frame()?;
      if let Some(output) = last_frame.render_output_value() {
        self.push_val(output)?;
      }
    }
    
    // Once stack is empty, program is done-- return last frame's output as a string
    Ok(self.pop_val().unwrap_or_default().to_string())
  }

  #[inline(always)]
  fn set_local(&mut self, key: &str, val: RantValue) -> RantResult<()> {
    self.call_stack.set_local(self.engine, key, val)
  }

  #[inline(always)]
  fn get_local(&self, key: &str) -> RantResult<RantValue> {
    self.call_stack.get_local(self.engine, key)
  }

  #[inline(always)]
  fn def_local(&mut self, key: &str, val: RantValue) -> RantResult<()> {
    self.call_stack.def_local(self.engine, key, val)
  }
  
  #[inline(always)]
  fn is_stack_empty(&self) -> bool {
    self.call_stack.is_empty()
  }

  #[inline]
  fn push_val(&mut self, val: RantValue) -> RantResult<usize> {
    if self.val_stack.len() < MAX_STACK_SIZE {
      self.val_stack.push(val);
      Ok(self.val_stack.len())
    } else {
      runtime_error!(StackOverflow, "value stack has overflowed");
    }
  }

  #[inline]
  fn pop_val(&mut self) -> RantResult<RantValue> {
    if self.val_stack.len() > 0 {
      Ok(self.val_stack.pop().unwrap())
    } else {
      runtime_error!(StackUnderflow, "value stack has underflowed");
    }
  }

  #[inline]
  fn pop_frame(&mut self) -> RantResult<StackFrame> {
    if let Some(frame) = self.call_stack.pop() {
      Ok(frame)
    } else {
      runtime_error!(StackUnderflow, "call stack has underflowed");
    }
  }
  
  #[inline]
  fn push_frame(&mut self, callee: Rc<Sequence>, use_output: bool) -> RantResult<()> {
    
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(StackOverflow, "call stack has overflowed");
    }
    
    let frame = StackFrame::new(callee, Default::default(), use_output);
    self.call_stack.push(frame);
    Ok(())
  }

  pub fn cur_frame_mut(&mut self) -> &mut StackFrame {
    self.call_stack.last_mut().unwrap()
  }

  pub fn cur_frame(&self) -> &StackFrame {
    self.call_stack.last().unwrap()
  }
}