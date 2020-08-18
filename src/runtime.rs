use crate::{Rant, RantProgram, RantMap, lang::{Sequence, RST, PrintFlag, Identifier}, RantResult, RantError, RuntimeErrorType, RantString, random::RantRng, ToRant, RantValue, RantClosure};
use output::OutputWriter;
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
  val_stack: RefCell<Vec<RantValue>>,
  call_stack: RefCell<CallStack>,
  prev_frame: RefCell<Option<Rc<RefCell<StackFrame>>>>,
  intent: RefCell<Intent>,
  resolver: Resolver
}

impl<'rant> VM<'rant> {
  pub fn new(rng: Rc<RantRng>, engine: &'rant mut Rant, program: &'rant RantProgram) -> Self {
    Self {
      resolver: Resolver::new(&rng),
      rng,
      engine,
      program,
      intent: RefCell::new(Intent::Default),
      val_stack: Default::default(),
      call_stack: Default::default(),
      prev_frame: Default::default(),
    }
  }
  
  pub fn cur_frame(&self) -> Rc<RefCell<StackFrame>> {
    self.call_stack.borrow().last().unwrap().clone()
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
enum Intent {
  // Run frame sequence as normal.
  Default,
  // Invoke the following function using values from the stack.
  Invoke { func: Rc<RantClosure>, argc: usize, varg_start: Option<usize> },
  // Pop a value off the stack and assign it to a variable.
  SetVar { vname: Identifier }
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
  pub fn run(&self) -> RantResult<String> {
    println!("RST: {:#?}", self.program.root);

    // Push the root RST onto the stack
    self.push_frame(self.program.root.clone(), true)?;
    
    // Run whatever is on the top of the call stack
    'from_the_top: while !self.is_stack_empty() {
      let frame = self.cur_frame();
      let mut frame = frame.borrow_mut();
      
      // Write last frame's output value to current output
      if let Some(last_output) = self.pop_output() {
        frame.write_value(last_output);
      }
      
      // Run frame's sequence elements in order
      while let Some(rst) = &frame.seq_next() {
        match Rc::deref(rst) {
          RST::Fragment(frag) => frame.write_frag(frag),
          RST::Whitespace(ws) => frame.write_ws(ws),
          RST::Integer(n) => frame.write_value(RantValue::Integer(*n)),
          RST::Float(n) => frame.write_value(RantValue::Float(*n)),
          RST::EmptyVal => frame.write_value(RantValue::Empty),
          RST::Boolean(b) => frame.write_value(RantValue::Boolean(*b)),
          RST::Block(block) => {
            let elem = Rc::clone(&block.elements[self.rng.next_usize(block.len())]);
            self.push_frame(elem, block.flag != PrintFlag::Sink)?;
            continue 'from_the_top;
          },
          RST::FuncDef(fdef) => {
            
          },
          _ => {}
        }
      }
      
      // Pop frame once its sequence is finished
      self.pop_frame()?;
    }
    
    // Once stack is empty, program is done-- return last frame's output as a string
    Ok(self.pop_output_string().unwrap_or_default().to_string())
  }

  #[inline(always)]
  fn set_local(&self, key: Identifier, val: RantValue) {
    todo!()
  }

  #[inline(always)]
  fn get_local<'a>(&self, key: Identifier) -> &'a RantValue {
    todo!()
  }

  #[inline(always)]
  fn def_local(&self, key: Identifier, val: RantValue) {
    
  }
  
  #[inline(always)]
  fn is_stack_empty(&self) -> bool {
    self.call_stack.borrow().is_empty()
  }
  
  #[inline]
  fn pop_output_string(&self) -> Option<RantString> {
    self.prev_frame.borrow_mut()
    .take()
    .map(|frame| frame.borrow_mut().render_output_string())
    .flatten()
  }

  #[inline]
  fn pop_output(&self) -> Option<RantValue> {
    self.prev_frame.borrow_mut()
    .take()
    .map(|frame| frame.borrow_mut().render_output_value())
    .flatten()
  }

  #[inline]
  fn push_val(&self, val: RantValue) -> RantResult<usize> {
    let mut stack = self.val_stack.borrow_mut();
    if stack.len() < MAX_STACK_SIZE {
      stack.push(val);
      Ok(stack.len())
    } else {
      runtime_error!(StackOverflow);
    }
  }

  #[inline]
  fn pop_val(&self) -> RantResult<RantValue> {
    let mut stack = self.val_stack.borrow_mut();
    if stack.len() > 0 {
      Ok(stack.pop().unwrap())
    } else {
      runtime_error!(StackUnderflow);
    }
  }
  
  #[inline]
  fn pop_frame(&self) -> RantResult<Rc<RefCell<StackFrame>>> {
    if let Some(frame) = self.call_stack.borrow_mut().pop() {
      self.prev_frame.replace(Some(frame.clone()));
      Ok(frame)
    } else {
      runtime_error!(StackUnderflow);
    }
  }
  
  #[inline]
  fn push_frame(&self, callee: Rc<Sequence>, use_output: bool) -> RantResult<Rc<RefCell<StackFrame>>> {
    let mut stack = self.call_stack.borrow_mut();
    
    // Check if this push would overflow the stack
    if stack.len() >= MAX_STACK_SIZE {
      runtime_error!(StackOverflow);
    }
    
    let frame = StackFrame::new(callee, Default::default(), use_output);
    stack.push(Rc::new(RefCell::new(frame)));
    Ok(stack.last().unwrap().clone())
  }
}