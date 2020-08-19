use crate::{Rant, RantProgram, lang::*, RantResult, RantError, RuntimeErrorType, random::RantRng, RantValue, RantClosure, RantList};
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
  ($err_type:expr) => {
    return Err(RantError::RuntimeError {
      error_type: $err_type,
      description: None
    })
  };
  ($err_type:expr, $desc:expr) => {
    return Err(RantError::RuntimeError {
      error_type: $err_type,
      description: Some($desc.to_string())
    })
  };
}

/// RSTs can assign intents to the current stack frame
/// to override the its usual behavior next time it's active.
#[derive(Debug)]
pub enum Intent {
  /// Run frame sequence as normal.
  Default,
  /// Take the pending output from last frame and print it.
  PrintLastOutput,
  /// Invoke the following function using values from the stack.
  Invoke { func: Rc<RantClosure>, argc: usize, varg_start: Option<usize> },
  /// Pop a value off the stack and assign it to a variable.
  SetVar { vname: Identifier },
  /// Pop a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions yet, switch intent to `GetValue` 
  BuildDynamicGetter { path: Rc<VarAccessPath>, expr_count: usize, pending_exprs: Vec<Rc<Block>> },
  /// Pop `expr_count` values off the stack and use them for expression fields in a getter.
  GetValue { path: Rc<VarAccessPath>, expr_count: usize },
  /// Pop `size` values off the stack, put them in a list, then push the list to the stack.
  NewList { size: usize },
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
      
      // Handle intent
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
        },
        Intent::BuildDynamicGetter { path, expr_count, mut pending_exprs } => {
          if let Some(block) = pending_exprs.pop() {
            // Set next intent based on remaining expressions in getter
            if pending_exprs.is_empty() {
              self.cur_frame_mut().set_intent(Intent::GetValue { path, expr_count });
            } else {
              self.cur_frame_mut().set_intent(Intent::BuildDynamicGetter { path, expr_count, pending_exprs });
            }
            self.push_block(block.as_ref(), true)?;
          } else {
            self.cur_frame_mut().set_intent(Intent::GetValue { path, expr_count });
          }
          continue 'from_the_top;
        },
        Intent::GetValue { path, expr_count } => {
          // Gather dynamic accessor values from stack
          let mut expr_values = vec![];
          for _ in 0..expr_count {
            expr_values.push(self.pop_val()?);
          }

          let mut path_iter = path.iter();
          let mut expr_values = expr_values.drain(..);

          // Get the root variable
          let mut getter_value = match path_iter.next() {
              Some(VarAccessComponent::Name(vname)) => {
                self.get_local(vname.as_str())?
              },
              Some(VarAccessComponent::Expression(_)) => {
                let key = expr_values.next().unwrap().to_string();
                self.get_local(key.as_str())?
              },
              _ => unreachable!()
          };

          // Evaluate the rest of the path
          for accessor in path_iter {
            match accessor {
              // Key
              VarAccessComponent::Name(key) => {
                getter_value = match getter_value.get_by_key(key.as_str()) {
                  Ok(val) => val,
                  Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
                };
              },
              // Index
              VarAccessComponent::Index(index) => {
                getter_value = match getter_value.get_by_index(*index) {
                  Ok(val) => val,
                  Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
                }
              },
              // Key by expression
              VarAccessComponent::Expression(_) => {
                let key = expr_values.next().unwrap();
                match key {
                  RantValue::Integer(index) => {
                    getter_value = match getter_value.get_by_index(index) {
                      Ok(val) => val,
                      Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
                    }
                  },
                  _ => {
                    getter_value = match getter_value.get_by_key(key.to_string().as_str()) {
                      Ok(val) => val,
                      Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
                    };
                  }
                }
              }
            }
          }

          self.cur_frame_mut().write_value(getter_value);
        },
        Intent::NewList { size } => {
          let mut list = RantList::with_capacity(size);
          for _ in 0..size {
            list.push(self.pop_val()?);
          }
          self.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
        },
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
          RST::ListInit(elements) => {
            // TODO: Probably don't push all list element expressions onto the stack at once
            self.cur_frame_mut().set_intent(Intent::NewList { size: elements.len() });
            for expr in elements.iter() {
              self.push_frame(Rc::clone(expr), true)?;
            }
            continue 'from_the_top;
          },
          RST::Block(block) => {
            self.push_block(block, false)?;
            continue 'from_the_top;
          },
          RST::VarDef(vname, val_expr) => {
            if let Some(val_expr) = val_expr {
              self.cur_frame_mut().set_intent(Intent::DefVar { vname: vname.clone() });
              self.push_frame(Rc::clone(val_expr), true)?;
              continue 'from_the_top;
            } else {
              // If there's no assignment, just set it to <>
              self.def_local(vname.as_str(), RantValue::Empty)?;
            }
          },
          RST::VarGet(path) => {
            // Get a list of dynamic accessors
            let exprs = path.iter().filter_map(|c| {
              match c {
                VarAccessComponent::Expression(expr) => Some(Rc::clone(expr)),
                _ => None
              }
            }).collect::<Vec<Rc<Block>>>();

            if exprs.is_empty() {
              // Getter is static, so run it directly
              self.cur_frame_mut().set_intent(Intent::GetValue { path: Rc::clone(path), expr_count: 0 });
            } else {
              // Build dynamic keys before running getter
              // Push dynamic accessors onto call stack
              self.cur_frame_mut().set_intent(Intent::BuildDynamicGetter {
                expr_count: exprs.len(),
                path: Rc::clone(path),
                pending_exprs: exprs
              });
            }
            
            continue 'from_the_top;
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

  fn push_block(&mut self, block: &Block, override_print: bool) -> RantResult<()> {
    let elem = Rc::clone(&block.elements[self.rng.next_usize(block.len())]);
    let is_printing = block.flag != PrintFlag::Sink;
    if is_printing && !override_print {
      self.cur_frame_mut().set_intent(Intent::PrintLastOutput);
    }
    self.push_frame(elem, is_printing)?;
    Ok(())
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
      runtime_error!(RuntimeErrorType::StackOverflow, "value stack has overflowed");
    }
  }

  #[inline]
  fn pop_val(&mut self) -> RantResult<RantValue> {
    if self.val_stack.len() > 0 {
      Ok(self.val_stack.pop().unwrap())
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "value stack has underflowed");
    }
  }

  #[inline]
  fn pop_frame(&mut self) -> RantResult<StackFrame> {
    if let Some(frame) = self.call_stack.pop() {
      Ok(frame)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "call stack has underflowed");
    }
  }
  
  #[inline]
  fn push_frame(&mut self, callee: Rc<Sequence>, use_output: bool) -> RantResult<()> {
    
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let frame = StackFrame::new(callee, Default::default(), use_output);
    self.call_stack.push(frame);
    Ok(())
  }

  #[inline(always)]
  pub fn cur_frame_mut(&mut self) -> &mut StackFrame {
    self.call_stack.last_mut().unwrap()
  }

  #[inline(always)]
  pub fn cur_frame(&self) -> &StackFrame {
    self.call_stack.last().unwrap()
  }
}