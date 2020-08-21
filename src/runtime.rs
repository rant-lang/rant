use crate::*;
use crate::lang::*;
use std::{rc::Rc, cell::RefCell, ops::Deref};
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
/// to override its usual behavior next time it's active.
#[derive(Debug)]
pub enum Intent {
  /// Run frame sequence as normal.
  Default,
  /// Take the pending output from last frame and print it.
  PrintLastOutput,
  /// Invoke the following function using values from the stack.
  Invoke { func: Rc<RantFunction>, argc: usize, varg_start: Option<usize> },
  /// Pop a value off the stack and assign it to an existing variable.
  SetVar { vname: Identifier },
  /// Pop a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `GetValue`.
  BuildDynamicGetter { path: Rc<VarAccessPath>, expr_count: usize, pending_exprs: Vec<Rc<Block>> },
  /// Pop `expr_count` values off the stack and use them for expression fields in a getter.
  GetValue { path: Rc<VarAccessPath>, expr_count: usize },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `SetValue`.
  BuildDynamicSetter { path: Rc<VarAccessPath>, auto_def: bool, expr_count: usize, pending_exprs: Vec<Rc<Block>>, val_source: SetterValueSource },
  /// Pop `expr_count` values off the stack and use them for expression fields in a setter.
  SetValue { path: Rc<VarAccessPath>, auto_def: bool, expr_count: usize },
  /// Pop value from stack and add it to a list. If `index` is out of range, print the list.
  BuildList { init: Rc<Vec<Rc<Sequence>>>, index: usize, list: RantList },
  /// Pop value and optional key from stack and add them to a map. If `pair_index` is out of range, print the map.
  BuildMap { init: Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>, pair_index: usize, map: RantMap },
}

impl Intent {
  /// Returns true if the intent is `Default`.
  fn is_default(&self) -> bool {
    matches!(self, Intent::Default)
  }
}

#[derive(Debug)]
enum SetterKey<'a> {
  Index(i64),
  KeyRef(&'a str),
  KeyString(RantString)
}

#[derive(Debug)]
pub enum SetterValueSource {
  FromExpression(Rc<Sequence>),
  FromValue(RantValue),
}

#[inline]
fn convert_index_result(result: ValueIndexResult) -> RantResult<RantValue> {
  match result {
      Ok(val) => Ok(val),
      Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
  }
}

#[inline]
fn convert_key_result(result: ValueKeyResult) -> RantResult<RantValue> {
  match result {
    Ok(val) => Ok(val),
    Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
  }
}

#[inline]
fn convert_index_set_result(result: ValueIndexSetResult) -> RantResult<()> {
  match result {
    Ok(_) => Ok(()),
    Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
  }
}

#[inline]
fn convert_key_set_result(result: ValueKeySetResult) -> RantResult<()> {
  match result {
    Ok(_) => Ok(()),
    Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
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
      
      // Read frame's current intent and handle it before running the sequence
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
            self.push_block_frame(block.as_ref(), true)?;
          } else {
            self.cur_frame_mut().set_intent(Intent::GetValue { path, expr_count });
          }
          continue 'from_the_top;
        },
        Intent::GetValue { path, expr_count } => {
          self.get_value(path, expr_count)?;
        },
        Intent::BuildDynamicSetter { path, auto_def, expr_count, mut pending_exprs, val_source } => {
          if let Some(block) = pending_exprs.pop() {
            // Set next intent based on remaining expressions in setter
            if pending_exprs.is_empty() {
              // Set value once this frame is active again
              self.cur_frame_mut().set_intent(Intent::SetValue { path, auto_def, expr_count });
              self.push_block_frame(block.as_ref(), true)?;
            } else {
              self.cur_frame_mut().set_intent(Intent::BuildDynamicSetter { path, auto_def, expr_count, pending_exprs, val_source });
              self.push_block_frame(block.as_ref(), true)?;
              continue 'from_the_top;
            }
            
          } else {
            self.cur_frame_mut().set_intent(Intent::SetValue { path, auto_def, expr_count });
          }

          // Prepare setter value
          match val_source {
            SetterValueSource::FromExpression(expr) => {
              self.push_frame(Rc::clone(&expr), true)?;
            }
            SetterValueSource::FromValue(val) => {
              self.push_val(val)?;
            }
          }
          continue 'from_the_top;
        },
        Intent::SetValue { path, auto_def, expr_count } => {
          self.set_value(path, auto_def, expr_count)?;
        },
        Intent::BuildList { init, index, mut list } => {
          // Add latest evaluated value to list
          if index > 0 {
            list.push(self.pop_val()?);
          }

          // Check if the list is complete
          if index >= init.len() {
            self.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))))
          } else {
            // Continue list creation
            self.cur_frame_mut().set_intent(Intent::BuildList { init: Rc::clone(&init), index: index + 1, list });
            let val_expr = &init[index];
            self.push_frame(Rc::clone(val_expr), true)?;
            continue 'from_the_top;
          }
        },
        Intent::BuildMap { init, pair_index, mut map } => {
          // Add latest evaluated pair to map
          if pair_index > 0 {
            let prev_pair = &init[pair_index - 1];
            // If the key is dynamic, there are two values to pop
            let key = match prev_pair {
              (MapKeyExpr::Dynamic(_), _) => RantString::from(self.pop_val()?.to_string()),
              (MapKeyExpr::Static(key), _) => key.clone()
            };
            let val = self.pop_val()?;
            map.raw_set(key.as_str(), val);
          }

          // Check if the map is completed
          if pair_index >= init.len() {
            self.cur_frame_mut().write_value(RantValue::Map(Rc::new(RefCell::new(map))));
          } else {
            // Continue map creation
            self.cur_frame_mut().set_intent(Intent::BuildMap { init: Rc::clone(&init), pair_index: pair_index + 1, map });
            let (key_expr, val_expr) = &init[pair_index];
            if let MapKeyExpr::Dynamic(key_expr_block) = key_expr {
              // Push dynamic key expression onto call stack
              self.push_block_frame(key_expr_block, true)?;
            }
            // Push value expression onto call stack
            self.push_frame(Rc::clone(val_expr), true)?;
            continue 'from_the_top;
          }
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
          RST::ListInit(elements) => {
            self.cur_frame_mut().set_intent(Intent::BuildList { init: Rc::clone(elements), index: 0, list: RantList::new() });
            continue 'from_the_top;
          },
          RST::MapInit(elements) => {
            self.cur_frame_mut().set_intent(Intent::BuildMap { init: Rc::clone(elements), pair_index: 0, map: RantMap::new() });
            continue 'from_the_top;
          },
          RST::Block(block) => {
            self.push_block_frame(block, false)?;
            continue 'from_the_top;
          },
          RST::VarDef(vname, val_expr) => {
            if let Some(val_expr) = val_expr {
              // If a value is present, it needs to be evaluated first
              self.cur_frame_mut().set_intent(Intent::DefVar { vname: vname.clone() });
              self.push_frame(Rc::clone(val_expr), true)?;
              continue 'from_the_top;
            } else {
              // If there's no assignment, just set it to <>
              self.def_local(vname.as_str(), RantValue::Empty)?;
            }
          },
          RST::VarGet(path) => {
            // Get list of dynamic keys in path
            let exprs = path.dynamic_keys();

            if exprs.is_empty() {
              // Getter is static, so run it directly
              self.cur_frame_mut().set_intent(Intent::GetValue { path: Rc::clone(path), expr_count: 0 });
            } else {
              // Build dynamic keys before running getter
              // Push dynamic key expressions onto call stack
              self.cur_frame_mut().set_intent(Intent::BuildDynamicGetter {
                expr_count: exprs.len(),
                path: Rc::clone(path),
                pending_exprs: exprs
              });
            }
            continue 'from_the_top;
          },
          RST::VarSet(path, val_expr) => {
            // Get list of dynamic keys in path
            let exprs = path.dynamic_keys();

            if exprs.is_empty() {
              // Setter is static, so run it directly
              self.cur_frame_mut().set_intent(Intent::SetValue { path: Rc::clone(&path), auto_def: false, expr_count: 0 });
              self.push_frame(Rc::clone(&val_expr), true)?;
            } else {
              // Build dynamic keys before running setter
              // Push dynamic key expressions onto call stack
              self.cur_frame_mut().set_intent(Intent::BuildDynamicSetter {
                expr_count: exprs.len(),
                auto_def: false,
                path: Rc::clone(path),
                pending_exprs: exprs,
                val_source: SetterValueSource::FromExpression(Rc::clone(val_expr))
              });
            }
            continue 'from_the_top;
          },
          RST::FuncDef(fdef) => {
            let FunctionDef { 
              id, 
              body, 
              params, 
              capture_vars 
            } = fdef;

            let func = RantValue::Function(Rc::new(RantFunction {
              params: Rc::clone(params),
              body: RantFunctionInterface::User(Rc::clone(body)),
              captured_vars: Default::default(), // TODO: Actually capture variables, don't be lazy!
              min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
              vararg_index: params.iter().skip_while(|p| !p.varity.is_variadic()).count(),
            }));

            let dynamic_keys = id.dynamic_keys();

            self.cur_frame_mut().set_intent(Intent::BuildDynamicSetter {
              expr_count: dynamic_keys.len(),
              auto_def: true,
              pending_exprs: dynamic_keys,
              path: Rc::clone(id),
              val_source: SetterValueSource::FromValue(func)
            });

            continue 'from_the_top;
          },
          RST::Closure(closure_expr) => {
            let ClosureExpr {
              capture_vars,
              expr,
              params,
            } = closure_expr;

            let func = RantValue::Function(Rc::new(RantFunction {
              params: Rc::clone(params),
              body: RantFunctionInterface::User(Rc::clone(&expr)),
              captured_vars: Default::default(), // TODO: Capture variables on anonymous functions
              min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
              vararg_index: params.iter().skip_while(|p| !p.varity.is_variadic()).count(),
            }));

            self.cur_frame_mut().write_value(func);
          }
          rst => {
            panic!(format!("Unsupported RST: {:?}", rst));
          }
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

  fn set_value(&mut self, path: Rc<VarAccessPath>, auto_def: bool, dynamic_key_count: usize) -> RantResult<()> {
    // The setter value should be at the top of the value stack, so pop that first
    let setter_value = self.pop_val()?;

    // Gather evaluated dynamic keys from stack
    let mut dynamic_keys = vec![];
    for _ in 0..dynamic_key_count {
      dynamic_keys.push(self.pop_val()?);
    }

    let mut path_iter = path.iter();
    let mut dynamic_keys = dynamic_keys.drain(..).rev();
         
    // The setter key is the location on the setter target that will be written to.
    let mut setter_key = match path_iter.next() {
      Some(VarAccessComponent::Name(vname)) => {
        SetterKey::KeyRef(vname.as_str())
      },
      Some(VarAccessComponent::Expression(_)) => {
        let key = RantString::from(dynamic_keys.next().unwrap().to_string());
        SetterKey::KeyString(key)
      },
      _ => unreachable!()
    };

    // The setter target is the value that will be modified. If None, the setter key is a variable.
    let mut setter_target: Option<RantValue> = None;

    // Evaluate the path
    for accessor in path_iter {
      // Update setter target by keying off setter_key
      setter_target = match (&setter_target, &setter_key) {
        (None, SetterKey::KeyRef(key)) => Some(self.get_local(key)?),
        (None, SetterKey::KeyString(key)) => Some(self.get_local(key.as_str())?),
        (Some(val), SetterKey::Index(index)) => Some(convert_index_result(val.get_by_index(*index))?),
        (Some(val), SetterKey::KeyRef(key)) => Some(convert_key_result(val.get_by_key(key))?),
        (Some(val), SetterKey::KeyString(key)) => Some(convert_key_result(val.get_by_key(key.as_str()))?),
        _ => unreachable!()
      };

      setter_key = match accessor {
        // Static key
        VarAccessComponent::Name(key) => SetterKey::KeyRef(key.as_str()),
        // Index
        VarAccessComponent::Index(index) => SetterKey::Index(*index),
        // Dynamic key
        VarAccessComponent::Expression(_) => {
          match dynamic_keys.next().unwrap() {
            RantValue::Integer(index) => {
              SetterKey::Index(index)
            },
            key_val => {
              let key = RantString::from(key_val.to_string());
              SetterKey::KeyString(key)
            }
          }
        }
      }
    }

    // Finally, set the value
    match (&mut setter_target, &setter_key) {
      (None, SetterKey::KeyRef(vname)) => {
        if auto_def {
          self.def_local(vname, setter_value)?;
        } else {
          self.set_local(vname, setter_value)?;
        }
      },
      (None, SetterKey::KeyString(vname)) => {
        if auto_def {
          self.def_local(vname.as_str(), setter_value)?
        } else {
          self.set_local(vname.as_str(), setter_value)?
        }
      },
      (Some(target), SetterKey::Index(index)) => convert_index_set_result(target.set_by_index(*index, setter_value))?,
      (Some(target), SetterKey::KeyRef(key)) => convert_key_set_result(target.set_by_key(key, setter_value))?,
      (Some(target), SetterKey::KeyString(key)) => convert_key_set_result(target.set_by_key(key.as_str(), setter_value))?,
      _ => unreachable!()
    }

    Ok(())
  }

  fn get_value(&mut self, path: Rc<VarAccessPath>, dynamic_key_count: usize) -> RantResult<()> {
    // Gather evaluated dynamic keys from stack
    let mut dynamic_keys = vec![];
    for _ in 0..dynamic_key_count {
      dynamic_keys.push(self.pop_val()?);
    }

    let mut path_iter = path.iter();
    let mut dynamic_keys = dynamic_keys.drain(..);

    // Get the root variable
    let mut getter_value = match path_iter.next() {
        Some(VarAccessComponent::Name(vname)) => {
          self.get_local(vname.as_str())?
        },
        Some(VarAccessComponent::Expression(_)) => {
          let key = dynamic_keys.next().unwrap().to_string();
          self.get_local(key.as_str())?
        },
        _ => unreachable!()
    };

    // Evaluate the rest of the path
    for accessor in path_iter {
      match accessor {
        // Static key
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
        // Dynamic key
        VarAccessComponent::Expression(_) => {
          let key = dynamic_keys.next().unwrap();
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
    Ok(())
  }

  fn push_block_frame(&mut self, block: &Block, override_print: bool) -> RantResult<()> {
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
    if !self.val_stack.is_empty() {
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