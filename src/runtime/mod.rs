pub(crate) mod resolver;
mod error;
mod intent;
mod output;
mod stack;

use crate::*;
use crate::lang::*;
use crate::util::*;
use self::resolver::*;

pub use self::intent::*;
pub use self::stack::*;
pub use self::error::*;

use std::{fmt::{Debug, Display}, ops::Deref, rc::Rc};
use smallvec::{SmallVec, smallvec};

/// The largest possible stack size before a stack overflow error is raised by the runtime.
pub const MAX_STACK_SIZE: usize = 20000;

pub(crate) const CALL_STACK_INLINE_COUNT: usize = 4;
pub(crate) const VALUE_STACK_INLINE_COUNT: usize = 4;

/// The Rant Virtual Machine.
pub struct VM<'rant> {
  rng_stack: SmallVec<[Rc<RantRng>; 1]>,
  engine: &'rant mut Rant,
  program: &'rant RantProgram,
  val_stack: SmallVec<[RantValue; VALUE_STACK_INLINE_COUNT]>,
  call_stack: CallStack<Intent>,
  resolver: Resolver,
  // TODO: Store unwind states in call stack
  unwinds: SmallVec<[UnwindState; 1]>,
}

impl<'rant> VM<'rant> {
  #[inline]
  pub(crate) fn new(rng: Rc<RantRng>, engine: &'rant mut Rant, program: &'rant RantProgram) -> Self {
    Self {
      resolver: Resolver::new(&rng),
      rng_stack: smallvec![rng],
      engine,
      program,
      val_stack: Default::default(),
      call_stack: Default::default(),
      unwinds: Default::default(),
    }
  }
}

/// Feature-gated stderr print function for providing diagnostic information on the Rant VM state.
///
/// Enable the `vm-trace` feature to use.
macro_rules! runtime_trace {
  ($fmt:literal) => {#[cfg(all(feature = "vm-trace", debug_assertions))]{
    eprintln!("[vm-trace] {}", $fmt)
  }};
  ($fmt:literal, $($args:expr),+) => {#[cfg(all(feature = "vm-trace", debug_assertions))]{
    eprintln!("[vm-trace] {}", format!($fmt, $($args),+))
  }};
}

/// Returns a runtime error from the current execution context with the specified error type and optional description.
macro_rules! runtime_error {
  ($err_type:expr) => {{
    let e = $err_type;
    return Err(RuntimeError {
      error_type: e,
      description: None,
      stack_trace: None,
    })
  }};
  ($err_type:expr, $desc:expr) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: Some($desc.to_string()),
      stack_trace: None,
    })
  };
}

pub struct UnwindState {
  pub handler: Option<RantFunctionHandle>,
  pub value_stack_size: usize,
  pub block_stack_size: usize,
  pub attr_stack_size: usize,
  pub call_stack_size: usize,
}

impl<'rant> VM<'rant> {
  /// Runs the program.
  pub(crate) fn run(&mut self) -> RuntimeResult<RantValue> {
    let mut result = self.run_inner();
    // On error, generate stack trace
    if let Err(err) = result.as_mut() {
      err.stack_trace = Some(self.call_stack.gen_stack_trace());
    }
    result
  }

  /// Runs the program with arguments.
  pub(crate) fn run_with<A>(&mut self, args: A) -> RuntimeResult<RantValue> 
  where A: Into<Option<HashMap<String, RantValue>>>
  {
    if let Some(args) = args.into() {
      for (k, v) in args {
        self.def_var_value(&k, AccessPathKind::Local, v, true)?;
      }
    }

    let mut result = self.run_inner();
    // On error, generate stack trace
    if let Err(err) = result.as_mut() {
      err.stack_trace = Some(self.call_stack.gen_stack_trace());
    }
    result
  }
  
  #[inline]
  fn run_inner(&mut self) -> RuntimeResult<RantValue> {
    runtime_trace!("AST: {:#?}", &self.program.root);

    // Push the program's root sequence onto the call stack
    // This doesn't need an overflow check because it will *always* succeed
    self.push_frame_unchecked(self.program.root.clone(), StackFrameFlavor::FunctionBody);
    
    while !self.is_stack_empty() {
      // Tick VM
      match self.tick() {
        Ok(true) => {
          runtime_trace!("tick interrupted (stack @ {})", self.call_stack.len());
          continue
        },
        Ok(false) => {
          runtime_trace!("tick done (stack @ {})", self.call_stack.len());
        },
        Err(err) => {
          // Try to unwind to last safe point
          if let Some(unwind) = self.unwind() {
            // Fire off handler if available
            if let Some(handler) = unwind.handler {
              self.call_func(handler, vec![RantValue::String(err.to_string().into())], false)?;
              continue;
            }
          } else {
            return Err(err)
          }
        }
      }
    }

    // Value stack should *always* be 1 when program ends.
    debug_assert_eq!(self.val_stack.len(), 1, "value stack is unbalanced");
    
    // Once stack is empty, program is done-- return last frame's output
    Ok(self.pop_val().unwrap_or_default())
  }

  #[inline(always)]
  fn tick(&mut self) -> RuntimeResult<bool> {
    runtime_trace!("tick start (stack size = {}; current frame = {})", self.call_stack.len(), self.call_stack.top().map_or("none".to_owned(), |top| top.to_string()));
    // Read frame's current intents and handle them before running the sequence
    while let Some(intent) = self.cur_frame_mut().pop_intent() {
      runtime_trace!("intent: {}", intent.name());
      match intent {
        Intent::PrintLast => {
          let val = self.pop_val()?;
          self.cur_frame_mut().write_value(val);
        },
        Intent::ReturnLast => {
          let val = self.pop_val()?;
          self.func_return(Some(val))?;
          return Ok(true)
        },
        Intent::ContinueLast => {
          let val = self.pop_val()?;
          self.interrupt_repeater(Some(val), true)?;
          return Ok(true)
        },
        Intent::BreakLast => {
          let val = self.pop_val()?;
          self.interrupt_repeater(Some(val), false)?;
          return Ok(true)
        },
        Intent::TickCurrentBlock => {            
          self.tick_current_block()?;
        },
        Intent::BuildWeightedBlock { block, mut weights, mut pop_next_weight } => {
          while weights.len() < block.elements.len() {
            if pop_next_weight {
              let weight_value = self.pop_val()?;
              weights.push(match weight_value {
                RantValue::Int(n) => n as f64,
                RantValue::Float(n) => n,
                RantValue::Boolean(b) => bf64(b),
                RantValue::Empty => 1.0,
                other => runtime_error!(RuntimeErrorType::ArgumentError, format!("weight values cannot be of type '{}'", other.type_name())),
              });
              pop_next_weight = false;
              continue
            }

            match &block.elements[weights.len()].weight {
              Some(weight) => match weight {
                BlockWeight::Dynamic(weight_expr) => {
                  let weight_expr = Rc::clone(weight_expr);
                  self.cur_frame_mut().push_intent(Intent::BuildWeightedBlock {
                    block,
                    weights,
                    pop_next_weight: true,
                  });
                  self.push_frame(weight_expr, true)?;
                  return Ok(true)
                },
                BlockWeight::Constant(weight_value) => {
                  weights.push(*weight_value);
                },
              },
              None => {
                weights.push(1.0);
              },
            }
          }

          // Weights are finished
          self.push_block(block.as_ref(), Some(weights))?;
        },
        Intent::SetVar { vname, access_kind } => {
          let val = self.pop_val()?;
          self.set_var_value(vname.as_str(), access_kind, val)?;
        },
        Intent::DefVar { vname, access_kind, is_const } => {
          let val = self.pop_val()?;
          self.def_var_value(vname.as_str(), access_kind, val, is_const)?;
        },
        Intent::BuildDynamicGetter { 
          path, dynamic_key_count, mut pending_exprs, 
          override_print, prefer_function, fallback } => {
          if let Some(key_expr) = pending_exprs.pop() {
            // Set next intent based on remaining expressions in getter
            if pending_exprs.is_empty() {
              self.cur_frame_mut().push_intent(Intent::GetValue { path, dynamic_key_count, override_print, prefer_function, fallback });
            } else {
              self.cur_frame_mut().push_intent(Intent::BuildDynamicGetter { path, dynamic_key_count, pending_exprs, override_print, prefer_function, fallback });
            }
            self.push_frame_flavored(Rc::clone(&key_expr), StackFrameFlavor::DynamicKeyExpression)?;
          } else {
            self.cur_frame_mut().push_intent(Intent::GetValue { path, dynamic_key_count, override_print, prefer_function, fallback });
          }
          return Ok(true)
        },
        Intent::GetValue { path, dynamic_key_count, override_print, prefer_function, fallback } => {
          let getter_result = self.get_value(path, dynamic_key_count, override_print, prefer_function);
          match (getter_result, fallback) {
            // If it worked, do nothing
            (Ok(()), _) => {},
            // If no fallback, raise error
            (Err(err), None) => return Err(err),
            // Run fallback if available
            (Err(_), Some(fallback)) => {
              if !override_print {
                self.cur_frame_mut().push_intent(Intent::PrintLast);
              }
              self.push_frame(fallback, true)?;
              return Ok(true)
            }
          }
        },
        Intent::CreateDefaultArgs { 
          context, 
          default_arg_exprs, 
          mut eval_index 
        } => {
          // Check if there's a default arg ready to pop
          if eval_index > 0 {
            // Pop it off the value stack
            let default_arg = self.pop_val()?;
            // Store it as a local
            let (_, var_name_index) = default_arg_exprs[eval_index - 1];
            let var_name = &context.params[var_name_index].name;
            self.def_var_value(var_name.as_str(), AccessPathKind::Local, default_arg, true)?;
          }

          // Check if there's another arg expression to evaluate
          if let Some(cur_expr) = default_arg_exprs.get(eval_index).map(|(e, _)| Rc::clone(e)) {            
            // Continuation intent (if needed)  
            eval_index += 1;
            if eval_index <= default_arg_exprs.len() {
              self.cur_frame_mut().push_intent(Intent::CreateDefaultArgs {
                context,
                default_arg_exprs,
                eval_index,
              });
            }

            // Evaluate
            self.push_frame_flavored(cur_expr, StackFrameFlavor::ArgumentExpression)?;

            // Interrupt
            return Ok(true)
          }

          // When finished, just fall through so the underlying function runs right away
        },
        Intent::Invoke { 
          arg_exprs, 
          arg_eval_count, 
          is_temporal, 
        } => {
          // First, evaluate all arguments
          if arg_eval_count < arg_exprs.len() {
            let arg_expr = arg_exprs.get(arg_exprs.len() - arg_eval_count - 1).unwrap();
            let arg_seq = Rc::clone(&arg_expr.expr);

            // Continuation intent
            self.cur_frame_mut().push_intent(Intent::Invoke { 
              arg_exprs, 
              arg_eval_count: arg_eval_count + 1,
              is_temporal, 
            });

            // Evaluate arg
            self.push_frame_flavored(arg_seq, StackFrameFlavor::ArgumentExpression)?;
            return Ok(true)
          } else {
            // Pop the evaluated args off the stack
            let mut args = vec![];
            for arg_expr in arg_exprs.iter() {
              let arg = self.pop_val()?;
              // When parametric spread is used and the argument is a list, expand its values into individual args
              if matches!(arg_expr.spread_mode, ArgumentSpreadMode::Parametric) {
                if arg.is_indexable() {
                  match &arg {
                    RantValue::List(list_ref) => {
                      for spread_arg in list_ref.borrow().iter() {
                        args.push(spread_arg.clone());
                      }
                    },
                    RantValue::Tuple(tuple_ref) => {
                      for spread_arg in tuple_ref.iter() {
                        args.push(spread_arg.clone());
                      }
                    },
                    other => {
                      for i in 0..(other.len()) {
                        args.push(other.index_get(i as i64).into_runtime_result()?);
                      }
                    }
                  }
                  continue
                }
              }
              args.push(arg);
            }

            // Pop the function and make sure it's callable
            let func = match self.pop_val()? {
              RantValue::Function(func) => {
                func
              },
              other => runtime_error!(RuntimeErrorType::CannotInvokeValue, format!("cannot call '{}' value", other.type_name()))
            };

            // Call the function
            if is_temporal {
              let temporal_state = TemporalSpreadState::new(arg_exprs.as_slice(), args.as_slice());
              
              // If the temporal state has zero iterations, don't call the function at all
              if !temporal_state.is_empty() {
                self.cur_frame_mut().push_intent(Intent::CallTemporal { 
                  func,
                  temporal_state, 
                  args: Rc::new(args),
                });
              }
            } else {
              self.call_func(func, args, false)?;
            }
            
            return Ok(true)
          }
        },
        Intent::InvokePipeStep { 
          steps, 
          step_index, 
          state,
          pipeval,
        } => {          
          match state {
            InvokePipeStepState::EvaluatingFunc => {
              let step = &steps[step_index];
              let pipeval_copy = pipeval.clone();

              self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                steps: Rc::clone(&steps),
                step_index,
                state: InvokePipeStepState::EvaluatingArgs { num_evaluated: 0 },
                pipeval,
              });

              match &step.target {
                FunctionCallTarget::Path(path) => {
                  // TODO: expose pipeval to path-based function access in piped calls
                  self.push_getter_intents(path, true, true, None);
                },
                FunctionCallTarget::Expression(expr) => {
                  self.push_frame(Rc::clone(expr), true)?;
                  if let Some(pipeval) = pipeval_copy {
                    self.def_pipeval(pipeval)?;
                  }
                },
              }
              return Ok(true)
            },
            InvokePipeStepState::EvaluatingArgs { num_evaluated } => {
              let step = &steps[step_index];
              let arg_exprs = &step.arguments;
              let argc = arg_exprs.len();
              if num_evaluated < argc {                
                // Evaluate next argument
                let arg_expr = arg_exprs.get(argc - num_evaluated - 1).unwrap();
                let arg_seq = Rc::clone(&arg_expr.expr);
                let pipeval_copy = pipeval.clone();

                // Prepare next arg eval intent
                self.cur_frame_mut().push_intent(Intent::InvokePipeStep { 
                  steps: Rc::clone(&steps),
                  step_index,
                  state: InvokePipeStepState::EvaluatingArgs {
                    num_evaluated: num_evaluated + 1,
                  },
                  pipeval,
                });

                // Push current argument expression to call stack
                self.push_frame_flavored(arg_seq, StackFrameFlavor::ArgumentExpression)?;
                if let Some(pipeval) = pipeval_copy {
                  self.def_pipeval(pipeval)?;
                }
              } else {
                // If all args are evaluated, pop them off the stack
                let mut args = vec![];
                for arg_expr in arg_exprs.iter() {
                  let arg = self.pop_val()?;
                  // When parametric spread is used and the argument is a list, expand its values into individual args
                  if matches!(arg_expr.spread_mode, ArgumentSpreadMode::Parametric) {
                    if arg.is_indexable() {
                      match &arg {
                        RantValue::List(list_ref) => {
                          for spread_arg in list_ref.borrow().iter() {
                            args.push(spread_arg.clone());
                          }
                        },
                        RantValue::Tuple(tuple_ref) => {
                          for spread_arg in tuple_ref.iter() {
                            args.push(spread_arg.clone());
                          }
                        },
                        other => {
                          for i in 0..(other.len()) {
                            args.push(other.index_get(i as i64).into_runtime_result()?);
                          }
                        }
                      }
                      continue
                    }
                  }
                  args.push(arg);
                }

                // Pop the function and make sure it's callable
                let step_function = match self.pop_val()? {
                  RantValue::Function(func) => {
                    func
                  },
                  // What are you doing, step function?
                  other => runtime_error!(RuntimeErrorType::CannotInvokeValue, format!("cannot call '{}' value", other.type_name()))
                };
                
                // Transition to pre-call for next step
                self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                  state: if step.is_temporal {  
                    InvokePipeStepState::PreTemporalCall {
                      step_function,
                      temporal_state: TemporalSpreadState::new(arg_exprs.as_slice(), args.as_slice()),
                      args,
                    }
                  } else {
                    InvokePipeStepState::PreCall { step_function, args }
                  },
                  steps,
                  step_index,
                  pipeval,
                });
              }
              return Ok(true)
            },
            InvokePipeStepState::PreCall { step_function, args } => {
              // Transition intent to PostCall after function returns
              self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                steps,
                step_index,
                state: InvokePipeStepState::PostCall,
                pipeval,
              });

              // Call it and interrupt
              self.call_func(step_function, args, true)?;
              return Ok(true)
            },
            InvokePipeStepState::PostCall => {
              let next_pipeval = self.pop_val()?;
              let next_step_index = step_index + 1;
              // Check if there is a next step
              if next_step_index < steps.len() {
                // Create intent for next step
                self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                  steps,
                  step_index: next_step_index,
                  state: InvokePipeStepState::EvaluatingFunc,
                  pipeval: Some(next_pipeval),
                });
                return Ok(true)
              } else {
                // If there are no more steps in the chain, just print the pipeval and let this intent die
                self.cur_frame_mut().write_value(next_pipeval);
              }
            },
            InvokePipeStepState::PreTemporalCall { step_function, args, temporal_state } => {
              let targs = args.iter().enumerate().map(|(arg_index, arg)| {
                // Check if this is a temporally spread argument
                if let Some(tindex) = temporal_state.get(arg_index) {
                  if arg.is_indexable() {
                    if let Ok(tval) = arg.index_get(tindex as i64) {
                      return tval
                    }
                  }
                }
                arg.clone()
              }).collect::<Vec<RantValue>>();

              // Push continuation intent for temporal call
              self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                steps,
                step_index,
                state: InvokePipeStepState::PostTemporalCall {
                  step_function: Rc::clone(&step_function),
                  temporal_state,
                  args,
                },
                pipeval,
              });

              self.call_func(step_function, targs, true)?;
              return Ok(true)
            },
            InvokePipeStepState::PostTemporalCall { step_function, args, mut temporal_state } => {
              let next_piprval = self.pop_val()?;
              let next_step_index = step_index + 1;
              let step_count = steps.len();

              // Queue next iteration if available
              if temporal_state.increment() {
                self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                  steps: Rc::clone(&steps),
                  step_index,
                  state: InvokePipeStepState::PreTemporalCall {
                    step_function,
                    temporal_state,
                    args,
                  },
                  pipeval,
                })
              }

              // Call next function in chain
              if next_step_index < step_count {
                // Create intent for next step
                self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
                  steps,
                  step_index: next_step_index,
                  state: InvokePipeStepState::EvaluatingFunc,
                  pipeval: Some(next_piprval),
                });
                return Ok(true)
              } else {
                // If there are no more steps in the chain, just print the pipeval and let this intent die
                self.cur_frame_mut().write_value(next_piprval);
              }
            },
          }
        },
        Intent::CallTemporal { func, args, mut temporal_state } => {
          let targs = args.iter().enumerate().map(|(arg_index, arg)| {
            // Check if this is a temporally spread argument
            if let Some(tindex) = temporal_state.get(arg_index) {
              if arg.is_indexable() {
                if let Ok(tval) = arg.index_get(tindex as i64) {
                  return tval
                }
              }
            }
            arg.clone()
          }).collect::<Vec<RantValue>>();

          if temporal_state.increment() {
            self.cur_frame_mut().push_intent(Intent::CallTemporal { func: Rc::clone(&func), args, temporal_state });
          }

          self.call_func(func, targs, false)?;
          return Ok(true)
        },
        Intent::Call { argc, override_print } => {
          // Pop the evaluated args off the stack
          let mut args = vec![];
          for _ in 0..argc {
            args.push(self.pop_val()?);
          }

          // Pop the function and make sure it's callable
          let func = match self.pop_val()? {
            RantValue::Function(func) => {
              func
            },
            other => runtime_error!(RuntimeErrorType::CannotInvokeValue, format!("cannot call '{}' value", other.type_name()))
          };

          // Call the function
          self.call_func(func, args, override_print)?;
          return Ok(true)
        },
        Intent::BuildDynamicSetter { path, write_mode, expr_count, mut pending_exprs, val_source } => {
          // Prepare setter value
          match val_source {
            // Value must be evaluated from an expression
            SetterValueSource::FromExpression(expr) => {
              self.cur_frame_mut().push_intent(Intent::BuildDynamicSetter { path, write_mode, expr_count, pending_exprs, val_source: SetterValueSource::Consumed });
              self.push_frame(Rc::clone(&expr), true)?;
              return Ok(true)
            },
            // Value can be pushed directly onto the stack
            SetterValueSource::FromValue(val) => {
              self.push_val(val)?;
            },
            // Do nothing, it's already taken care of
            SetterValueSource::Consumed => {}
          }
          
          if let Some(key_expr) = pending_exprs.pop() {
            // Set next intent based on remaining expressions in setter
            if pending_exprs.is_empty() {
              // Set value once this frame is active again
              self.cur_frame_mut().push_intent(Intent::SetValue { path, write_mode, expr_count });
            } else {
              // Continue building setter
              self.cur_frame_mut().push_intent(Intent::BuildDynamicSetter { path, write_mode, expr_count, pending_exprs, val_source: SetterValueSource::Consumed });                
            }
            self.push_frame_flavored(Rc::clone(&key_expr), StackFrameFlavor::DynamicKeyExpression)?;
          } else {
            self.cur_frame_mut().push_intent(Intent::SetValue { path, write_mode, expr_count });
          }
          
          return Ok(true)
        },
        Intent::SetValue { path, write_mode, expr_count } => {
          self.set_value(path, write_mode, expr_count)?;
        },
        Intent::BuildList { init, index, mut list } => {
          // Add latest evaluated value to list
          if index > 0 {
            list.push(self.pop_val()?);
          }

          // Check if the list is complete
          if index >= init.len() {
            self.cur_frame_mut().write_value(RantValue::List(RantList::from(list).into_handle()))
          } else {
            // Continue list creation
            let val_expr = Rc::clone(&init[index]);
            self.cur_frame_mut().push_intent(Intent::BuildList { init, index: index + 1, list });
            self.push_frame(val_expr, true)?;
            return Ok(true)
          }
        },
        Intent::BuildTuple { init, index, mut items } => {
          // Add latest evaluated value to tuple
          if index > 0 {
            items.push(self.pop_val()?);
          }

          // Check if the tuple is complete
          if index >= init.len() {
            self.cur_frame_mut().write_value(RantValue::Tuple(RantTuple::from(items).into_handle()))
          } else {
            // Continue tuple creation
            let val_expr = Rc::clone(&init[index]);
            self.cur_frame_mut().push_intent(Intent::BuildTuple { init, index: index + 1, items });
            self.push_frame(val_expr, true)?;
            return Ok(true)
          }
        },
        Intent::BuildMap { init, pair_index, mut map } => {
          // Add latest evaluated pair to map
          if pair_index > 0 {
            let prev_pair = &init[pair_index - 1];
            // If the key is dynamic, there are two values to pop
            let key = match prev_pair {
              (MapKeyExpr::Dynamic(_), _) => InternalString::from(self.pop_val()?.to_string()),
              (MapKeyExpr::Static(key), _) => key.clone()
            };
            let val = self.pop_val()?;
            map.raw_set(key.as_str(), val);
          }

          // Check if the map is completed
          if pair_index >= init.len() {
            self.cur_frame_mut().write_value(RantValue::Map(map.into_handle()));
          } else {
            // Continue map creation
            self.cur_frame_mut().push_intent(Intent::BuildMap { init: Rc::clone(&init), pair_index: pair_index + 1, map });
            let (key_expr, val_expr) = &init[pair_index];
            if let MapKeyExpr::Dynamic(key_expr) = key_expr {
              // Push dynamic key expression onto call stack
              self.push_frame(Rc::clone(key_expr), true)?;
            }
            // Push value expression onto call stack
            self.push_frame(Rc::clone(val_expr), true)?;
            return Ok(true)
          }
        },
        Intent::ImportLastAsModule { module_name, descope } => {
          let module = self.pop_val()?;

          // Cache the module
          if let Some(RantValue::Map(module_cache_ref)) = self.engine.get_global(crate::MODULES_CACHE_KEY) {
            module_cache_ref.borrow_mut().raw_set(&module_name, module.clone());
          } else {
            let mut cache = RantMap::new();
            cache.raw_set(&module_name, module.clone());
            self.engine.set_global(crate::MODULES_CACHE_KEY, RantValue::Map(RantMap::from(cache).into_handle()));
          }

          self.def_var_value(&module_name, AccessPathKind::Descope(descope), module, true)?;
        },
        Intent::RuntimeCall { function, interrupt } => {
          function(self)?;
          if interrupt {
            return Ok(true)
          }
        },
        Intent::DropStaleUnwinds => {
          while let Some(unwind) = self.unwinds.last() {
            if unwind.call_stack_size >= self.call_stack.len() {
              break
            }
            self.unwinds.pop();
          }
        },
        Intent::LogicShortCircuit { on_truthiness, short_circuit_result, gen_op_intent, rhs } => {
          let lhs = self.pop_val()?;
          let lhs_truth = lhs.to_bool();
          self.push_val(if lhs_truth == on_truthiness { 
            match short_circuit_result {
              LogicShortCircuitHandling::Passthrough => lhs,
              LogicShortCircuitHandling::OverrideWith(b) => RantValue::Boolean(b),
            }
          } else { 
            lhs 
          })?;

          if lhs_truth != on_truthiness {
            let op_intent = gen_op_intent();
            self.cur_frame_mut().push_intent(op_intent);
            self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: rhs });
            return Ok(true);
          }
        },
        Intent::CallOperand { sequence } => {
          self.push_frame(sequence, false)?;
          return Ok(true)
        },
        Intent::Add => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs + rhs)?;
        },
        Intent::Subtract => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs - rhs)?;
        },
        Intent::Multiply => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs * rhs)?;
        },
        Intent::Divide => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val((lhs / rhs).into_runtime_result()?)?;
        },
        Intent::Modulo => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val((lhs % rhs).into_runtime_result()?)?;
        },
        Intent::Power => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs.pow(rhs).into_runtime_result()?)?;
        },
        Intent::Negate => {
          let operand = self.pop_val()?;
          self.push_val(-operand)?;
        },
        Intent::LogicNot => {
          let operand = self.pop_val()?;
          self.push_val(!operand)?;
        }
        Intent::LogicAnd => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs.and(rhs))?;
        },
        Intent::LogicOr => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs.or(rhs))?;
        },
        Intent::LogicNand => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(!lhs.and(rhs))?;
        },
        Intent::LogicNor => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(!lhs.or(rhs))?;
        },
        Intent::LogicXor => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(lhs.xor(rhs))?;
        },
        Intent::Equals => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs == rhs))?;
        },
        Intent::NotEquals => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs != rhs))?;
        },
        Intent::Less => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs < rhs))?;
        },
        Intent::LessOrEqual => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs <= rhs))?;
        },
        Intent::Greater => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs > rhs))?;
        },
        Intent::GreaterOrEqual => {
          let rhs = self.pop_val()?;
          let lhs = self.pop_val()?;
          self.push_val(RantValue::Boolean(lhs >= rhs))?;
        },
        Intent::CheckCondition { conditions, fallback, index } => {
          if index == 0 {
            // No conditions have run yet
            let condition = Rc::clone(&conditions[index].0);
            self.cur_frame_mut().push_intent(Intent::CheckCondition { conditions, fallback, index: index + 1 });
            self.push_frame(condition, false)?;
            return Ok(true)
          } else {
            let prev_cond_result = self.pop_val()?.to_bool();
            if prev_cond_result {
              // Condition was met; run the body
              self.pre_push_block(&conditions[index - 1].1)?;
              return Ok(true)
            }

            // Previous condition failed
            if index < conditions.len() {
              // Run next condition
              let condition = Rc::clone(&conditions[index].0);
              self.cur_frame_mut().push_intent(Intent::CheckCondition { conditions, fallback, index: index + 1 });
              self.push_frame(condition, false)?;
              return Ok(true)
            } else {
              // All conditions have been checked
              if let Some(fallback) = fallback {
                self.pre_push_block(&fallback)?;
                return Ok(true)
              }
            }
          }
        }
      }
    }

    runtime_trace!("intents finished");
    
    // Run frame's sequence elements in order
    while let Some(rst) = &self.cur_frame_mut().seq_next() {
      match Rc::deref(rst) {
        Rst::Sequence(seq) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.push_frame(Rc::clone(seq), false)?;
          return Ok(true)
        },
        Rst::ListInit(elements) => {
          self.cur_frame_mut().push_intent(Intent::BuildList { init: Rc::clone(elements), index: 0, list: RantList::with_capacity(elements.len()) });
          return Ok(true)
        },
        Rst::TupleInit(elements) => {
          self.cur_frame_mut().push_intent(Intent::BuildTuple { init: Rc::clone(elements), index: 0, items: vec![] });
          return Ok(true)
        },
        Rst::MapInit(elements) => {
          self.cur_frame_mut().push_intent(Intent::BuildMap { init: Rc::clone(elements), pair_index: 0, map: RantMap::new() });
          return Ok(true)
        },
        Rst::Block(block) => {
          self.pre_push_block(block)?;
          return Ok(true)
        },
        Rst::DefVar(vname, access_kind, val_expr) => {
          if let Some(val_expr) = val_expr {
            // If a value is present, it needs to be evaluated first
            self.cur_frame_mut().push_intent(Intent::DefVar { vname: vname.clone(), access_kind: *access_kind, is_const: false });
            self.push_frame(Rc::clone(val_expr), true)?;
            return Ok(true)
          } else {
            // If there's no assignment, just set it to empty value
            self.def_var_value(vname.as_str(), *access_kind, RantValue::Empty, false)?;
          }
        },
        Rst::DefConst(vname, access_kind, val_expr) => {
          if let Some(val_expr) = val_expr {
            // If a value is present, it needs to be evaluated first
            self.cur_frame_mut().push_intent(Intent::DefVar { vname: vname.clone(), access_kind: *access_kind, is_const: true });
            self.push_frame(Rc::clone(val_expr), true)?;
            return Ok(true)
          } else {
            // If there's no assignment, just set it to empty value
            self.def_var_value(vname.as_str(), *access_kind, RantValue::Empty, true)?;
          }
        },
        Rst::Get(path, fallback) => {
          self.push_getter_intents(path, false, false, fallback.as_ref().map(Rc::clone));
          return Ok(true)
        },
        Rst::Depth(vname, access_kind, fallback) => {
          match (self.get_var_depth(vname, *access_kind), fallback) {
            (Ok(depth), _) => self.cur_frame_mut().write_value(RantValue::Int(depth as i64)),
            (Err(_), Some(fallback)) => {
              self.cur_frame_mut().push_intent(Intent::PrintLast);
              self.push_frame(Rc::clone(fallback), true)?;
              return Ok(true)
            },
            (Err(err), None) => return Err(err),
          }
        },
        Rst::Set(path, val_expr) => {
          // Get list of dynamic expressions in path
          let exprs = path.dynamic_exprs();

          if exprs.is_empty() {
            // Setter is static, so run it directly
            self.cur_frame_mut().push_intent(Intent::SetValue { path: Rc::clone(path), write_mode: VarWriteMode::SetOnly, expr_count: 0 });
            self.push_frame(Rc::clone(val_expr), true)?;
          } else {
            // Build dynamic keys before running setter
            self.cur_frame_mut().push_intent(Intent::BuildDynamicSetter {
              expr_count: exprs.len(),
              write_mode: VarWriteMode::SetOnly,
              path: Rc::clone(path),
              pending_exprs: exprs,
              val_source: SetterValueSource::FromExpression(Rc::clone(val_expr))
            });
          }
          return Ok(true)
        },
        Rst::FuncDef(FunctionDef {
          body,
          capture_vars: to_capture,
          is_const,
          params,
          path,
        }) => {
          // Capture variables
          let mut captured_vars = vec![];
          for capture_id in to_capture.iter() {
            let var = self.call_stack.get_var_mut(self.engine, capture_id, AccessPathKind::Local)?;
            var.make_by_ref();
            captured_vars.push((capture_id.clone(), var.clone()));
          }

          // Build function
          let func = RantValue::Function(Rc::new(RantFunction {
            body: RantFunctionInterface::User(Rc::clone(body)),
            captured_vars,
            min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
            vararg_start_index: params.iter()
            .enumerate()
            .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
            .unwrap_or_else(|| params.len()),
            params: Rc::clone(params),
            flavor: None,
          }));

          // Evaluate setter path
          let dynamic_keys = path.dynamic_exprs();
          self.cur_frame_mut().push_intent(Intent::BuildDynamicSetter {
            expr_count: dynamic_keys.len(),
            write_mode: if *is_const { VarWriteMode::DefineConst } else { VarWriteMode::Define },
            pending_exprs: dynamic_keys,
            path: Rc::clone(path),
            val_source: SetterValueSource::FromValue(func)
          });

          return Ok(true)
        },
        Rst::Lambda(LambdaExpr { 
          params, 
          body, 
          capture_vars: to_capture 
        }) => {
          // Capture variables
          let mut captured_vars = vec![];
          for capture_id in to_capture.iter() {
            let var = self.call_stack.get_var_mut(self.engine, capture_id, AccessPathKind::Local)?;
            var.make_by_ref();
            captured_vars.push((capture_id.clone(), var.clone()));
          }

          let func = RantValue::Function(Rc::new(RantFunction {
            body: RantFunctionInterface::User(Rc::clone(body)),
            captured_vars,
            min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
            vararg_start_index: params.iter()
            .enumerate()
            .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
            .unwrap_or_else(|| params.len()),
            params: Rc::clone(params),
            flavor: None,
          }));

          self.cur_frame_mut().write_value(func);
        },
        Rst::FuncCall(fcall) => {
          let FunctionCall {
            target,
            arguments,
            is_temporal,
          } = fcall;

          match target {
            // Named function call
            FunctionCallTarget::Path(path) => {
              // Queue up the function call behind the dynamic keys
              self.cur_frame_mut().push_intent(Intent::Invoke {
                arg_eval_count: 0,
                arg_exprs: Rc::clone(arguments),
                is_temporal: *is_temporal,
              });

              self.push_getter_intents(path, true, true, None);
            },
            // Anonymous function call
            FunctionCallTarget::Expression(expr) => {
              // Evaluate arguments after function is evaluated
              self.cur_frame_mut().push_intent(Intent::Invoke {
                arg_exprs: Rc::clone(arguments),
                arg_eval_count: 0,
                is_temporal: *is_temporal,
              });

              // Push function expression onto stack
              self.push_frame(Rc::clone(expr), true)?;
            },
          }
          return Ok(true)
        },
        Rst::PipedCall(compcall) => {     
          self.cur_frame_mut().push_intent(Intent::InvokePipeStep {
            steps: Rc::clone(&compcall.steps),
            step_index: 0,
            state: InvokePipeStepState::EvaluatingFunc,
            pipeval: None,
          });
          return Ok(true)
        },
        Rst::PipeValue => {
          let pipeval = self.get_var_value(PIPE_VALUE_NAME, AccessPathKind::Local, false)?;
          self.cur_frame_mut().write_value(pipeval);
        },
        Rst::DebugCursor(info) => {
          self.cur_frame_mut().set_debug_info(info);
        },
        Rst::Fragment(frag) => self.cur_frame_mut().write_frag(frag),
        Rst::Whitespace(ws) => self.cur_frame_mut().write_ws(ws),
        Rst::Integer(n) => self.cur_frame_mut().write_value(RantValue::Int(*n)),
        Rst::Float(n) => self.cur_frame_mut().write_value(RantValue::Float(*n)),
        Rst::EmptyValue => self.cur_frame_mut().write_value(RantValue::Empty),
        Rst::Boolean(b) => self.cur_frame_mut().write_value(RantValue::Boolean(*b)),
        Rst::Nop => {},
        Rst::Return(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent(Intent::ReturnLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.func_return(None)?;
            return Ok(true)
          }
        },
        Rst::Continue(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent(Intent::ContinueLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.interrupt_repeater(None, true)?;
            return Ok(true)
          }
        },
        Rst::Break(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent(Intent::BreakLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.interrupt_repeater(None, false)?;
            return Ok(true)
          }
        },
        Rst::Add(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Add);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Subtract(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Subtract);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Multiply(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Multiply);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Divide(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Divide);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Modulo(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Modulo);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Power(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Power);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Negate(operand) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Negate);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(operand) });
          return Ok(true)
        },
        Rst::LogicNot(operand) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicNot);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(operand) });
          return Ok(true)
        },
        Rst::Equals(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Equals);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::NotEquals(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::NotEquals);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Less(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Less);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LessOrEqual(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LessOrEqual);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Greater(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::Greater);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::GreaterOrEqual(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::GreaterOrEqual);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LogicAnd(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicShortCircuit {
            on_truthiness: false,
            rhs: Rc::clone(rhs),
            short_circuit_result: LogicShortCircuitHandling::Passthrough,
            gen_op_intent: Box::new(|| Intent::LogicAnd),
          });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LogicOr(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicShortCircuit {
            on_truthiness: true,
            rhs: Rc::clone(rhs),
            short_circuit_result: LogicShortCircuitHandling::Passthrough,
            gen_op_intent: Box::new(|| Intent::LogicOr),
          });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LogicNand(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicShortCircuit {
            on_truthiness: false,
            rhs: Rc::clone(rhs),
            short_circuit_result: LogicShortCircuitHandling::OverrideWith(true),
            gen_op_intent: Box::new(|| Intent::LogicNand),
          });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LogicNor(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicShortCircuit {
            on_truthiness: true,
            rhs: Rc::clone(rhs),
            short_circuit_result: LogicShortCircuitHandling::OverrideWith(false),
            gen_op_intent: Box::new(|| Intent::LogicNor),
          });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::LogicXor(lhs, rhs) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          self.cur_frame_mut().push_intent(Intent::LogicXor);
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(rhs) });
          self.cur_frame_mut().push_intent(Intent::CallOperand { sequence: Rc::clone(lhs) });
          return Ok(true)
        },
        Rst::Require { alias, path } => {
          // TODO: Move this into a separate function
          // Get name of module from path
          if let Some(module_name) = 
          PathBuf::from(path.as_str())
          .with_extension("")
          .file_name()
          .map(|name| name.to_str())
          .flatten()
          .map(|name| name.to_owned())
          {
            // Check if module is cached; if so, don't do anything
            if let Some(RantValue::Map(module_cache_ref)) = self.context().get_global(crate::MODULES_CACHE_KEY) {
              if let Some(module @ RantValue::Map(..)) = module_cache_ref.borrow().raw_get(&module_name) {
                self.def_var_value(alias.as_ref().map(|a| a.as_str()).unwrap_or_else(|| module_name.as_str()), AccessPathKind::Local, module.clone(), true)?;
                continue
              }
            }

            // If not cached, attempt to load it from file and run its root sequence
            let caller_origin = Rc::clone(self.cur_frame().origin());
            let module_pgm = self.context_mut().try_read_module(path.as_str(), caller_origin).into_runtime_result()?;
            self.cur_frame_mut().push_intent(Intent::ImportLastAsModule { module_name: alias.as_ref().map(|a| a.to_string()).unwrap_or(module_name), descope: 0 });
            self.push_frame_flavored(Rc::clone(&module_pgm.root), StackFrameFlavor::FunctionBody)?;
            continue
          } else {
            runtime_error!(RuntimeErrorType::ArgumentError, "module name is missing from path");
          }
        },
        Rst::Conditional { conditions, fallback } => {
          self.cur_frame_mut().push_intent(Intent::CheckCondition{ conditions: Rc::clone(conditions), fallback: fallback.as_ref().map(Rc::clone), index: 0 });
          return Ok(true)
        },
        #[allow(unreachable_patterns)]
        rst => {
          runtime_error!(RuntimeErrorType::InternalError, format!("unsupported node type: '{}'", rst.display_name()));
        },
      }
    }

    runtime_trace!("frame done: {}", self.call_stack.len());
    
    // Pop frame once its sequence is finished
    let last_frame = self.pop_frame()?;
    self.push_val(last_frame.into_output())?;
    
    Ok(false)
  }

  #[inline(always)]
  pub fn push_getter_intents(&mut self, path: &Rc<AccessPath>, override_print: bool, prefer_function: bool, fallback: Option<Rc<Sequence>>) {
    let dynamic_keys = path.dynamic_exprs();

    // Run the getter to retrieve the function we're calling first...
    self.cur_frame_mut().push_intent(if dynamic_keys.is_empty() {
      // Getter is static, so run it directly
      Intent::GetValue { 
        path: Rc::clone(path), 
        dynamic_key_count: 0, 
        override_print,
        prefer_function,
        fallback,
      }
    } else {
      // Build dynamic keys before running getter
      Intent::BuildDynamicGetter {
        dynamic_key_count: dynamic_keys.len(),
        path: Rc::clone(path),
        pending_exprs: dynamic_keys,
        override_print,
        prefer_function,
        fallback,
      }
    });
  }

  /// Prepares a call to a function with the specified arguments.
  #[inline]
  pub fn call_func(&mut self, func: RantFunctionHandle, mut args: Vec<RantValue>, override_print: bool) -> RuntimeResult<()> {
    let argc = args.len();

    if !override_print {
      self.cur_frame_mut().push_intent(Intent::PrintLast);
    }

    // Verify the args fit the signature
    if func.is_variadic() {
      if argc < func.min_arg_count {
        runtime_error!(RuntimeErrorType::ArgumentMismatch, format!("arguments don't match; expected at least {}, found {}", func.min_arg_count, argc))
      }
    } else if argc < func.min_arg_count || argc > func.params.len() {
      runtime_error!(RuntimeErrorType::ArgumentMismatch, format!("arguments don't match; expected {}, found {}", func.min_arg_count, argc))
    }

    // Call the function
    match &func.body {
      RantFunctionInterface::Foreign(foreign_func) => {
        let foreign_func = Rc::clone(foreign_func);
        self.push_native_call_frame(Box::new(move |vm| foreign_func(vm, args)), StackFrameFlavor::NativeCall)?;
      },
      RantFunctionInterface::User(user_func) => {
        // Split args at vararg
        let mut args_iter = args.drain(..);
        let mut args_nonvariadic = vec![];
        
        for _ in 0..func.vararg_start_index {
          if let Some(arg) = args_iter.next() {
            args_nonvariadic.push(arg);
            continue
          }
          break
        }

        // This won't be added to args because we need to check default arguments
        let mut vararg = func.is_variadic().then(|| RantValue::List(RantList::from(args_iter.collect::<RantList>()).into_handle()));

        // Push the function onto the call stack
        self.push_frame_flavored(Rc::clone(user_func), func.flavor.unwrap_or(StackFrameFlavor::FunctionBody))?;

        // Pass captured vars to the function scope
        for (capture_name, capture_var) in func.captured_vars.iter() {
          self.call_stack.def_local_var(
            capture_name.as_str(),
            RantVar::clone(capture_var)
          )?;
        }

        // Pass the args to the function scope
        let mut needs_default_args = false;
        let mut args_nonvariadic = args_nonvariadic.drain(..);
        let mut default_arg_exprs = vec![];
        for (i, p) in func.params.iter().enumerate() {
          let pname_str = p.name.as_str();
          
          let user_arg = if p.varity.is_variadic() {
            vararg.take()
          } else {
            let user_arg = args_nonvariadic.next();
            if p.is_optional() && user_arg.is_none() {
              if let Some(default_arg_expr) = &p.default_value_expr {
                default_arg_exprs.push((Rc::clone(default_arg_expr), i));
                needs_default_args = true;
              }
              continue
            }
            user_arg
          };
          
          self.call_stack.def_var_value(
            self.engine, 
            pname_str, 
            AccessPathKind::Local, 
            user_arg.unwrap_or_default(),
            true,
          )?;
        }

        // Evaluate default args if needed
        if needs_default_args {
          self.cur_frame_mut().push_intent(Intent::CreateDefaultArgs {
            context: Rc::clone(&func),
            default_arg_exprs,
            eval_index: 0,
          });
        }
      },
    }
    Ok(())
  }

  /// Runs a setter.
  #[inline]
  fn set_value(&mut self, path: Rc<AccessPath>, write_mode: VarWriteMode, dynamic_value_count: usize) -> RuntimeResult<()> {
    // Gather evaluated dynamic path components from stack
    let mut dynamic_values = vec![];
    for _ in 0..dynamic_value_count {
      dynamic_values.push(self.pop_val()?);
    }

    // Setter RHS should be last value to pop
    let setter_value = self.pop_val()?;
    
    let access_kind = path.kind();
    let mut path_iter = path.iter();
    let mut dynamic_values = dynamic_values.drain(..);
    
    // The setter target is the value that will be modified. If None, setter_key refers to a variable.
    let mut setter_target: Option<RantValue> = None;

    // The setter key is the location on the setter target that will be written to.
    let mut setter_key = match path_iter.next() {
      Some(AccessPathComponent::Name(vname)) => {
        Some(SetterKey::KeyRef(vname.as_str()))
      },
      Some(AccessPathComponent::DynamicKey(_)) => {
        let key = InternalString::from(dynamic_values.next().unwrap().to_string());
        Some(SetterKey::KeyString(key))
      },
      Some(AccessPathComponent::AnonymousValue(_)) => {
        setter_target = Some(dynamic_values.next().unwrap());
        None
      },
      other => runtime_error!(RuntimeErrorType::InternalError, format!("setter key unsupported: '{:?}'; the access path was probably miscompiled", other))
    };

    // Evaluate the path
    for accessor in path_iter {
      // Update setter target by keying off setter_key
      setter_target = match (&setter_target, &mut setter_key) {
        (None, Some(SetterKey::KeyRef(key))) => Some(self.get_var_value(key, access_kind, false)?),
        (None, Some(SetterKey::KeyString(key))) => Some(self.get_var_value(key.as_str(), access_kind, false)?),
        (Some(_), None) => setter_target,
        (Some(val), Some(SetterKey::Index(index))) => Some(val.index_get(*index).into_runtime_result()?),
        (Some(val), Some(SetterKey::KeyRef(key))) => Some(val.key_get(key).into_runtime_result()?),
        (Some(val), Some(SetterKey::KeyString(key))) => Some(val.key_get(key.as_str()).into_runtime_result()?),
        (Some(val), Some(SetterKey::Slice(slice))) => Some(val.slice_get(slice).into_runtime_result()?),
        _ => unreachable!()
      };

      setter_key = Some(match accessor {
        // Static key
        AccessPathComponent::Name(key) => SetterKey::KeyRef(key.as_str()),
        // Index
        AccessPathComponent::Index(index) => SetterKey::Index(*index),
        // Slice
        AccessPathComponent::Slice(slice) => {
          let slice = match slice.as_static_slice(|_di| dynamic_values.next().unwrap()) {
            Ok(slice) => slice,
            Err(bad_bound_type) => runtime_error!(RuntimeErrorType::SliceError(SliceError::UnsupportedSliceBoundType(bad_bound_type))),
          };
          SetterKey::Slice(slice)
        },
        // Dynamic key
        AccessPathComponent::DynamicKey(_) => {
          match dynamic_values.next().unwrap() {
            RantValue::Int(index) => {
              SetterKey::Index(index)
            },
            key_val => {
              SetterKey::KeyString(InternalString::from(key_val.to_string()))
            }
          }
        },
        // Pipeval
        AccessPathComponent::PipeValue => {
          let pipeval = self.get_var_value(PIPE_VALUE_NAME, AccessPathKind::Local, false)?;
          match pipeval {
            RantValue::Int(i) => SetterKey::Index(i),
            key_val => SetterKey::KeyString(InternalString::from(key_val.to_string()))
          }
        },
        // Anonymous value (not allowed)
        AccessPathComponent::AnonymousValue(_) => {
          runtime_error!(RuntimeErrorType::InvalidOperation, "anonymous values may only appear as the first component in an access path")
        },
      })
    }

    macro_rules! def_or_set {
      ($vname:expr, $access_kind:expr, $value:expr) => {
        match write_mode {
          VarWriteMode::SetOnly => self.set_var_value($vname, $access_kind, $value)?,
          VarWriteMode::Define => self.def_var_value($vname, $access_kind, $value, false)?,
          VarWriteMode::DefineConst => self.def_var_value($vname, $access_kind, $value, true)?,
        }
      }
    }

    // Finally, set the value
    match (&mut setter_target, &setter_key) {
      (None, Some(SetterKey::KeyRef(vname))) => {
        def_or_set!(vname, access_kind, setter_value);
      },
      (None, Some(SetterKey::KeyString(vname))) => {
        def_or_set!(vname.as_str(), access_kind, setter_value);
      },
      (Some(target), Some(SetterKey::Index(index))) => target.index_set(*index, setter_value).into_runtime_result()?,
      (Some(target), Some(SetterKey::KeyRef(key))) => target.key_set(key, setter_value).into_runtime_result()?,
      (Some(target), Some(SetterKey::KeyString(key))) => target.key_set(key.as_str(), setter_value).into_runtime_result()?,
      (Some(target), Some(SetterKey::Slice(slice))) => target.slice_set(slice, setter_value).into_runtime_result()?,
      _ => unreachable!()
    }

    Ok(())
  }

  /// Runs a getter.
  #[inline]
  fn get_value(&mut self, path: Rc<AccessPath>, dynamic_key_count: usize, override_print: bool, prefer_function: bool) -> RuntimeResult<()> {
    let prefer_function = prefer_function && path.len() == 1;

    // Gather evaluated dynamic keys from stack
    let mut dynamic_keys = vec![];
    for _ in 0..dynamic_key_count {
      dynamic_keys.push(self.pop_val()?);
    }

    let mut path_iter = path.iter();
    let mut dynamic_keys = dynamic_keys.drain(..);

    // Get the root variable or anon value
    let mut getter_value = match path_iter.next() {
        Some(AccessPathComponent::Name(vname)) => {
          self.get_var_value(vname.as_str(), path.kind(), prefer_function)?
        },
        Some(AccessPathComponent::DynamicKey(_)) => {
          let key = dynamic_keys.next().unwrap().to_string();
          self.get_var_value(key.as_str(), path.kind(), prefer_function)?
        },
        Some(AccessPathComponent::AnonymousValue(_)) => {
          dynamic_keys.next().unwrap()
        },
        Some(AccessPathComponent::PipeValue) => {
          self.get_var_value(PIPE_VALUE_NAME, AccessPathKind::Local, false)?
        }
        _ => unreachable!()
    };

    // Evaluate the rest of the path
    for accessor in path_iter {
      match accessor {
        // Static key
        AccessPathComponent::Name(key) => {
          getter_value = match getter_value.key_get(key.as_str()) {
            Ok(val) => val,
            Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
          };
        },
        // Index
        AccessPathComponent::Index(index) => {
          getter_value = match getter_value.index_get(*index) {
            Ok(val) => val,
            Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
          }
        },
        // Dynamic key
        AccessPathComponent::DynamicKey(_) => {
          let key = dynamic_keys.next().unwrap();
          match key {
            RantValue::Int(index) => {
              getter_value = match getter_value.index_get(index) {
                Ok(val) => val,
                Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
              }
            },
            _ => {
              getter_value = match getter_value.key_get(key.to_string().as_str()) {
                Ok(val) => val,
                Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
              };
            }
          }
        },
        AccessPathComponent::Slice(slice) => {
          let static_slice = match slice.as_static_slice(|_di| dynamic_keys.next().unwrap()) {
            Ok(slice) => slice,
            Err(bad_bound_type) => runtime_error!(RuntimeErrorType::SliceError(SliceError::UnsupportedSliceBoundType(bad_bound_type))),
          };
          getter_value = getter_value.slice_get(&static_slice).into_runtime_result()?;
        },
        // Pipeval
        AccessPathComponent::PipeValue => {
          let pipeval = self.get_var_value(PIPE_VALUE_NAME, AccessPathKind::Local, false)?;
          match pipeval {
            RantValue::Int(index) => {
              getter_value = match getter_value.index_get(index) {
                Ok(val) => val,
                Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
              }
            },
            key_val => {
              getter_value = match getter_value.key_get(key_val.to_string().as_str()) {
                Ok(val) => val,
                Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
              };
            }
          }
        },
        // Anonymous value (not allowed)
        AccessPathComponent::AnonymousValue(_) => {
          runtime_error!(RuntimeErrorType::InvalidOperation, "anonymous values may only appear as the first component in an access path")
        },
      }
    }

    if override_print {
      self.push_val(getter_value)?;
    } else {
      self.cur_frame_mut().write_value(getter_value);
    }

    Ok(())
  }

  /// Checks for an active block and attempts to iterate it. If a valid element is returned, it is pushed onto the call stack.
  pub fn tick_current_block(&mut self) -> RuntimeResult<()> {
    let mut is_repeater = false;

    let rng = self.rng_clone();
    
    // Get the active block from the resolver
    let next_element = if let Some(state) = self.resolver.active_block_mut() {
      is_repeater = state.is_repeater();
      
      // Request the next element to run for this block
      if let Some(element) = state.next_element(rng.as_ref()).into_runtime_result()? {
        Some(element)
      } else {
        // If the block is done, pop the state from the block stack
        self.resolver.pop_block();
        None
      }
      
    } else {
      None
    };

    // Push frame for next block element, if available
    if let Some(element) = next_element {  
      // Tell the calling frame to check the block status once the separator returns
      self.cur_frame_mut().push_intent(Intent::TickCurrentBlock);

      match element {
        BlockAction::Element(elem) => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);
          
          // Check for output modifier
          if let Some(modifier) = &elem.output_modifier {
            let modifier_input = self.cur_frame_mut().render_and_reset_output();

            // Push the next element
            self.push_frame_flavored(
              Rc::clone(&elem.main), 
              if is_repeater { 
                StackFrameFlavor::RepeaterElement 
              } else { 
                StackFrameFlavor::BlockElement
              }
            )?;

            // Define the input variable
            if let Some(input_id) = &modifier.input_var {
              self.def_var_value(input_id.as_str(), AccessPathKind::Local, modifier_input, true)?;
            }
          } else {
            // Push the next element
            self.push_frame_flavored(
              Rc::clone(&elem.main), 
              if is_repeater { 
                StackFrameFlavor::RepeaterElement 
              } else { 
                StackFrameFlavor::BlockElement
              }
            )?;
          }
        },
        BlockAction::MutateElement { elem, elem_func, mutator_func } => {
          self.cur_frame_mut().push_intent(Intent::PrintLast);

          if let Some(modifier) = &elem.output_modifier {
            let input_value = self.cur_frame_mut().render_and_reset_output();

            // Call the mutator function
            self.call_func(mutator_func, vec![RantValue::Function(elem_func)], true)?;

            // Define the input variable
            if let Some(input_id) = &modifier.input_var {
              self.def_var_value(input_id.as_str(), AccessPathKind::Local, input_value, true)?;
            }
          } else {
            // Call the mutator function
            self.call_func(mutator_func, vec![RantValue::Function(elem_func)], true)?;
          }
        },
        BlockAction::Separator(separator) => {
          match separator {
            // If the separator is a function, call the function
            RantValue::Function(sep_func) => {
              self.push_val(RantValue::Function(sep_func))?;
              self.cur_frame_mut().push_intent(Intent::Call { 
                argc: 0,
                override_print: false 
              });
            },
            // Print the separator if it's a non-function value
            val => {
              self.cur_frame_mut().write_value(val);
            }
          }
        }
      }      
    }
    
    Ok(())
  }

  /// Performs any necessary preparation (such as pushing weight intents) before pushing a block.
  /// If the block can be pushed immediately, it will be.
  #[inline]
  pub fn pre_push_block(&mut self, block: &Rc<Block>) -> RuntimeResult<()> {
    if block.is_weighted {
      self.cur_frame_mut().push_intent(Intent::BuildWeightedBlock {
        block: Rc::clone(block),
        weights: Weights::new(block.elements.len()),
        pop_next_weight: false,
      });
    } else {
      self.push_block(block, None)?;
    }
    Ok(())
  }

  /// Consumes attributes and pushes a block onto the resolver stack.
  #[inline]
  pub fn push_block(&mut self, block: &Block, weights: Option<Weights>) -> RuntimeResult<()> {
    // Push a new state onto the block stack
    self.resolver.push_block(block, weights)?;

    // Check the block to make sure it actually does something.
    // If the block has some skip condition, it will automatically remove it, and this method will have no net effect.
    self.tick_current_block()?;

    Ok(())
  }

  #[inline(always)]
  fn def_pipeval(&mut self, pipeval: RantValue) -> RuntimeResult<()> {
    self.call_stack.def_var_value(self.engine, PIPE_VALUE_NAME, AccessPathKind::Local, pipeval, true)
  }

  /// Sets the value of an existing variable.
  #[inline(always)]
  pub(crate) fn set_var_value(&mut self, varname: &str, access: AccessPathKind, val: RantValue) -> RuntimeResult<()> {
    self.call_stack.set_var_value(self.engine, varname, access, val)
  }

  /// Gets the value of an existing variable.
  #[inline(always)]
  pub fn get_var_value(&self, varname: &str, access: AccessPathKind, prefer_function: bool) -> RuntimeResult<RantValue> {
    self.call_stack.get_var_value(self.engine, varname, access, prefer_function)
  }

  #[inline(always)]
  pub fn get_var_depth(&self, varname: &str, access: AccessPathKind) -> RuntimeResult<usize> {
    self.call_stack.get_var_depth(self.engine, varname, access)
  }

  /// Defines a new variable in the current scope.
  #[inline(always)]
  pub fn def_var_value(&mut self, varname: &str, access: AccessPathKind, val: RantValue, is_const: bool) -> RuntimeResult<()> {
    self.call_stack.def_var_value(self.engine, varname, access, val, is_const)
  }
  
  /// Returns `true` if the call stack is currently empty.
  #[inline(always)]
  fn is_stack_empty(&self) -> bool {
    self.call_stack.is_empty()
  }

  /// Pushes a value onto the value stack.
  #[inline(always)]
  pub fn push_val(&mut self, val: RantValue) -> RuntimeResult<usize> {
    if self.val_stack.len() < MAX_STACK_SIZE {
      runtime_trace!("value stack <- {}", &val);
      self.val_stack.push(val);
      Ok(self.val_stack.len())
    } else {
      runtime_error!(RuntimeErrorType::StackOverflow, "value stack has overflowed");
    }
  }

  /// Removes and returns the topmost value from the value stack.
  #[inline(always)]
  pub fn pop_val(&mut self) -> RuntimeResult<RantValue> {
    if let Some(val) = self.val_stack.pop() {
      runtime_trace!("value stack -> {}", &val);
      Ok(val)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "value stack has underflowed");
    }
  }

  /// Removes and returns the topmost frame from the call stack.
  #[inline(always)]
  pub fn pop_frame(&mut self) -> RuntimeResult<StackFrame<Intent>> {
    runtime_trace!("pop_frame: {} -> {}", self.call_stack.len(), self.call_stack.len() - 1);
    if let Some(frame) = self.call_stack.pop_frame() {
      Ok(frame)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "call stack has underflowed");
    }
  }

  /// Pushes a frame onto the call stack without overflow checks.
  #[inline(always)]
  fn push_frame_unchecked(&mut self, callee: Rc<Sequence>, flavor: StackFrameFlavor) {
    runtime_trace!("push_frame_unchecked");
    let frame = StackFrame::new(
      callee, 
      self.call_stack.top().map(|last| last.output())
    ).with_flavor(flavor);

    self.call_stack.push_frame(frame);
  }
  
  /// Pushes a frame onto the call stack.
  #[inline(always)]
  pub fn push_frame(&mut self, callee: Rc<Sequence>, has_scope: bool) -> RuntimeResult<()> {
    runtime_trace!("push_frame");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let frame = if has_scope {
      StackFrame::new(
        callee,
        self.call_stack.top().map(|last| last.output())
      )
    } else {
      StackFrame::new(
        callee,
        self.call_stack.top().map(|last| last.output())
      ).without_scope()
    };

    self.call_stack.push_frame(frame);
    Ok(())
  }

  /// Pushes an empty frame onto the call stack with a single `RuntimeCall` intent.
  pub fn push_native_call_frame(&mut self, callee: Box<dyn FnOnce(&mut VM) -> RuntimeResult<()>>, flavor: StackFrameFlavor) -> RuntimeResult<()> {
    runtime_trace!("push_native_call_frame");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }

    let last_frame = self.call_stack.top().unwrap();

    let mut frame = StackFrame::with_extended_config(
      None,
      self.call_stack.top().map(|last| last.output()),
      Rc::clone(last_frame.origin()),
      true,
      last_frame.debug_pos(),
      StackFrameFlavor::Original
    ).with_flavor(flavor);

    frame.push_intent(Intent::RuntimeCall {
      function: callee,
      interrupt: true,
    });

    self.call_stack.push_frame(frame);
    Ok(())
  }

  /// Pushes a flavored frame onto the call stack.
  #[inline(always)]
  pub fn push_frame_flavored(&mut self, callee: Rc<Sequence>, flavor: StackFrameFlavor) -> RuntimeResult<()> {
    runtime_trace!("push_frame_flavored");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let frame = StackFrame::new(
      callee,
      self.call_stack.top().map(|last| last.output())
    ).with_flavor(flavor);

    self.call_stack.push_frame(frame);
    Ok(())
  }

  /// Interrupts execution of a repeater and either continues the next iteration or exits the block.
  #[inline]
  pub fn interrupt_repeater(&mut self, break_val: Option<RantValue>, should_continue: bool) -> RuntimeResult<()> {
    if let Some(block_depth) = self.call_stack.taste_for_first(StackFrameFlavor::RepeaterElement) {
      // Tell the active block to stop running if it's a break
      if !should_continue {
        self.resolver_mut().active_repeater_mut().unwrap().force_stop();
      }

      // Pop down to owning scope of block
      if let Some(break_val) = break_val {
        for _ in 0..=block_depth {
          self.pop_frame()?;
        }
        self.push_val(break_val)?;
      } else {
        for i in 0..=block_depth {
          let old_frame_output = self.pop_frame()?.into_output();
          if i < block_depth {
            self.cur_frame_mut().write_value(old_frame_output);
          } else {
            self.push_val(old_frame_output)?;
          }
        }
      }

      // Make sure to pop off any blocks that are on top of the repeater, or weird stuff happens
      while !self.resolver().active_block().unwrap().is_repeater() {
        self.resolver_mut().pop_block();
      }      
      
      Ok(())
    } else {
      runtime_error!(RuntimeErrorType::ControlFlowError, "no reachable repeater to interrupt");
    }
  }

  /// Returns from the currently running function.
  #[inline]
  pub fn func_return(&mut self, ret_val: Option<RantValue>) -> RuntimeResult<()> {
    runtime_trace!("func_return");
    if let Some(block_depth) = self.call_stack.taste_for_first(StackFrameFlavor::FunctionBody) {
      // Pop down to owning scope of function
      if let Some(break_val) = ret_val {
        for _ in 0..=block_depth {
          self.pop_frame()?;
        }
        self.push_val(break_val)?;
      } else {
        for i in 0..=block_depth {
          let old_frame = self.pop_frame()?;
          let old_frame_flavor = old_frame.flavor();
          let old_frame_output = old_frame.into_output();

          // If a block state is associated with the popped frame, pop that too
          match old_frame_flavor {
            StackFrameFlavor::RepeaterElement | StackFrameFlavor::BlockElement => {
              self.resolver_mut().pop_block();
            },
            _ => {}
          }

          // Handle output
          if i < block_depth {
            self.cur_frame_mut().write_value(old_frame_output);
          } else {
            self.push_val(old_frame_output)?;
          }
        }
      }
      
      Ok(())
    } else {
      runtime_error!(RuntimeErrorType::ControlFlowError, "no reachable function to return from");
    }
  }

  /// Gets a mutable reference to the topmost frame on the call stack.
  #[inline(always)]
  pub fn cur_frame_mut(&mut self) -> &mut StackFrame<Intent> {
    self.call_stack.top_mut().unwrap()
  }

  /// Safely attempts to get a mutable reference to the topmost frame on the call stack.
  #[inline(always)]
  pub fn any_cur_frame_mut(&mut self) -> Option<&mut StackFrame<Intent>> {
    self.call_stack.top_mut()
  }

  /// Safely attempts to get a mutable reference to the frame `depth` frames below the top of the call stack.
  #[inline(always)]
  pub fn parent_frame_mut(&mut self, depth: usize) -> Option<&mut StackFrame<Intent>> {
    self.call_stack.parent_mut(depth)
  }

  /// Safely attempts to get a reference to the frame `depth` frames below the top of the call stack.
  #[inline(always)]
  pub fn parent_frame(&self, depth: usize) -> Option<&StackFrame<Intent>> {
    self.call_stack.parent(depth)
  }

  /// Gets a reference to the topmost frame on the call stack.
  #[inline(always)]
  pub fn cur_frame(&self) -> &StackFrame<Intent> {
    self.call_stack.top().unwrap()
  }

  /// Gets a reference to the topmost RNG on the RNG stack.
  #[inline(always)]
  pub fn rng(&self) -> &RantRng {
    self.rng_stack.last().unwrap().as_ref()
  }

  /// Gets a copy of the topmost RNG on the RNG stack.
  #[inline(always)]
  pub fn rng_clone(&self) -> Rc<RantRng> {
    Rc::clone(self.rng_stack.last().unwrap())
  }

  /// Adds a new RNG to the top of the RNG stack.
  #[inline]
  pub fn push_rng(&mut self, rng: Rc<RantRng>) {
    self.rng_stack.push(rng);
  }

  /// Removes the topmost RNG from the RNG stack and returns it.
  #[inline]
  pub fn pop_rng(&mut self) -> Option<Rc<RantRng>> {
    if self.rng_stack.len() <= 1 {
      return None
    }

    self.rng_stack.pop()
  }

  /// Gets a reference to the Rant context that created the VM.
  #[inline(always)]
  pub fn context(&self) -> &Rant {
    self.engine
  }

  /// Gets a mutable reference to the Rant context that created the VM.
  #[inline(always)]
  pub fn context_mut(&mut self) -> &mut Rant {
    self.engine
  }

  /// Gets a reference to the resolver associated with the VM.
  #[inline(always)]
  pub fn resolver(&self) -> &Resolver {
    &self.resolver
  }

  /// Gets a mutable reference to the resolver associated with the VM.
  #[inline(always)]
  pub fn resolver_mut(&mut self) -> &mut Resolver {
    &mut self.resolver
  }

  /// Gets a reference to the program being executed by the VM.
  #[inline(always)]
  pub fn program(&self) -> &RantProgram {
    self.program
  }

  #[inline]
  pub fn push_unwind_state(&mut self, handler: Option<RantFunctionHandle>) {
    self.unwinds.push(UnwindState {
      handler,
      call_stack_size: self.call_stack.len(),
      value_stack_size: self.val_stack.len(),
      block_stack_size: self.resolver.block_stack_len(),
      attr_stack_size: self.resolver.count_attrs(),
    });
  }

  #[inline]
  pub fn unwind(&mut self) -> Option<UnwindState> {
    let state = self.unwinds.pop();

    if let Some(state) = &state {
      // Unwind call stack
      while self.call_stack.len() > state.call_stack_size {
        self.call_stack.pop_frame();
      }

      // Unwind value stack
      while self.val_stack.len() > state.value_stack_size {
        self.val_stack.pop();
      }

      // Unwind block stack
      while self.resolver.block_stack_len() > state.block_stack_size {
        self.resolver.pop_block();
      }

      // Unwind attribute stack
      while self.resolver.count_attrs() > state.attr_stack_size {
        self.resolver.pop_attrs();
      }
    }

    state
  }
}