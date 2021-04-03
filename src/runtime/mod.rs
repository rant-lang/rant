pub use stack::*;

use crate::*;
use crate::lang::*;
use crate::util::*;
use self::resolver::*;

use std::{cell::RefCell, error::Error, fmt::{Debug, Display}, ops::Deref, rc::Rc};
use smallvec::{SmallVec, smallvec};


pub(crate) mod format;
pub(crate) mod resolver;
mod output;
mod stack;

/// Type alias for `Result<T, RuntimeError>`
pub type RuntimeResult<T> = Result<T, RuntimeError>;

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
  call_stack: CallStack,
  resolver: Resolver,
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
      description: e.to_string(),
      error_type: e,
      stack_trace: None,
    })
  }};
  ($err_type:expr, $desc:expr) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: $desc.to_string(),
      stack_trace: None,
    })
  };
}

/// Intents are actions queued on a stack frame that are performed before the frame runs.
///
/// ## "Call" or "Invoke"?
/// In the context of Rant runtime intents, "calling" and "invoking" have specific meanings:
/// * "Invoke" means that argument expressions potentially need to be evaluated before the call can proceed;
/// * "Call" means that all argument values are already known (either in the intent or on the value stack).
pub enum Intent {
  /// Pop a value off the value stack and print it to the current frame's output.
  PrintLast,
  /// Pops a value off the value stack and returns it from the current function.
  ReturnLast,
  /// Pops a value off the value stack and continues to the next repeater iteration with it.
  ContinueLast,
  /// Pops a value off the value stack and breaks from the current repeater with it.
  BreakLast,
  /// Pops a map off the stack and loads it as a module with the specified name.
  ImportLastAsModule { module_name: String, descope: usize },
  /// Check if the active block is finished and either continue the block or pop the state from the stack
  CheckBlock,
  /// Pop a value off the stack and assign it to an existing variable.
  SetVar { vname: Identifier, access_kind: AccessPathKind, },
  /// Pop a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier, access_kind: AccessPathKind, is_const: bool },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `GetValue`.
  BuildDynamicGetter { 
    path: Rc<AccessPath>, dynamic_key_count: usize, pending_exprs: Vec<Rc<Sequence>>, 
    override_print: bool, prefer_function: bool, fallback: Option<Rc<Sequence>> 
  },
  /// Pop `dynamic_key_count` values off the stack and use them for expression fields in a getter.
  GetValue { path: Rc<AccessPath>, dynamic_key_count: usize, override_print: bool, prefer_function: bool, fallback: Option<Rc<Sequence>> },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `SetValue`.
  BuildDynamicSetter { path: Rc<AccessPath>, write_mode: VarWriteMode, expr_count: usize, pending_exprs: Vec<Rc<Sequence>>, val_source: SetterValueSource },
  /// Pop `expr_count` values off the stack and use them for expression fields in a setter.
  SetValue { path: Rc<AccessPath>, write_mode: VarWriteMode, expr_count: usize },
  /// Evaluate `arg_exprs` in order, then pop the argument values off the stack, pop a function off the stack, and pass the arguments to the function.
  Invoke { arg_exprs: Rc<Vec<ArgumentExpr>>, eval_count: usize, flag: PrintFlag, is_temporal: bool, },
  /// Invoke a single function in a composed function call chain.
  InvokeComposedStep { 
    /// All steps in the entire composed function call
    steps: Rc<Vec<FunctionCall>>,
    /// The current step being executed
    step_index: usize, 
    /// Current state of the intent.
    state: InvokeComposedStepState,
    /// The composition value from the last step
    compval: Option<RantValue>,
    /// The print flag to use.
    flag: PrintFlag,
  },
  /// Pop `argc` args off the stack, then pop a function off the stack and call it with the args.
  Call { argc: usize, flag: PrintFlag, override_print: bool },
  /// Call a function for every variant of a temporal argument set and increment the provided temporal state.
  CallTemporal { func: RantFunctionRef, args: Rc<Vec<RantValue>>, temporal_state: TemporalSpreadState, flag: PrintFlag, },
  /// Pop value from stack and add it to a list. If `index` is out of range, print the list.
  BuildList { init: Rc<Vec<Rc<Sequence>>>, index: usize, list: RantList },
  /// Pop value and optional key from stack and add them to a map. If `pair_index` is out of range, print the map.
  BuildMap { init: Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>, pair_index: usize, map: RantMap },
  /// Evaluate block weights and then run the block
  BuildWeightedBlock { block: Rc<Block>, weights: Weights, pop_next_weight: bool, },
  /// Calls a function that accepts a mutable reference to the current runtime. Optionally interrupts the intent loop to force another tick.
  RuntimeCall { function: Box<dyn FnOnce(&mut VM) -> RuntimeResult<()>>, interrupt: bool },
  /// Drops all unwind states that are no longer within the call stack.
  DropStaleUnwinds,
}

impl Intent {
  fn name(&self) -> &'static str {
    match self {
      Intent::PrintLast => "print",
      Intent::CheckBlock => "check_block",
      Intent::SetVar { .. } => "set_var",
      Intent::DefVar { .. } => "def_var",
      Intent::BuildDynamicGetter { .. } => "build_dyn_getter",
      Intent::GetValue { .. } => "get_value",
      Intent::BuildDynamicSetter { .. } => "build_dyn_setter",
      Intent::SetValue { .. } => "set_value",
      Intent::Invoke { .. } => "invoke",
      Intent::InvokeComposedStep { .. } => "invoke_composed_step",
      Intent::Call { .. } => "call",
      Intent::CallTemporal { .. } => "call_temporal",
      Intent::BuildList { .. } => "build_list",
      Intent::BuildMap { .. } => "build_map",
      Intent::ImportLastAsModule { .. } => "load_module",
      Intent::RuntimeCall { .. } => "runtime_call",
      Intent::DropStaleUnwinds => "drop_stale_unwinds",
      Intent::ReturnLast => "return_last",
      Intent::ContinueLast => "continue_last",
      Intent::BreakLast => "break_last",
      Intent::BuildWeightedBlock { .. } => "build_weighted_block",
    }
  }
}

/// States for the `InvokeComposedStep` intent.
#[derive(Debug)]
pub enum InvokeComposedStepState {
  /// Evaluate step function and leave it on the value stack.
  ///
  /// Transitions to `EvaluatingArgs`.
  EvaluatingFunc,
  /// Evaluate argument expressions, then pop them off the value stack.
  /// Then, before transitioning, pop the function off the value stack and store it.
  ///
  /// Transitions to `PreCall` or `PreTemporalCall`.
  EvaluatingArgs { 
    /// Number of arguments that have already been evaluated.
    num_evaluated: usize 
  },
  /// Temporal step function is ready to iterate.
  ///
  /// Transitions to `PostTemporalCall`.
  PreTemporalCall {
    step_function: RantFunctionRef,
    temporal_state: TemporalSpreadState,
    args: Vec<RantValue>,
  },
  /// Step function is ready to call.
  ///
  /// Transitions to `PostCall`.
  PreCall { 
    step_function: RantFunctionRef,
    args: Vec<RantValue>,
  },
  /// Step function has returned and output can be used.
  PostCall,
  /// Temporal step function has iterated and output can be used.
  ///
  /// Might transition to `PreTemporalCall`.
  PostTemporalCall {
    step_function: RantFunctionRef,
    temporal_state: TemporalSpreadState,
    args: Vec<RantValue>,
  }
}

/// Defines variable write modes for setter intents.
/// Used by function definitions to control conditional definition behavior.
#[derive(Debug, Copy, Clone)]
pub enum VarWriteMode {
  /// Only set existing variables.
  SetOnly,
  /// Defines and sets a variable.
  Define,
  /// Defines and sets a new constant.
  DefineConst,
}

#[derive(Debug)]
enum SetterKey<'a> {
  Index(i64),
  Slice(Slice),
  KeyRef(&'a str),
  KeyString(InternalString),
}

/// Describes where a setter gets its RHS value.
#[derive(Debug)]
pub enum SetterValueSource {
  /// Setter RHS is evaluated from an expression.
  FromExpression(Rc<Sequence>),
  /// Setter RHS is a value.
  FromValue(RantValue),
  /// Setter RHS was already consumed.
  Consumed
}

pub struct UnwindState {
  pub handler: Option<RantFunctionRef>,
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
    // Push the program's root sequence onto the call stack
    // This doesn't need an overflow check because it will *always* succeed
    self.push_frame_unchecked(self.program.root.clone(), true, StackFrameFlavor::FunctionBody);
    
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
              self.call_func(handler, vec![RantValue::String(err.to_string().into())], PrintFlag::None, false)?;
              continue;
            }
          } else {
            return Err(err)
          }
        }
      }
    }

    // Value stack should *always* be 1 when program ends.
    debug_assert_eq!(self.val_stack.len(), 1, "value stack is imbalanced");
    
    // Once stack is empty, program is done-- return last frame's output
    Ok(self.pop_val().unwrap_or_default())
  }

  #[inline(always)]
  fn tick(&mut self) -> RuntimeResult<bool> {
    runtime_trace!("tick start (stack @ {}: {})", self.call_stack.len(), self.call_stack.top().map_or("none".to_owned(), |top| top.to_string()));
    // Read frame's current intents and handle them before running the sequence
    while let Some(intent) = self.cur_frame_mut().take_intent() {
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
        Intent::CheckBlock => {            
          self.check_block()?;
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
                  self.cur_frame_mut().push_intent_front(Intent::BuildWeightedBlock {
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
          self.push_block(block.as_ref(), Some(weights), block.flag)?;
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
              self.cur_frame_mut().push_intent_front(Intent::GetValue { path, dynamic_key_count, override_print, prefer_function, fallback });
            } else {
              self.cur_frame_mut().push_intent_front(Intent::BuildDynamicGetter { path, dynamic_key_count, pending_exprs, override_print, prefer_function, fallback });
            }
            self.push_frame_flavored(Rc::clone(&key_expr), true, StackFrameFlavor::DynamicKeyExpression)?;
          } else {
            self.cur_frame_mut().push_intent_front(Intent::GetValue { path, dynamic_key_count, override_print, prefer_function, fallback });
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
                self.cur_frame_mut().push_intent_front(Intent::PrintLast);
              }
              self.push_frame(fallback, true)?;
              return Ok(true)
            }
          }
        },
        Intent::Invoke { arg_exprs, eval_count, flag, is_temporal } => {
          // First, evaluate all arguments
          if eval_count < arg_exprs.len() {
            let arg_expr = arg_exprs.get(arg_exprs.len() - eval_count - 1).unwrap();
            let arg_seq = Rc::clone(&arg_expr.expr);
            self.cur_frame_mut().push_intent_front(Intent::Invoke { arg_exprs, eval_count: eval_count + 1, flag, is_temporal, });
            self.push_frame_flavored(arg_seq, true, StackFrameFlavor::ArgumentExpression)?;
            return Ok(true)
          } else {
            // Pop the evaluated args off the stack
            let mut args = vec![];
            for arg_expr in arg_exprs.iter() {
              let arg = self.pop_val()?;
              // When parametric spread is used and the argument is a list, expand its values into individual args
              if matches!(arg_expr.spread_mode, ArgumentSpreadMode::Parametric) {
                if let RantValue::List(list_ref) = &arg {
                  for spread_arg in list_ref.borrow().iter() {
                    args.push(spread_arg.clone());
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
                self.cur_frame_mut().push_intent_front(Intent::CallTemporal { 
                  func,
                  temporal_state, 
                  args: Rc::new(args), 
                  flag
                });
              }
            } else {
              self.call_func(func, args, flag, false)?;
            }
            
            return Ok(true)
          }
        },
        Intent::InvokeComposedStep { 
          steps, 
          step_index, 
          state,
          compval, 
          flag 
        } => {          
          match state {
            InvokeComposedStepState::EvaluatingFunc => {
              let step = &steps[step_index];
              let compval_copy = compval.clone();

              self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                steps: Rc::clone(&steps),
                step_index,
                state: InvokeComposedStepState::EvaluatingArgs { num_evaluated: 0 },
                compval,
                flag,
              });

              match &step.target {
                FunctionCallTarget::Path(path) => {
                  // TODO: expose compval to path-based function access in compositions
                  self.push_getter_intents(path, true, true, None);
                },
                FunctionCallTarget::Expression(expr) => {
                  self.push_frame(Rc::clone(expr), true)?;
                  if let Some(compval) = compval_copy {
                    self.def_compval(compval)?;
                  }
                },
              }
              return Ok(true)
            },
            InvokeComposedStepState::EvaluatingArgs { num_evaluated } => {
              let step = &steps[step_index];
              let arg_exprs = &step.arguments;
              let argc = arg_exprs.len();
              if num_evaluated < argc {                
                // Evaluate next argument
                let arg_expr = arg_exprs.get(argc - num_evaluated - 1).unwrap();
                let arg_seq = Rc::clone(&arg_expr.expr);
                let compval_copy = compval.clone();

                // Prepare next arg eval intent
                self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep { 
                  steps: Rc::clone(&steps),
                  step_index,
                  state: InvokeComposedStepState::EvaluatingArgs {
                    num_evaluated: num_evaluated + 1,
                  },
                  compval,
                  flag,
                });

                // Push current argument expression to call stack
                self.push_frame_flavored(arg_seq, true, StackFrameFlavor::ArgumentExpression)?;
                if let Some(compval) = compval_copy {
                  self.def_compval(compval)?;
                }
              } else {
                // If all args are evaluated, pop them off the stack
                let mut args = vec![];
                for arg_expr in arg_exprs.iter() {
                  let arg = self.pop_val()?;
                  // When parametric spread is used and the argument is a list, expand its values into individual args
                  if matches!(arg_expr.spread_mode, ArgumentSpreadMode::Parametric) {
                    if let RantValue::List(list_ref) = &arg {
                      for spread_arg in list_ref.borrow().iter() {
                        args.push(spread_arg.clone());
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
                self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                  state: if step.is_temporal {  
                    InvokeComposedStepState::PreTemporalCall {
                      step_function,
                      temporal_state: TemporalSpreadState::new(arg_exprs.as_slice(), args.as_slice()),
                      args,
                    }
                  } else {
                    InvokeComposedStepState::PreCall { step_function, args }
                  },
                  steps,
                  step_index,
                  compval,
                  flag,
                });
              }
              return Ok(true)
            },
            InvokeComposedStepState::PreCall { step_function, args } => {
              // Transition intent to PostCall after function returns
              self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                steps,
                step_index,
                state: InvokeComposedStepState::PostCall,
                compval,
                flag,
              });

              // Call it and interrupt
              self.call_func(step_function, args, PrintFlag::None, true)?;
              return Ok(true)
            },
            InvokeComposedStepState::PostCall => {
              let next_compval = self.pop_val()?;
              let next_step_index = step_index + 1;
              // Check if there is a next step
              if next_step_index < steps.len() {
                // Create intent for next step
                self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                  steps,
                  step_index: next_step_index,
                  state: InvokeComposedStepState::EvaluatingFunc,
                  compval: Some(next_compval),
                  flag,
                });
                return Ok(true)
              } else {
                // If there are no more steps in the chain, just print the compval and let this intent die
                self.cur_frame_mut().write_value(next_compval);
              }
            },
            InvokeComposedStepState::PreTemporalCall { step_function, args, temporal_state } => {
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
              self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                steps,
                step_index,
                state: InvokeComposedStepState::PostTemporalCall {
                  step_function: Rc::clone(&step_function),
                  temporal_state,
                  args,
                },
                compval,
                flag,
              });

              self.call_func(step_function, targs, PrintFlag::None, true)?;
              return Ok(true)
            },
            InvokeComposedStepState::PostTemporalCall { step_function, args, mut temporal_state } => {
              let next_compval = self.pop_val()?;
              let next_step_index = step_index + 1;
              let step_count = steps.len();

              // Queue next iteration if available
              if temporal_state.increment() {
                self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                  steps: Rc::clone(&steps),
                  step_index,
                  state: InvokeComposedStepState::PreTemporalCall {
                    step_function,
                    temporal_state,
                    args,
                  },
                  compval,
                  flag,
                })
              }

              // Call next function in chain
              if next_step_index < step_count {
                // Create intent for next step
                self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
                  steps,
                  step_index: next_step_index,
                  state: InvokeComposedStepState::EvaluatingFunc,
                  compval: Some(next_compval),
                  flag,
                });
                return Ok(true)
              } else {
                // If there are no more steps in the chain, just print the compval and let this intent die
                self.cur_frame_mut().write_value(next_compval);
              }
            },
          }
        },
        Intent::CallTemporal { func, args, mut temporal_state, flag } => {
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
            self.cur_frame_mut().push_intent_front(Intent::CallTemporal { func: Rc::clone(&func), args, temporal_state, flag });
          }

          self.call_func(func, targs, flag, false)?;
          return Ok(true)
        },
        Intent::Call { argc, flag, override_print } => {
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
          self.call_func(func, args, flag, override_print)?;
          return Ok(true)
        },
        Intent::BuildDynamicSetter { path, write_mode, expr_count, mut pending_exprs, val_source } => {
          // Prepare setter value
          match val_source {
            // Value must be evaluated from an expression
            SetterValueSource::FromExpression(expr) => {
              self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter { path, write_mode, expr_count, pending_exprs, val_source: SetterValueSource::Consumed });
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
              self.cur_frame_mut().push_intent_front(Intent::SetValue { path, write_mode, expr_count });
            } else {
              // Continue building setter
              self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter { path, write_mode, expr_count, pending_exprs, val_source: SetterValueSource::Consumed });                
            }
            self.push_frame_flavored(Rc::clone(&key_expr), true, StackFrameFlavor::DynamicKeyExpression)?;
          } else {
            self.cur_frame_mut().push_intent_front(Intent::SetValue { path, write_mode, expr_count });
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
            self.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))))
          } else {
            // Continue list creation
            self.cur_frame_mut().push_intent_front(Intent::BuildList { init: Rc::clone(&init), index: index + 1, list });
            let val_expr = &init[index];
            self.push_frame(Rc::clone(val_expr), true)?;
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
            self.cur_frame_mut().write_value(RantValue::Map(Rc::new(RefCell::new(map))));
          } else {
            // Continue map creation
            self.cur_frame_mut().push_intent_front(Intent::BuildMap { init: Rc::clone(&init), pair_index: pair_index + 1, map });
            let (key_expr, val_expr) = &init[pair_index];
            if let MapKeyExpr::Dynamic(key_expr) = key_expr {
              // Push dynamic key expression onto call stack
              self.push_frame(Rc::clone(&key_expr), true)?;
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
            self.engine.set_global(crate::MODULES_CACHE_KEY, RantValue::Map(Rc::new(RefCell::new(cache))));
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
        }
      }
    }

    runtime_trace!("intents finished");
    
    // Run frame's sequence elements in order
    while let Some(rst) = &self.cur_frame_mut().seq_next() {
      match Rc::deref(rst) {        
        Rst::ListInit(elements) => {
          self.cur_frame_mut().push_intent_front(Intent::BuildList { init: Rc::clone(elements), index: 0, list: RantList::with_capacity(elements.len()) });
          return Ok(true)
        },
        Rst::MapInit(elements) => {
          self.cur_frame_mut().push_intent_front(Intent::BuildMap { init: Rc::clone(elements), pair_index: 0, map: RantMap::new() });
          return Ok(true)
        },
        Rst::Block(block) => {
          self.pre_push_block(&block, block.flag)?;
          return Ok(true)
        },
        Rst::VarDef(vname, access_kind, val_expr) => {
          if let Some(val_expr) = val_expr {
            // If a value is present, it needs to be evaluated first
            self.cur_frame_mut().push_intent_front(Intent::DefVar { vname: vname.clone(), access_kind: *access_kind, is_const: false });
            self.push_frame(Rc::clone(val_expr), true)?;
            return Ok(true)
          } else {
            // If there's no assignment, just set it to empty value
            self.def_var_value(vname.as_str(), *access_kind, RantValue::Empty, false)?;
          }
        },
        Rst::ConstDef(vname, access_kind, val_expr) => {
          if let Some(val_expr) = val_expr {
            // If a value is present, it needs to be evaluated first
            self.cur_frame_mut().push_intent_front(Intent::DefVar { vname: vname.clone(), access_kind: *access_kind, is_const: true });
            self.push_frame(Rc::clone(val_expr), true)?;
            return Ok(true)
          } else {
            // If there's no assignment, just set it to empty value
            self.def_var_value(vname.as_str(), *access_kind, RantValue::Empty, true)?;
          }
        },
        Rst::VarGet(path, fallback) => {
          self.push_getter_intents(path, false, false, fallback.as_ref().map(Rc::clone));
          return Ok(true)
        },
        Rst::VarSet(path, val_expr) => {
          // Get list of dynamic expressions in path
          let exprs = path.dynamic_exprs();

          if exprs.is_empty() {
            // Setter is static, so run it directly
            self.cur_frame_mut().push_intent_front(Intent::SetValue { path: Rc::clone(&path), write_mode: VarWriteMode::SetOnly, expr_count: 0 });
            self.push_frame(Rc::clone(&val_expr), true)?;
          } else {
            // Build dynamic keys before running setter
            self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter {
              expr_count: exprs.len(),
              write_mode: VarWriteMode::SetOnly,
              path: Rc::clone(path),
              pending_exprs: exprs,
              val_source: SetterValueSource::FromExpression(Rc::clone(val_expr))
            });
          }
          return Ok(true)
        },
        Rst::FuncDef(fdef) => {
          let FunctionDef { 
            path, 
            body, 
            params, 
            capture_vars,
            is_const,
          } = fdef;

          // Capture variables
          let mut captured_vars = vec![];
          for capture_id in capture_vars.iter() {
            let var = self.call_stack.get_var_mut(&mut self.engine, capture_id, AccessPathKind::Local)?;
            var.make_by_ref();
            captured_vars.push((capture_id.clone(), var.clone()));
          }

          let func = RantValue::Function(Rc::new(RantFunction {
            params: Rc::clone(params),
            body: RantFunctionInterface::User(Rc::clone(body)),
            captured_vars,
            min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
            vararg_start_index: params.iter()
              .enumerate()
              .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
              .unwrap_or_else(|| params.len()),
            flavor: None,
          }));

          let dynamic_keys = path.dynamic_exprs();

          self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter {
            expr_count: dynamic_keys.len(),
            write_mode: if *is_const { VarWriteMode::DefineConst } else { VarWriteMode::Define },
            pending_exprs: dynamic_keys,
            path: Rc::clone(path),
            val_source: SetterValueSource::FromValue(func)
          });

          return Ok(true)
        },
        Rst::Closure(closure_expr) => {
          let ClosureExpr {
            capture_vars,
            body: expr,
            params,
          } = closure_expr;

          // Capture variables
          let mut captured_vars = vec![];
          for capture_id in capture_vars.iter() {
            let var = self.call_stack.get_var_mut(&mut self.engine, capture_id, AccessPathKind::Local)?;
            var.make_by_ref();
            captured_vars.push((capture_id.clone(), var.clone()));
          }

          let func = RantValue::Function(Rc::new(RantFunction {
            params: Rc::clone(params),
            body: RantFunctionInterface::User(Rc::clone(&expr)),
            captured_vars,
            min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
            vararg_start_index: params.iter()
              .enumerate()
              .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
              .unwrap_or_else(|| params.len()),
            flavor: None,
          }));

          self.cur_frame_mut().write_value(func);
        },
        Rst::FuncCall(fcall) => {
          let FunctionCall {
            target,
            arguments,
            flag,
            is_temporal,
          } = fcall;

          match target {
            // Named function call
            FunctionCallTarget::Path(path) => {
              // Queue up the function call behind the dynamic keys
              self.cur_frame_mut().push_intent_front(Intent::Invoke {
                eval_count: 0,
                arg_exprs: Rc::clone(arguments),
                flag: *flag,
                is_temporal: *is_temporal,
              });

              self.push_getter_intents(path, true, true, None);
            },
            // Anonymous function call
            FunctionCallTarget::Expression(expr) => {
              // Evaluate arguments after function is evaluated
              self.cur_frame_mut().push_intent_front(Intent::Invoke {
                arg_exprs: Rc::clone(arguments),
                eval_count: 0,
                flag: *flag,
                is_temporal: *is_temporal,
              });

              // Push function expression onto stack
              self.push_frame(Rc::clone(expr), true)?;
            },
          }
          return Ok(true)
        },
        Rst::ComposedCall(compcall) => {     
          self.cur_frame_mut().push_intent_front(Intent::InvokeComposedStep {
            steps: Rc::clone(&compcall.steps),
            step_index: 0,
            state: InvokeComposedStepState::EvaluatingFunc,
            compval: None,
            flag: compcall.flag,
          });
          return Ok(true)
        },
        Rst::ComposeValue => {
          let compval = self.get_var_value(COMPOSE_VALUE_NAME, AccessPathKind::Local, false)?;
          self.cur_frame_mut().write_value(compval);
        },
        Rst::DebugCursor(info) => {
          self.cur_frame_mut().set_debug_info(info);
        },
        Rst::Fragment(frag) => self.cur_frame_mut().write_frag(frag),
        Rst::Whitespace(ws) => self.cur_frame_mut().write_ws(ws),
        Rst::Integer(n) => self.cur_frame_mut().write_value(RantValue::Int(*n)),
        Rst::Float(n) => self.cur_frame_mut().write_value(RantValue::Float(*n)),
        Rst::EmptyVal => self.cur_frame_mut().write_value(RantValue::Empty),
        Rst::Boolean(b) => self.cur_frame_mut().write_value(RantValue::Boolean(*b)),
        Rst::BlockValue(block) => self.cur_frame_mut().write_value(RantValue::Block(Rc::clone(block))),
        Rst::Nop => {},
        Rst::Return(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent_front(Intent::ReturnLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.func_return(None)?;
            return Ok(true)
          }
        },
        Rst::Continue(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent_front(Intent::ContinueLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.interrupt_repeater(None, true)?;
            return Ok(true)
          }
        },
        Rst::Break(expr) => {
          if let Some(expr) = expr {
            self.cur_frame_mut().push_intent_front(Intent::BreakLast);
            self.push_frame(Rc::clone(expr), true)?;
            continue
          } else {
            self.interrupt_repeater(None, false)?;
            return Ok(true)
          }
        },
        rst => {
          runtime_error!(RuntimeErrorType::InternalError, format!("unsupported node type: '{}'", rst.display_name()));
        },
      }
    }

    runtime_trace!("frame done: {}", self.call_stack.len());
    
    // Pop frame once its sequence is finished
    let mut last_frame = self.pop_frame()?;
    if let Some(output) = last_frame.render_output_value() {
      self.push_val(output)?;
    }
    
    Ok(false)
  }

  #[inline(always)]
  pub fn push_getter_intents(&mut self, path: &Rc<AccessPath>, override_print: bool, prefer_function: bool, fallback: Option<Rc<Sequence>>) {
    let dynamic_keys = path.dynamic_exprs();

    // Run the getter to retrieve the function we're calling first...
    self.cur_frame_mut().push_intent_front(if dynamic_keys.is_empty() {
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
  pub fn call_func(&mut self, func: RantFunctionRef, mut args: Vec<RantValue>, flag: PrintFlag, override_print: bool) -> RuntimeResult<()> {
    let argc = args.len();
    let is_printing = !flag.is_sink();

    // Verify the args fit the signature
    let mut args = if func.is_variadic() {
      if argc < func.min_arg_count {
        runtime_error!(RuntimeErrorType::ArgumentMismatch, format!("arguments don't match; expected at least {}, found {}", func.min_arg_count, argc))
      }

      // Condense args to turn variadic args into one arg
      // Only do this for user functions, since native functions take care of variadic condensation already
      if !func.is_native() {
        let mut condensed_args = args.drain(0..func.vararg_start_index).collect::<Vec<RantValue>>();
        let vararg = RantValue::List(Rc::new(RefCell::new(args.into_iter().collect::<RantList>())));
        condensed_args.push(vararg);
        condensed_args
      } else {
        args
      }
    } else {
      if argc < func.min_arg_count || argc > func.params.len() {
        runtime_error!(RuntimeErrorType::ArgumentMismatch, format!("arguments don't match; expected {}, found {}", func.min_arg_count, argc))
      }
      args
    };

    // Tell frame to print output if it's available
    if is_printing && !override_print {
      self.cur_frame_mut().push_intent_front(Intent::PrintLast);
    }

    // Call the function
    match &func.body {
      RantFunctionInterface::Foreign(foreign_func) => {
        let foreign_func = Rc::clone(foreign_func);
        self.push_native_call_frame(Box::new(move |vm| foreign_func(vm, args)), is_printing, StackFrameFlavor::NativeCall)?;
      },
      RantFunctionInterface::User(user_func) => {
        // Push the function onto the call stack
        self.push_frame_flavored(Rc::clone(user_func), is_printing, func.flavor.unwrap_or(StackFrameFlavor::FunctionBody))?;

        // Pass the args to the function scope
        let mut args = args.drain(..);
        for param in func.params.iter() {
          self.call_stack.def_var_value(
            self.engine, 
            param.name.as_str(), 
            AccessPathKind::Local, 
            args.next().unwrap_or(RantValue::Empty),
            true,
          )?;
        }

        // Pass captured vars to the function scope
        for (capture_name, capture_var) in func.captured_vars.iter() {
          self.call_stack.def_local_var(
            capture_name.as_str(),
            RantVar::clone(capture_var)
          )?;
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
      _ => unreachable!()
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
              let key = InternalString::from(key_val.to_string());
              SetterKey::KeyString(key)
            }
          }
        },
        // Anonymous value (not allowed)
        AccessPathComponent::AnonymousValue(_) => {
          runtime_error!(RuntimeErrorType::InvalidOperation, "anonymous values may only appear as the first component in an access path")
        }
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
  pub fn check_block(&mut self) -> RuntimeResult<()> {
    let mut is_printing = false;
    let mut is_repeater = false;

    let rng = self.rng_clone();
    
    // Check if there's an active block and try to iterate it
    let next_element = if let Some(state) = self.resolver.active_block_mut() {
      is_repeater = state.is_repeater();
      
      // Get the next element
      if let Some(element) = state.next_element(rng.as_ref()).into_runtime_result()? {
        // Figure out if the block is supposed to print anything
        is_printing = !state.flag().is_sink();
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
    // TODO: Consider moving BlockAction handler into Resolver
    if let Some(element) = next_element {  
      // Tell the calling frame to check the block status once the separator returns
      self.cur_frame_mut().push_intent_front(Intent::CheckBlock);

      match element {
        BlockAction::Element(elem_seq) => {
          // Determine if we should print anything, or just push the result to the stack
          if is_printing {
            self.cur_frame_mut().push_intent_front(Intent::PrintLast);
          }
          // Push the next element
          self.push_frame_flavored(
            Rc::clone(&elem_seq), 
            is_printing, 
            if is_repeater { 
              StackFrameFlavor::RepeaterElement 
            } else { 
              StackFrameFlavor::BlockElement
            }
          )?;
        },
        BlockAction::PipedElement { elem_func, pipe_func } => {
          // Determine if we should print anything, or just push the result to the stack
          if is_printing {
            self.cur_frame_mut().push_intent_front(Intent::PrintLast);
          }

          let flag = if is_printing {
            PrintFlag::Hint
          } else {
            PrintFlag::Sink
          };

          // Call the pipe function
          self.call_func(pipe_func, vec![RantValue::Function(elem_func)], flag, true)?;
        },
        BlockAction::Separator(separator) => {
          match separator {
            // If the separator is a function, call the function
            RantValue::Function(sep_func) => {
              self.push_val(RantValue::Function(sep_func))?;
              self.cur_frame_mut().push_intent_front(Intent::Call { 
                argc: 0, 
                flag: if is_printing { PrintFlag::Hint } else { PrintFlag::Sink }, 
                override_print: false 
              });
            },
            // If the separator is a block, resolve it
            RantValue::Block(sep_block) => {
              self.pre_push_block(&sep_block, sep_block.flag)?;
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
  pub fn pre_push_block(&mut self, block: &Rc<Block>, flag: PrintFlag) -> RuntimeResult<()> {
    if block.is_weighted {
      self.cur_frame_mut().push_intent_front(Intent::BuildWeightedBlock {
        block: Rc::clone(block),
        weights: Weights::new(block.elements.len()),
        pop_next_weight: false,
      });
    } else {
      self.push_block(block, None, flag)?;
    }
    Ok(())
  }

  /// Consumes attributes and pushes a block onto the resolver stack.
  #[inline]
  pub fn push_block(&mut self, block: &Block, weights: Option<Weights>, flag: PrintFlag) -> RuntimeResult<()> {
    // Push a new state onto the block stack
    self.resolver.push_block(block, weights, flag);

    // Check the block to make sure it actually does something.
    // If the block has some skip condition, it will automatically remove it, and this method will have no net effect.
    self.check_block()?;

    Ok(())
  }

  #[inline(always)]
  fn def_compval(&mut self, compval: RantValue) -> RuntimeResult<()> {
    self.call_stack.def_var_value(self.engine, COMPOSE_VALUE_NAME, AccessPathKind::Local, compval, true)
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
      self.val_stack.push(val);
      Ok(self.val_stack.len())
    } else {
      runtime_error!(RuntimeErrorType::StackOverflow, "value stack has overflowed");
    }
  }

  /// Removes the topmost value from the value stack and returns it.
  #[inline(always)]
  pub fn pop_val(&mut self) -> RuntimeResult<RantValue> {
    if let Some(val) = self.val_stack.pop() {
      Ok(val)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "value stack has underflowed");
    }
  }

  /// Removes the topmost frame from the call stack and returns it.
  #[inline(always)]
  pub fn pop_frame(&mut self) -> RuntimeResult<StackFrame> {
    runtime_trace!("pop_frame: {} -> {}", self.call_stack.len(), self.call_stack.len() - 1);
    if let Some(frame) = self.call_stack.pop_frame() {
      Ok(frame)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "call stack has underflowed");
    }
  }

  /// Pushes a frame onto the call stack without overflow checks.
  #[inline(always)]
  fn push_frame_unchecked(&mut self, callee: Rc<Sequence>, use_output: bool, flavor: StackFrameFlavor) {
    runtime_trace!("push_frame_unchecked");
    let frame = StackFrame::new(
      callee, 
      use_output, 
      self.call_stack.top().map(|last| last.output()).flatten()
    ).with_flavor(flavor);

    self.call_stack.push_frame(frame);
  }
  
  /// Pushes a frame onto the call stack.
  #[inline(always)]
  pub fn push_frame(&mut self, callee: Rc<Sequence>, use_output: bool) -> RuntimeResult<()> {
    runtime_trace!("push_frame");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let frame = StackFrame::new(
      callee,
      use_output,
      self.call_stack.top().map(|last| last.output()).flatten()
    );

    self.call_stack.push_frame(frame);
    Ok(())
  }

  /// Pushes an empty frame onto the call stack with a single `RuntimeCall` intent.
  pub fn push_native_call_frame(&mut self, callee: Box<dyn FnOnce(&mut VM) -> RuntimeResult<()>>, use_output: bool, flavor: StackFrameFlavor) -> RuntimeResult<()> {
    runtime_trace!("push_native_call_frame");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }

    let last_frame = self.call_stack.top().unwrap();

    let frame = StackFrame::new_native_call(
      callee,
      use_output,
      self.call_stack.top().map(|last| last.output()).flatten(),
      Rc::clone(last_frame.origin()),
      last_frame.debug_pos(),
      StackFrameFlavor::Original
    ).with_flavor(flavor);

    self.call_stack.push_frame(frame);
    Ok(())
  }

  /// Pushes a flavored frame onto the call stak.
  #[inline(always)]
  pub fn push_frame_flavored(&mut self, callee: Rc<Sequence>, use_output: bool, flavor: StackFrameFlavor) -> RuntimeResult<()> {
    runtime_trace!("push_frame_flavored");
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let frame = StackFrame::new(
      callee,
      use_output,
      self.call_stack.top().map(|last| last.output()).flatten()
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
          let mut old_frame = self.pop_frame()?;
          if let Some(output) = old_frame.render_output_value() {
            if i < block_depth {
              self.cur_frame_mut().write_value(output);
            } else {
              self.push_val(output)?;
            }
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
          let mut old_frame = self.pop_frame()?;

          // If a block state is associated with the popped frame, pop that too
          match old_frame.flavor() {
            StackFrameFlavor::RepeaterElement | StackFrameFlavor::BlockElement => {
              self.resolver_mut().pop_block();
            },
            _ => {}
          }

          // Handle output
          if let Some(output) = old_frame.render_output_value() {
            if i < block_depth {
              self.cur_frame_mut().write_value(output);
            } else {
              self.push_val(output)?;
            }
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
  pub fn cur_frame_mut(&mut self) -> &mut StackFrame {
    self.call_stack.top_mut().unwrap()
  }

  /// Safely attempts to get a mutable reference to the topmost frame on the call stack.
  #[inline(always)]
  pub fn any_cur_frame_mut(&mut self) -> Option<&mut StackFrame> {
    self.call_stack.top_mut()
  }

  /// Safely attempts to get a mutable reference to the frame `depth` frames below the top of the call stack.
  #[inline(always)]
  pub fn parent_frame_mut(&mut self, depth: usize) -> Option<&mut StackFrame> {
    self.call_stack.parent_mut(depth)
  }

  /// Gets a reference to the topmost frame on the call stack.
  #[inline(always)]
  pub fn cur_frame(&self) -> &StackFrame {
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
    &self.engine
  }

  /// Gets a mutable reference to the Rant context that created the VM.
  #[inline(always)]
  pub fn context_mut(&mut self) -> &mut Rant {
    &mut self.engine
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
  pub fn push_unwind_state(&mut self, handler: Option<RantFunctionRef>) {
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

pub(crate) trait IntoRuntimeResult<T> {
  fn into_runtime_result(self) -> RuntimeResult<T>;
}

/// A runtime error raised by a Rant program.
#[derive(Debug)]
pub struct RuntimeError {
  /// The type of runtime error.
  pub error_type: RuntimeErrorType,
  /// A description of what went wrong.
  pub description: String,
  /// A stack trace describing the location of the error.
  pub stack_trace: Option<String>,
}

impl Error for RuntimeError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match &self.error_type {
      RuntimeErrorType::IndexError(err) => Some(err),
      RuntimeErrorType::KeyError(err) => Some(err),
      RuntimeErrorType::ValueError(err) => Some(err),
      _ => None,
    }
  }

  fn cause(&self) -> Option<&dyn Error> {
    self.source()
  }
}

impl Display for RuntimeError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "[{}] {}", self.error_type, self.description)
  }
}

/// Provides general categories of runtime errors encountered in Rant.
#[derive(Debug)]
pub enum RuntimeErrorType {
  /// Stack overflow
  StackOverflow,
  /// Stack underflow
  StackUnderflow,
  /// Variable access error, such as attempting to access a nonexistent variable or write to a constant
  InvalidAccess,
  /// Operation is not valid for the current program state
  InvalidOperation,
  /// Internal VM error, usually indicating a bug or corrupted data
  InternalError,
  /// Too few/many arguments were passed to a function
  ArgumentMismatch,
  /// Invalid argument passed to function
  ArgumentError,
  /// Tried to invoke a non-function
  CannotInvokeValue,
  /// Assertion failed
  AssertError,
  /// Error occurred due to unexpected value type
  TypeError,
  /// Error occurred when creating value
  ValueError(ValueError),
  /// Error occurred while indexing value
  IndexError(IndexError),
  /// Error occurred while keying value
  KeyError(KeyError),
  /// Error occurred while slicing value
  SliceError(SliceError),
  /// Error occurred while iterating selector
  SelectorError(SelectorError),
  /// Error occurred while trying to load a module
  ModuleLoadError(ModuleLoadError),
  /// Error manually triggered by program
  UserError,
  /// Error during control flow operation (e.g. return or break)
  ControlFlowError,
}

impl Display for RuntimeErrorType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", match self {
      RuntimeErrorType::StackOverflow => "stack overflow",
      RuntimeErrorType::StackUnderflow => "stack underflow",
      RuntimeErrorType::InvalidAccess => "invalid access",
      RuntimeErrorType::InvalidOperation => "invalid operation",
      RuntimeErrorType::InternalError => "external error",
      RuntimeErrorType::ArgumentMismatch => "argument mismatch",
      RuntimeErrorType::ArgumentError => "argument error",
      RuntimeErrorType::CannotInvokeValue => "cannot invoke value",
      RuntimeErrorType::UserError => "user error",
      RuntimeErrorType::AssertError => "assertion error",
      RuntimeErrorType::TypeError => "type error",
      RuntimeErrorType::ValueError(_) => "value error",
      RuntimeErrorType::IndexError(_) => "index error",
      RuntimeErrorType::KeyError(_) => "key error",
      RuntimeErrorType::SliceError(_) => "slice error",
      RuntimeErrorType::SelectorError(_) => "selector error",
      RuntimeErrorType::ModuleLoadError(_) => "module load error",
      RuntimeErrorType::ControlFlowError => "control flow error",
    })
  }
}