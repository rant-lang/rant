use crate::*;
use crate::lang::*;
use std::{rc::Rc, cell::RefCell, ops::Deref, error::Error, fmt::Display};
use resolver::{SelectorError, Resolver, BlockAction};
use smallvec::SmallVec;
pub use stack::*;
pub use output::*;

pub(crate) mod resolver;
mod output;
mod stack;

pub type RuntimeResult<T> = Result<T, RuntimeError>;

pub const MAX_STACK_SIZE: usize = 20000;
pub(crate) const CALL_STACK_INLINE_COUNT: usize = 4;
pub(crate) const VALUE_STACK_INLINE_COUNT: usize = 4;

pub struct VM<'rant> {
  rng: Rc<RantRng>,
  engine: &'rant mut Rant,
  program: &'rant RantProgram,
  val_stack: SmallVec<[RantValue; VALUE_STACK_INLINE_COUNT]>,
  call_stack: CallStack,
  resolver: Resolver
}

impl<'rant> VM<'rant> {
  #[inline]
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

/// Returns a runtime error from the current execution context with the specified error type and optional description.
macro_rules! runtime_error {
  ($err_type:expr) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: "".to_owned(),
      stack_trace: None,
    })
  };
  ($err_type:expr, $desc:expr) => {
    return Err(RuntimeError {
      error_type: $err_type,
      description: $desc.to_string(),
      stack_trace: None,
    })
  };
}

/// RSTs can assign intents to the current stack frame
/// to override its usual behavior next time it's active.
#[derive(Debug)]
pub enum Intent {
  /// Take the pending output from last frame and print it.
  PrintValue,
  /// Check if the active block is finished and either continue the block or pop the state from the stack
  CheckBlock,
  /// Pop a value off the stack and assign it to an existing variable.
  SetVar { vname: Identifier },
  /// Pop a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `GetValue`.
  BuildDynamicGetter { path: Rc<VarAccessPath>, dynamic_key_count: usize, pending_exprs: Vec<Rc<Sequence>>, override_print: bool },
  /// Pop `dynamic_key_count` values off the stack and use them for expression fields in a getter.
  GetValue { path: Rc<VarAccessPath>, dynamic_key_count: usize, override_print: bool },
  /// Pop a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `SetValue`.
  BuildDynamicSetter { path: Rc<VarAccessPath>, auto_def: bool, expr_count: usize, pending_exprs: Vec<Rc<Sequence>>, val_source: SetterValueSource },
  /// Pop `expr_count` values off the stack and use them for expression fields in a setter.
  SetValue { path: Rc<VarAccessPath>, auto_def: bool, expr_count: usize },
  /// Evaluate `arg_exprs` in order, then pop the argument values off the stack, pop a function off the stack, and pass the arguments to the function.
  Invoke { arg_exprs: Rc<Vec<Rc<Sequence>>>, eval_count: usize, flag: PrintFlag },
  /// Pop `argc` args off the stack, then pop a function off the stack and call it with the args.
  Call { argc: usize, flag: PrintFlag },
  /// Pop value from stack and add it to a list. If `index` is out of range, print the list.
  BuildList { init: Rc<Vec<Rc<Sequence>>>, index: usize, list: RantList },
  /// Pop value and optional key from stack and add them to a map. If `pair_index` is out of range, print the map.
  BuildMap { init: Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>, pair_index: usize, map: RantMap },
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

impl<'rant> VM<'rant> {
  /// Runs the program.
  #[inline]
  pub fn run(&mut self) -> RuntimeResult<String> {
    let mut result = self.run_inner();
    // On error, generate stack trace
    if let Err(err) = result.as_mut() {
      err.stack_trace = Some(self.stack_trace());
    }
    result
  }

  
  #[inline]
  fn run_inner(&mut self) -> RuntimeResult<String> {
    // Push the program's root sequence onto the call stack
    // This doesn't need an overflow check because it will *always* succeed
    self.push_frame_unchecked(self.program.root.clone(), true, None);
    
    // Run whatever is on the top of the call stack
    'from_the_top: 
    while !self.is_stack_empty() {
      
      // Read frame's current intents and handle them before running the sequence
      while let Some(intent) = self.cur_frame_mut().take_intent() {
        match intent {
          Intent::PrintValue => {
            let val = self.pop_val()?;
            self.cur_frame_mut().write_value(val);
          },
          Intent::CheckBlock => {
            self.check_block()?;
          },
          Intent::SetVar { vname } => {
            let val = self.pop_val()?;
            self.set_local(vname.as_str(), val)?;
          },
          Intent::DefVar { vname } => {
            let val = self.pop_val()?;
            self.def_local(vname.as_str(), val)?;
          },
          Intent::BuildDynamicGetter { path, dynamic_key_count, mut pending_exprs, override_print } => {
            if let Some(key_expr) = pending_exprs.pop() {
              // Set next intent based on remaining expressions in getter
              if pending_exprs.is_empty() {
                self.cur_frame_mut().push_intent_front(Intent::GetValue { path, dynamic_key_count, override_print });
              } else {
                self.cur_frame_mut().push_intent_front(Intent::BuildDynamicGetter { path, dynamic_key_count, pending_exprs, override_print });
              }
              self.push_frame(Rc::clone(&key_expr), true, None)?;
            } else {
              self.cur_frame_mut().push_intent_front(Intent::GetValue { path, dynamic_key_count, override_print });
            }
            continue 'from_the_top;
          },
          Intent::GetValue { path, dynamic_key_count, override_print } => {
            self.get_value(path, dynamic_key_count, override_print)?;
          },
          Intent::Invoke { arg_exprs, eval_count, flag } => {
            // First, evaluate all arguments
            if eval_count < arg_exprs.len() {
              let arg_expr = Rc::clone(arg_exprs.get(arg_exprs.len() - eval_count - 1).unwrap());
              self.cur_frame_mut().push_intent_front(Intent::Invoke { arg_exprs, eval_count: eval_count + 1, flag });
              self.push_frame(arg_expr, true, None)?;
              continue 'from_the_top;
            } else {
              // Pop the evaluated args off the stack
              let mut args = vec![];
              for _ in 0..arg_exprs.len() {
                args.push(self.pop_val()?);
              }

              // Pop the function and make sure it's callable
              let func = match self.pop_val()? {
                RantValue::Function(func) => {
                  func
                },
                other => runtime_error!(RuntimeErrorType::CannotInvokeValue, format!("cannot invoke '{}' value", other.type_name()))
              };

              // Call the function
              self.call_func(func, args, flag)?;
              continue 'from_the_top;
            }
          },
          Intent::Call { argc, flag } => {
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
              other => runtime_error!(RuntimeErrorType::CannotInvokeValue, format!("cannot invoke '{}' value", other.type_name()))
            };

            // Call the function
            self.call_func(func, args, flag)?;
            continue 'from_the_top;
          },
          Intent::BuildDynamicSetter { path, auto_def, expr_count, mut pending_exprs, val_source } => {
            if let Some(key_expr) = pending_exprs.pop() {
              // Set next intent based on remaining expressions in setter
              if pending_exprs.is_empty() {
                // Set value once this frame is active again
                self.cur_frame_mut().push_intent_front(Intent::SetValue { path, auto_def, expr_count });
                self.push_frame(Rc::clone(&key_expr), true, None)?;
              } else {
                self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter { path, auto_def, expr_count, pending_exprs, val_source });
                self.push_frame(Rc::clone(&key_expr), true, None)?;
                continue 'from_the_top;
              }
              
            } else {
              self.cur_frame_mut().push_intent_front(Intent::SetValue { path, auto_def, expr_count });
            }
  
            // Prepare setter value
            match val_source {
              SetterValueSource::FromExpression(expr) => {
                self.push_frame(Rc::clone(&expr), true, None)?;
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
              self.cur_frame_mut().push_intent_front(Intent::BuildList { init: Rc::clone(&init), index: index + 1, list });
              let val_expr = &init[index];
              self.push_frame(Rc::clone(val_expr), true, None)?;
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
              self.cur_frame_mut().push_intent_front(Intent::BuildMap { init: Rc::clone(&init), pair_index: pair_index + 1, map });
              let (key_expr, val_expr) = &init[pair_index];
              if let MapKeyExpr::Dynamic(key_expr) = key_expr {
                // Push dynamic key expression onto call stack
                self.push_frame(Rc::clone(&key_expr), true, None)?;
              }
              // Push value expression onto call stack
              self.push_frame(Rc::clone(val_expr), true, None)?;
              continue 'from_the_top;
            }
          },
        }
      }
      
      // Run frame's sequence elements in order
      while let Some(rst) = &self.cur_frame_mut().seq_next() {
        match Rc::deref(rst) {
          RST::DebugInfoUpdateOuter(info) => {
            self.cur_frame_mut().set_debug_info(info);
          },
          RST::Fragment(frag) => self.cur_frame_mut().write_frag(frag),
          RST::Whitespace(ws) => self.cur_frame_mut().write_ws(ws),
          RST::Integer(n) => self.cur_frame_mut().write_value(RantValue::Integer(*n)),
          RST::Float(n) => self.cur_frame_mut().write_value(RantValue::Float(*n)),
          RST::EmptyVal => self.cur_frame_mut().write_value(RantValue::Empty),
          RST::Boolean(b) => self.cur_frame_mut().write_value(RantValue::Boolean(*b)),
          RST::ListInit(elements) => {
            self.cur_frame_mut().push_intent_front(Intent::BuildList { init: Rc::clone(elements), index: 0, list: RantList::new() });
            continue 'from_the_top;
          },
          RST::MapInit(elements) => {
            self.cur_frame_mut().push_intent_front(Intent::BuildMap { init: Rc::clone(elements), pair_index: 0, map: RantMap::new() });
            continue 'from_the_top;
          },
          RST::Block(block) => {
            self.push_block(block, block.flag)?;
            continue 'from_the_top;
          },
          RST::VarDef(vname, val_expr) => {
            if let Some(val_expr) = val_expr {
              // If a value is present, it needs to be evaluated first
              self.cur_frame_mut().push_intent_front(Intent::DefVar { vname: vname.clone() });
              self.push_frame(Rc::clone(val_expr), true, None)?;
              continue 'from_the_top;
            } else {
              // If there's no assignment, just set it to <>
              self.def_local(vname.as_str(), RantValue::Empty)?;
            }
          },
          RST::VarGet(path) => {
            // Get list of dynamic keys in path
            let dynamic_keys = path.dynamic_keys();

            if dynamic_keys.is_empty() {
              // Getter is static, so run it directly
              self.cur_frame_mut().push_intent_front(Intent::GetValue { 
                path: Rc::clone(path), 
                dynamic_key_count: 0, 
                override_print: false 
              });
            } else {
              // Build dynamic keys before running getter
              self.cur_frame_mut().push_intent_front(Intent::BuildDynamicGetter {
                dynamic_key_count: dynamic_keys.len(),
                path: Rc::clone(path),
                pending_exprs: dynamic_keys,
                override_print: false,
              });
            }
            continue 'from_the_top;
          },
          RST::VarSet(path, val_expr) => {
            // Get list of dynamic keys in path
            let exprs = path.dynamic_keys();

            if exprs.is_empty() {
              // Setter is static, so run it directly
              self.cur_frame_mut().push_intent_front(Intent::SetValue { path: Rc::clone(&path), auto_def: false, expr_count: 0 });
              self.push_frame(Rc::clone(&val_expr), true, None)?;
            } else {
              // Build dynamic keys before running setter
              self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter {
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
              vararg_start_index: params.iter()
                .enumerate()
                .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
                .unwrap_or_else(|| params.len()),
            }));

            let dynamic_keys = id.dynamic_keys();

            self.cur_frame_mut().push_intent_front(Intent::BuildDynamicSetter {
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
              vararg_start_index: params.iter()
                .enumerate()
                .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
                .unwrap_or_else(|| params.len()),
            }));

            self.cur_frame_mut().write_value(func);
          },
          RST::AnonFuncCall(afcall) => {
            let AnonFunctionCall {
              expr,
              args,
              flag,
            } = afcall;

            // Evaluate arguments after function is evaluated
            self.cur_frame_mut().push_intent_front(Intent::Invoke {
              arg_exprs: Rc::clone(args),
              eval_count: 0,
              flag: *flag
            });

            // Push function expression onto stack
            self.push_frame(Rc::clone(expr), true, None)?;

            continue 'from_the_top;
          },
          RST::FuncCall(fcall) => {
            let FunctionCall {
              id,
              arguments,
              flag,
            } = fcall;

            let dynamic_keys = id.dynamic_keys();

            // Run the getter to retrieve the function we're calling first...
            self.cur_frame_mut().push_intent_front(if dynamic_keys.is_empty() {
              // Getter is static, so run it directly
              Intent::GetValue { 
                path: Rc::clone(id), 
                dynamic_key_count: 0, 
                override_print: true 
              }
            } else {
              // Build dynamic keys before running getter
              Intent::BuildDynamicGetter {
                dynamic_key_count: dynamic_keys.len(),
                path: Rc::clone(id),
                pending_exprs: dynamic_keys,
                override_print: true,
              }
            });

            // Queue up the function call next
            self.cur_frame_mut().push_intent_back(Intent::Invoke {
              eval_count: 0,
              arg_exprs: Rc::clone(arguments),
              flag: *flag,
            });
            continue 'from_the_top;
          },
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

  #[inline]
  fn call_func(&mut self, func: RantFunctionRef, mut args: Vec<RantValue>, flag: PrintFlag) -> RuntimeResult<()> {
    let argc = args.len();

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

    // Call the function
    match &func.body {
      RantFunctionInterface::Foreign(foreign_func) => {
        // TODO: Honor sinks on native function calls
        foreign_func(self, args)?;
      },
      RantFunctionInterface::User(user_func) => {
        // Convert the args into a locals map
        let mut func_locals = RantMap::new();
        let mut args = args.drain(..);
        for param in func.params.iter() {
          func_locals.raw_set(param.name.as_str(), args.next().unwrap_or(RantValue::Empty));
        }
        let is_printing = !flag.is_sink();

        // Tell frame to print output if it's available
        if is_printing {
          self.cur_frame_mut().push_intent_front(Intent::PrintValue);
        }
        // Push the function onto the call stack
        self.push_frame(Rc::clone(user_func), is_printing, Some(func_locals))?;
      },
    }
    Ok(())
  }

  #[inline]
  fn set_value(&mut self, path: Rc<VarAccessPath>, auto_def: bool, dynamic_key_count: usize) -> RuntimeResult<()> {
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
        (Some(val), SetterKey::Index(index)) => Some(val.index_get(*index).into_runtime_result()?),
        (Some(val), SetterKey::KeyRef(key)) => Some(val.key_get(key).into_runtime_result()?),
        (Some(val), SetterKey::KeyString(key)) => Some(val.key_get(key.as_str()).into_runtime_result()?),
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
      (Some(target), SetterKey::Index(index)) => target.index_set(*index, setter_value).into_runtime_result()?,
      (Some(target), SetterKey::KeyRef(key)) => target.key_set(key, setter_value).into_runtime_result()?,
      (Some(target), SetterKey::KeyString(key)) => target.key_set(key.as_str(), setter_value).into_runtime_result()?,
      _ => unreachable!()
    }

    Ok(())
  }

  #[inline]
  fn get_value(&mut self, path: Rc<VarAccessPath>, dynamic_key_count: usize, override_print: bool) -> RuntimeResult<()> {
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
          getter_value = match getter_value.key_get(key.as_str()) {
            Ok(val) => val,
            Err(err) => runtime_error!(RuntimeErrorType::KeyError(err))
          };
        },
        // Index
        VarAccessComponent::Index(index) => {
          getter_value = match getter_value.index_get(*index) {
            Ok(val) => val,
            Err(err) => runtime_error!(RuntimeErrorType::IndexError(err))
          }
        },
        // Dynamic key
        VarAccessComponent::Expression(_) => {
          let key = dynamic_keys.next().unwrap();
          match key {
            RantValue::Integer(index) => {
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
        }
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
  pub(crate) fn check_block(&mut self) -> RuntimeResult<()> {
    let mut is_printing = false;
    
    // Check if there's an active block and try to iterate it
    let next_element = if let Some(state) = self.resolver.active_block_mut() {
      // Get the next element
      if let Some(element) = state.next_element().into_runtime_result()? {
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
          // Combine with no_print to determine if we *should* print anything, or just push the result to the stack
          if is_printing {
            self.cur_frame_mut().push_intent_front(Intent::PrintValue);
          }
          // Push the next element
          self.push_frame(Rc::clone(&elem_seq), is_printing, Default::default())?;
        },
        BlockAction::Separator(separator) => {
          match separator {
            // If the separator is a function, call the function
            RantValue::Function(sep_func) => {
              self.push_val(RantValue::Function(sep_func))?;
              self.cur_frame_mut().push_intent_front(Intent::Call { argc: 0, flag: if is_printing { PrintFlag::Hint } else { PrintFlag::Sink } });
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

  #[inline(always)]
  pub(crate) fn push_block(&mut self, block: &Block, flag: PrintFlag) -> RuntimeResult<()> {
    // Push a new state onto the block stack
    self.resolver.push_block(block, flag);

    // Check the block to make sure it actually does something.
    // If the block has some skip condition, it will automatically remove it, and this method will have no net effect.
    self.check_block()?;

    Ok(())
  }

  #[inline(always)]
  pub(crate) fn set_local(&mut self, key: &str, val: RantValue) -> RuntimeResult<()> {
    self.call_stack.set_local(self.engine, key, val)
  }

  #[inline(always)]
  pub(crate) fn get_local(&self, key: &str) -> RuntimeResult<RantValue> {
    self.call_stack.get_local(self.engine, key)
  }

  #[inline(always)]
  pub(crate) fn def_local(&mut self, key: &str, val: RantValue) -> RuntimeResult<()> {
    self.call_stack.def_local(self.engine, key, val)
  }
  
  #[inline(always)]
  fn is_stack_empty(&self) -> bool {
    self.call_stack.is_empty()
  }

  #[inline(always)]
  pub(crate) fn push_val(&mut self, val: RantValue) -> RuntimeResult<usize> {
    if self.val_stack.len() < MAX_STACK_SIZE {
      self.val_stack.push(val);
      Ok(self.val_stack.len())
    } else {
      runtime_error!(RuntimeErrorType::StackOverflow, "value stack has overflowed");
    }
  }

  #[inline(always)]
  pub(crate) fn pop_val(&mut self) -> RuntimeResult<RantValue> {
    if let Some(val) = self.val_stack.pop() {
      Ok(val)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "value stack has underflowed");
    }
  }

  #[inline(always)]
  pub(crate) fn pop_frame(&mut self) -> RuntimeResult<StackFrame> {
    if let Some(frame) = self.call_stack.pop() {
      Ok(frame)
    } else {
      runtime_error!(RuntimeErrorType::StackUnderflow, "call stack has underflowed");
    }
  }

  #[inline(always)]
  fn push_frame_unchecked(&mut self, callee: Rc<Sequence>, use_output: bool, locals: Option<RantMap>) {
    let mut frame = StackFrame::new(callee, locals.unwrap_or_default(), use_output);

    if self.engine.debug_mode {
      if let Some(name) = self.program.name() {
        frame.set_debug_info(&DebugInfo::SourceName(RantString::from(name)));
      }
    }

    self.call_stack.push(frame);
  }
  
  #[inline(always)]
  pub(crate) fn push_frame(&mut self, callee: Rc<Sequence>, use_output: bool, locals: Option<RantMap>) -> RuntimeResult<()> {
    // Check if this push would overflow the stack
    if self.call_stack.len() >= MAX_STACK_SIZE {
      runtime_error!(RuntimeErrorType::StackOverflow, "call stack has overflowed");
    }
    
    let mut frame = StackFrame::new(callee, locals.unwrap_or_default(), use_output);

    if self.engine.debug_mode {
      if let Some(name) = self.program.name() {
        frame.set_debug_info(&DebugInfo::SourceName(RantString::from(name)));
      }
    }

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

  #[inline(always)]
  pub fn rng(&self) -> &RantRng {
    self.rng.as_ref()
  }

  #[inline(always)]
  pub fn resolver(&self) -> &Resolver {
    &self.resolver
  }

  #[inline(always)]
  pub fn resolver_mut(&mut self) -> &mut Resolver {
    &mut self.resolver
  }

  pub fn stack_trace(&self) -> String {
    let mut trace = String::new();
    for frame in self.call_stack.iter().rev() {
      trace.push_str(format!("-> {}\n", frame).as_str());
    }
    trace
  }
}

pub(crate) trait IntoRuntimeResult<T> {
  fn into_runtime_result(self) -> RuntimeResult<T>;
}

// TODO: Add stack trace to runtime errors
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
    write!(f, "{}", self.description)
  }
}

/// Provides general categories of runtime errors encountered in Rant.
#[derive(Debug)]
pub enum RuntimeErrorType {
  /// General error type; check message attached to error
  GeneralError,
  /// Stack overflow
  StackOverflow,
  /// Stack underflow
  StackUnderflow,
  /// Variable access error, such as attempting to access a nonexistent variable
  InvalidAccess,
  /// Error in function outside of Rant
  ExternalError,
  /// Too few/many arguments were passed to a function
  ArgumentMismatch,
  /// Invalid argument passed to function
  ArgumentError,
  /// Tried to invoke a non-function
  CannotInvokeValue,
  /// Error occurred when creating value
  ValueError(ValueError),
  /// Error occurred while indexing value
  IndexError(IndexError),
  /// Error occurred while keying value
  KeyError(KeyError),
  /// Error occurred while iterating selector
  SelectorError(SelectorError),
}