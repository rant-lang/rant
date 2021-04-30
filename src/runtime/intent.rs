use std::rc::Rc;

use crate::lang::*;
use crate::{RantFunctionRef, RantList, RantMap, RantValue};

use super::{RuntimeResult, SetterValueSource, VM, VarWriteMode, resolver::Weights};

// Intents are actions queued on a stack frame that are performed before the frame runs.
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
  Invoke { 
    arg_exprs: Rc<Vec<ArgumentExpr>>, 
    arg_eval_count: usize,
    flag: PrintFlag, 
    is_temporal: bool, 
  },
  /// Invoke a single function in a piped function call chain.
  InvokePipeStep { 
    /// All steps in the entire piped function call
    steps: Rc<Vec<FunctionCall>>,
    /// The current step being executed
    step_index: usize, 
    /// Current state of the intent.
    state: InvokePipeStepState,
    /// The pipe value from the last step
    pipeval: Option<RantValue>,
    /// The print flag to use.
    flag: PrintFlag,
  },
  /// Evaluates each sequence in `default_arg_exprs` in order and assigns their results to local constants with their associated `Identifier`.
  CreateDefaultArgs { context: RantFunctionRef, default_arg_exprs: Vec<(Rc<Sequence>, usize)>, eval_index: usize, },
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
  pub(crate) fn name(&self) -> &'static str {
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
      Intent::InvokePipeStep { .. } => "invoke_pipe_step",
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
      Intent::CreateDefaultArgs { .. } => "create_default_args",
    }
  }
}

/// States for the `InvokePipeStep` intent.
#[derive(Debug)]
pub enum InvokePipeStepState {
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