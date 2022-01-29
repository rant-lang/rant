use std::rc::Rc;

use crate::{lang::*, InternalString};
use crate::{RantFunctionHandle, RantList, RantMap, RantValue};

use super::{RuntimeResult, VM, resolver::Weights};

/// Actions that can be queued on a stack frame that are performed before the frame runs.
///
/// ## "Call" or "Invoke"?
/// In the context of Rant runtime intents, "calling" and "invoking" have specific meanings:
/// * "Invoke" means that argument expressions potentially need to be evaluated before the call can proceed;
/// * "Call" means that all argument values are already known (either in the intent or on the value stack).
pub enum Intent {
  /// Pops a value off the value stack and print it to the current frame's output.
  PrintLast,
  /// Pops a value off the value stack and returns it from the current function.
  ReturnLast,
  /// Pops a value off the value stack and continues to the next repeater iteration with it.
  ContinueLast,
  /// Pops a value off the value stack and breaks from the current repeater with it.
  BreakLast,
  /// Pops a map off the stack and loads it as a module with the specified name.
  ImportLastAsModule { module_name: String, descope: usize },
  /// Check if the current block is finished and either continue the block or pop the state from the stack
  TickCurrentBlock,
  /// Pops a value off the stack and assign it to an existing variable.
  SetVar { vname: Identifier, access_kind: VarAccessMode, },
  /// Pops a value off the stack and assign it to a new variable.
  DefVar { vname: Identifier, access_kind: VarAccessMode, is_const: bool },
  /// Pops a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `GetValue`.
  BuildDynamicGetter { 
    path: Rc<AccessPath>, 
    dynamic_key_count: usize, 
    pending_exprs: Vec<Rc<Sequence>>, 
    override_print: bool, 
    prefer_function: bool,
    fallback: Option<Rc<Sequence>>,
  },
  /// Pops dynamic key values off the stack and runs a getter. If the getter fails, evaluates the fallback.
  GetValue { 
    path: Rc<AccessPath>, 
    dynamic_key_count: usize, 
    override_print: bool, 
    prefer_function: bool,
    fallback: Option<Rc<Sequence>>, 
  },
  /// Used to evaluate a setter path with dynamic keys.
  /// 
  /// Pops a block from `pending_exprs` and evaluate it. If there are no expressions left, switch intent to `SetValue`.
  BuildDynamicSetter { 
    path: Rc<AccessPath>,
    write_mode: VarWriteMode, 
    expr_count: usize, 
    pending_exprs: Vec<Rc<Sequence>>, 
    val_source: SetterValueSource,
  },
  /// Pops `expr_count` values off the stack and uses them for expression fields in a setter.
  SetValue { 
    path: Rc<AccessPath>,
    write_mode: VarWriteMode,
    expr_count: usize 
  },
  /// Evaluates `arg_exprs` in order, then pops the argument values off the stack, pops a function off the stack, and passes the arguments to the function.
  Invoke { 
    arg_exprs: Rc<Vec<ArgumentExpr>>, 
    arg_eval_count: usize,
    is_temporal: bool, 
  },
  /// Invokes a single function in a piped function call chain.
  InvokePipeStep { 
    /// All steps in the entire piped function call
    steps: Rc<Vec<FunctionCall>>,
    /// The current step being executed
    step_index: usize, 
    /// Current state of the intent.
    state: InvokePipeStepState,
    /// The pipe value from the last step
    pipeval: Option<RantValue>,
    /// Optional assignment pipe
    assignment_pipe: Option<Rc<AssignmentPipeTarget>>,
  },
  /// Evaluates each sequence in `default_arg_exprs` in order and assigns their results to local constants with their associated `Identifier`.
  CreateDefaultArgs { context: RantFunctionHandle, default_arg_exprs: Vec<(Rc<Sequence>, usize)>, eval_index: usize, },
  /// Pops `argc` args off the stack, then pops a function off the stack and calls it with the args.
  Call { argc: usize, override_print: bool },
  /// Calls a sequence without an inner variable scope, then pushes its output to the value stack.
  CallOperand { sequence: Rc<Sequence> },
  /// Calls a function for every variant of a temporal argument set and increments the provided temporal state.
  CallTemporal { func: RantFunctionHandle, args: Rc<Vec<RantValue>>, temporal_state: TemporalSpreadState, },
  /// Pops value from stack and adds it to a list. If `index` is out of range, prints the list.
  BuildList { init: Rc<Vec<Rc<Sequence>>>, index: usize, list: RantList },
  /// Pops a value from the stack and adds it to `items`. If `index` is out of range, produces a tuple from `items` and prints it.
  BuildTuple { init: Rc<Vec<Rc<Sequence>>>, index: usize, items: Vec<RantValue> },
  /// Pops a value and optional key from stack and adds them to `map`. If `pair_index` is out of range, prints `map`.
  BuildMap { init: Rc<Vec<(MapKeyExpr, Rc<Sequence>)>>, pair_index: usize, map: RantMap },
  /// Evaluates expressions in `weights` and then runs the block with the computed element weights.
  BuildWeightedBlock { block: Rc<Block>, weights: Weights, pop_next_weight: bool, },
  /// Calls a function that accepts a mutable reference to the current runtime. Optionally interrupts the intent loop to force another tick.
  RuntimeCall { function: Box<dyn FnOnce(&mut VM) -> RuntimeResult<()>>, interrupt: bool },
  /// Drops all unwind states that are no longer within the call stack.
  DropStaleUnwinds,
  /// Pops two values (RHS, LHS), performs addition, and pushes the result.
  Add,
  /// Pops two values (RHS, LHS), performs subtraction, and pushes the result.
  Subtract,
  /// Pops two values (RHS, LHS), performs multiplication, and pushes the result.
  Multiply,
  /// Pops two values (RHS, LHS), performs division, and pushes the result.
  Divide,
  /// Pops two values (RHS, LHS), performs modulo, and pushes the result.
  Modulo,
  /// Pops two values (RHS, LHS), performs exponentiation, and pushes the result.
  Power,
  /// Pops a value, performs logical NOT, and pushes the result.
  LogicNot,
  /// Pops a value, performs negation, and pushes the result.
  Negate,
  /// Pops a value off the value stack and compares its truthiness against `on_truthiness`, and re-pushes the value.
  /// If they match, do nothing and continue.
  /// If they don't match, calls `gen_op_intent` and pushes the returned intent onto the current frame, then evaluates `rhs` using the `CallOperand` intent.
  LogicShortCircuit { on_truthiness: bool, short_circuit_result: LogicShortCircuitHandling, gen_op_intent: Box<dyn FnOnce() -> Intent>, rhs: Rc<Sequence> },
  /// Pops two values, performs logical AND, and pushes the result.
  LogicAnd,
  /// Pops two values, performs logical OR, and pushes the result.
  LogicOr,
  /// Pops two values (RHS, LHS), performs logical NAND, and pushes the result.
  LogicNand,
  /// Pops two values (RHS, LHS), performs logical NOR, and pushes the result.
  LogicNor,
  /// Pops two values (RHS, LHS), performs logical XOR, and pushes the result.
  LogicXor,
  /// Pops two values (RHS, LHS), calculates `LHS == RHS`, and pushes the result.
  Equals,
  /// Pops two values (RHS, LHS), calculates `LHS != RHS`, and pushes the result.
  NotEquals,
  /// Pops two values (RHS, LHS), calculates `LHS > RHS`, and pushes the result. 
  Greater,
  /// Pops two values (RHS, LHS), calculates `LHS >= RHS`, and pushes the result.
  GreaterOrEqual,
  /// Pops two values (RHS, LHS), calculates `LHS < RHS`, and pushes the result.
  Less,
  /// Pops two values (RHS, LHS), calculates `LHS <= RHS`, and pushes the result.
  LessOrEqual,
  /// If `index` is nonzero, pops a value off the value stack and checks its truthiness. 
  /// If it's falsy, pushes the next condition (or fallback) to the call stack and re-pushes this intent with `index = index + 1`.
  /// If it's truthy, pushes it to the value stack and exits. 
  CheckCondition { conditions: Rc<Vec<(Rc<Sequence>, Rc<Block>)>>, fallback: Option<Rc<Block>>, index: usize },
}

impl Intent {
  pub(crate) fn name(&self) -> &'static str {
    match self {
      Self::PrintLast => "print",
      Self::TickCurrentBlock => "check_block",
      Self::SetVar { .. } => "set_var",
      Self::DefVar { .. } => "def_var",
      Self::BuildDynamicGetter { .. } => "build_dyn_getter",
      Self::GetValue { .. } => "get_value",
      Self::BuildDynamicSetter { .. } => "build_dyn_setter",
      Self::SetValue { .. } => "set_value",
      Self::Invoke { .. } => "invoke",
      Self::InvokePipeStep { .. } => "invoke_pipe_step",
      Self::Call { .. } => "call",
      Self::CallOperand { .. } => "call_operand",
      Self::CallTemporal { .. } => "call_temporal",
      Self::BuildList { .. } => "build_list",
      Self::BuildTuple { .. } => "build_tuple",
      Self::BuildMap { .. } => "build_map",
      Self::ImportLastAsModule { .. } => "load_module",
      Self::RuntimeCall { .. } => "runtime_call",
      Self::DropStaleUnwinds => "drop_stale_unwinds",
      Self::ReturnLast => "return_last",
      Self::ContinueLast => "continue_last",
      Self::BreakLast => "break_last",
      Self::BuildWeightedBlock { .. } => "build_weighted_block",
      Self::CreateDefaultArgs { .. } => "create_default_args",
      Self::Add => "add",
      Self::Subtract => "subtract",
      Self::Multiply => "multiply",
      Self::Divide => "divide",
      Self::Modulo => "modulo",
      Self::Power => "power",
      Self::Negate => "negate",
      Self::LogicShortCircuit { .. } => "logic_short_circuit",
      Self::LogicNot => "not",
      Self::LogicAnd => "and",
      Self::LogicOr => "or",
      Self::LogicNand => "nand",
      Self::LogicNor => "nor",
      Self::LogicXor => "xor",
      Self::Equals => "equals",
      Self::NotEquals => "not_equals",
      Self::Less => "less",
      Self::LessOrEqual => "less_or_equal",
      Self::Greater => "greater",
      Self::GreaterOrEqual => "greater_or_equal",
      Self::CheckCondition { .. } => "conditional",
    }
  }
}

/// Defines short-circuiting behaviors for logic operators.
#[derive(Debug, Copy, Clone)]
pub enum LogicShortCircuitHandling {
  /// Pass through the LHS untouched.
  Passthrough,
  /// Use the specified boolean value as the LHS.
  OverrideWith(bool),
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
    step_function: RantFunctionHandle,
    temporal_state: TemporalSpreadState,
    args: Vec<RantValue>,
  },
  /// Step function is ready to call.
  ///
  /// Transitions to `PostCall`.
  PreCall { 
    step_function: RantFunctionHandle,
    args: Vec<RantValue>,
  },
  /// Step function has returned and output can be used.
  PostCall,
  /// Temporal step function has iterated and output can be used.
  ///
  /// Might transition to `PreTemporalCall`.
  PostTemporalCall {
    step_function: RantFunctionHandle,
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
pub enum SetterKey<'a> {
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
  /// Setter RHS was already consumed and pushed to the value stack.
  FromStack
}