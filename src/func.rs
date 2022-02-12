use std::{mem::{transmute, size_of}, rc::Rc, fmt::Debug};
use crate::*;
use crate::lang::*;
use crate::runtime::*;
use crate::stdlib::*;

/// A function callable from Rant.
#[derive(Debug)]
pub struct RantFunction {
  /// Parameter information for the function.
  pub(crate) params: Rc<Vec<Parameter>>,
  /// The number of required parameters.
  pub(crate) min_arg_count: usize,
  /// The parameter index at which variadic parameters start.
  /// If this is greater than or equal to the number of params, there are no variadic parameters.
  pub(crate) vararg_start_index: usize,
  /// The external variables captured by the function when it was defined.
  pub(crate) captured_vars: Vec<(Identifier, RantVar)>,
  /// The body of the function.
  pub(crate) body: RantFunctionInterface,
  /// Assigns a custom flavor to the stack frame created by the function call.
  /// If not set, the default function call flavor will be used.
  pub(crate) flavor: Option<StackFrameFlavor>,
}

impl RantFunction {
  /// Returns true if the function should be treated as variadic.
  #[inline]
  pub fn is_variadic(&self) -> bool {
    self.vararg_start_index < self.params.len() || self.vararg_start_index < self.min_arg_count
  }

  /// Returns true if the function is native.
  #[inline]
  pub fn is_native(&self) -> bool {
    matches!(self.body, RantFunctionInterface::Foreign(_))
  }
}

/// Defines endpoint variants for Rant functions.
#[derive(Clone)]
pub enum RantFunctionInterface {
  /// Represents a foreign function as a wrapper function accepting a variable number of arguments.
  Foreign(Rc<dyn 'static + Fn(&mut VM, Vec<RantValue>) -> RantStdResult>),
  /// Represents a user function as an RST.
  User(Rc<Sequence>)
}

impl Debug for RantFunctionInterface {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      RantFunctionInterface::Foreign(func) => unsafe {
        let (a, b) = transmute::<_, (usize, usize)>(Rc::as_ptr(func));
        write!(f, "{:#02$x}{:02$x}", a, b, &(size_of::<usize>() * 2))
      },
      RantFunctionInterface::User(func) => write!(f, "{:#p}", Rc::as_ptr(func))
    }
  }
}
