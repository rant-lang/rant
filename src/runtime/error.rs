use std::{error::Error, fmt::Display};

use crate::{IndexError, KeyError, ModuleLoadError, SliceError, ValueError};

use super::{resolver::SelectorError};

/// Type alias for `Result<T, RuntimeError>`
pub type RuntimeResult<T> = Result<T, RuntimeError>;
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
      RuntimeErrorType::StackOverflow => "STACK_OVERFLOW_ERROR",
      RuntimeErrorType::StackUnderflow => "STACK_UNDERFLOW_ERROR",
      RuntimeErrorType::InvalidAccess => "INVALID_ACCESS_ERROR",
      RuntimeErrorType::InvalidOperation => "INVALID_OP_ERROR",
      RuntimeErrorType::InternalError => "EXTERNAL_ERROR",
      RuntimeErrorType::ArgumentMismatch => "ARG_MISMATCH_ERROR",
      RuntimeErrorType::ArgumentError => "ARG_ERROR",
      RuntimeErrorType::CannotInvokeValue => "INVOKE_ERROR",
      RuntimeErrorType::UserError => "USER_ERROR",
      RuntimeErrorType::AssertError => "ASSERT_ERROR",
      RuntimeErrorType::TypeError => "TYPE_ERROR",
      RuntimeErrorType::ValueError(_) => "VALUE_ERROR",
      RuntimeErrorType::IndexError(_) => "INDEX_ERROR",
      RuntimeErrorType::KeyError(_) => "KEY_ERROR",
      RuntimeErrorType::SliceError(_) => "SLICE_ERROR",
      RuntimeErrorType::SelectorError(_) => "ERR_SELECTOR_ERROR",
      RuntimeErrorType::ModuleLoadError(_) => "ERR_MODULE_ERROR",
      RuntimeErrorType::ControlFlowError => "CONTROL_FLOW_ERROR",
    })
  }
}

pub(crate) trait IntoRuntimeResult<T> {
  fn into_runtime_result(self) -> RuntimeResult<T>;
}