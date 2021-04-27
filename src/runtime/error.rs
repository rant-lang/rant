use std::{error::Error, fmt::Display};

use crate::{IndexError, KeyError, ModuleLoadError, SliceError, ValueError, data::DataSourceError};

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
  /// Stack has overflowed.
  ///
  /// Rant error ID: `STACK_OVERFLOW_ERROR`
  StackOverflow,
  /// Stack has underflowed.
  ///
  /// Rant error ID: `STACK_UNDERFLOW_ERROR`
  StackUnderflow,
  /// Variable access error, such as attempting to access a nonexistent variable or write to a constant
  ///
  /// Rant error ID: `INVALID_ACCESS_ERROR`
  InvalidAccess,
  /// Operation is not valid for the current program state
  ///
  /// Rant error ID: `INVALID_OP_ERROR`
  InvalidOperation,
  /// Internal VM error, usually indicating a bug or corrupted data
  ///
  /// Rant error ID: `INTERNAL_ERROR`
  InternalError,
  /// Too few/many arguments were passed to a function
  ///
  /// Rant error ID: `ARG_MISMATCH_ERROR`
  ArgumentMismatch,
  /// Invalid argument passed to function
  ///
  /// Rant error ID: `ARG_ERROR`
  ArgumentError,
  /// Tried to invoke a non-function
  ///
  /// Rant error ID: `INVOKE_ERROR`
  CannotInvokeValue,
  /// Assertion failed
  ///
  /// Rant error ID: `ASSERT_ERROR`
  AssertError,
  /// Error occurred due to unexpected value type
  ///
  /// Rant error ID: `TYPE_ERROR`
  TypeError,
  /// Error occurred when creating value
  ///
  /// Rant error ID: `VALUE_ERROR`
  ValueError(ValueError),
  /// Error occurred while indexing value
  ///
  /// Rant error ID: `INDEX_ERROR`
  IndexError(IndexError),
  /// Error occurred while keying value
  ///
  /// Rant error ID: `KEY_ERROR`
  KeyError(KeyError),
  /// Error occurred while slicing value
  ///
  /// Rant error ID: `SLICE_ERROR`
  SliceError(SliceError),
  /// Error occurred while iterating selector
  ///
  /// Rant error ID: `SELECTOR_ERROR`
  SelectorError(SelectorError),
  /// Error occurred while trying to load a module
  ///
  /// Rant error ID: `MODULE_ERROR`
  ModuleLoadError(ModuleLoadError),
  /// Error manually triggered by program
  ///
  /// Rant error ID: `USER_ERROR`
  UserError,
  /// Error during control flow operation (e.g. return or break)
  ///
  /// Rant error ID: `CONTROL_FLOW_ERROR`
  ControlFlowError,
  /// Error occurred during data source operation.
  ///
  /// Rant error ID: `DATA_SOURCE_ERROR`
  DataSourceError(DataSourceError),
}

impl Display for RuntimeErrorType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", match self {
      Self::StackOverflow => "STACK_OVERFLOW_ERROR",
      Self::StackUnderflow => "STACK_UNDERFLOW_ERROR",
      Self::InvalidAccess => "INVALID_ACCESS_ERROR",
      Self::InvalidOperation => "INVALID_OP_ERROR",
      Self::InternalError => "INTERNAL_ERROR",
      Self::ArgumentMismatch => "ARG_MISMATCH_ERROR",
      Self::ArgumentError => "ARG_ERROR",
      Self::CannotInvokeValue => "INVOKE_ERROR",
      Self::UserError => "USER_ERROR",
      Self::AssertError => "ASSERT_ERROR",
      Self::TypeError => "TYPE_ERROR",
      Self::ValueError(_) => "VALUE_ERROR",
      Self::IndexError(_) => "INDEX_ERROR",
      Self::KeyError(_) => "KEY_ERROR",
      Self::SliceError(_) => "SLICE_ERROR",
      Self::SelectorError(_) => "SELECTOR_ERROR",
      Self::ModuleLoadError(_) => "MODULE_ERROR",
      Self::ControlFlowError => "CONTROL_FLOW_ERROR",
      Self::DataSourceError(_) => "DATA_SOURCE_ERROR",
    })
  }
}

pub(crate) trait IntoRuntimeResult<T> {
  fn into_runtime_result(self) -> RuntimeResult<T>;
}