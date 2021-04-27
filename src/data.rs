use std::{error::Error, fmt::{Debug, Display}};

use crate::{RantValue, runtime::{IntoRuntimeResult, RuntimeError, RuntimeErrorType, RuntimeResult}};

/// Result type used for data source operations.
pub type DataSourceResult<T> = Result<T, DataSourceError>;

/// Trait for defining a Rant data source type.
///
/// ## Security
/// This trait can potentially be used to grant Rant access to network, filesystem, or other sensitive resources.
/// Please take care to ensure that such access is adequately sandboxed, has granular permissions,
/// and that failed access to such resources is handled gracefully.
pub trait DataSource: Debug {
  /// Returns a string identifying the type of data source this is;
  /// ideally, this should be something short and human-readable.
  /// Refer to implementor documentation for the specific string returned.
  ///
  /// ## Security
  /// Because implementors can make this method return any string at all,
  /// usage should be limited to diagnostic and filtering purposes only.
  fn type_id(&self) -> &str;

  /// Requests some data from the data source.
  ///
  /// This method is called by Rant's `[data]` funtion to interact with data sources.
  fn request_data(&self, args: Vec<RantValue>) -> DataSourceResult<RantValue>;
}

/// Error type for data source operations.
#[derive(Debug, Clone)]
pub enum DataSourceError {
  /// User messed up; check string for explanation.
  User(String),
  /// Data source messed up; check string for explanation.
  Internal(String),
}

impl Display for DataSourceError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::User(msg) => write!(f, "user error: {}", msg),
      Self::Internal(msg) => write!(f, "internal: {}", msg),
    }
  }
}

impl Error for DataSourceError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    None
  }

  fn cause(&self) -> Option<&dyn Error> {
    self.source()
  }
}

impl<T> IntoRuntimeResult<T> for DataSourceResult<T> {
  fn into_runtime_result(self) -> RuntimeResult<T> {
    self.map_err(|err| RuntimeError {
      error_type: RuntimeErrorType::DataSourceError(err),
      description: None,
      stack_trace: None,
    })
  }
}