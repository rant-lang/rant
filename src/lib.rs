#![allow(dead_code)]
#![allow(unused_macros)]

mod stdlib;
mod runtime;
mod syntax;
mod convert;
pub mod value;
pub mod compiler;

pub use value::*;
pub use convert::*;
use syntax::RST;

/// The Rant version according to the crate metadata.
pub const RANT_VERSION: &str = env!("CARGO_PKG_VERSION");

pub type RantResult<T> = Result<T, RantError>;

/// Rant execution context
pub struct Rant {
    globals: RantMap
}

#[derive(Debug)]
pub struct RantProgram {
    rst: RST
}

impl RantProgram {
    pub(crate) fn new(pgm_root: RST) -> Self {
        RantProgram {
            rst: pgm_root
        }
    }
}

/// Provides information about errors encountered in Rant.
pub enum RantError {
    /// Indicates a runtime error.
    RuntimeError {
        error_type: RuntimeErrorType,
        description: Option<String>
    },
    /// Indicates that a value conversion to/from a Rant value type has failed.
    ValueConversionError {
        from: &'static str,
        to: &'static str,
        message: Option<String>
    },
    Other
}

/// Provides general categories of runtime errors encountered in Rant.
pub enum RuntimeErrorType {
    /// General error type; check message attached to error
    GeneralError,
    /// Stack overflow
    StackOverflow,
    /// Variable access error, such as attempting to access a nonexistent variable
    InvalidAccess,
    /// Error in function outside of Rant
    ExternalError,
}