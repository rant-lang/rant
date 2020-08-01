#![allow(dead_code)]
#![allow(unused_macros)]

mod runtime;
mod syntax;
mod convert;
mod util;
pub mod stdlib;
pub mod value;
pub mod compiler;

pub use value::*;
pub use convert::*;
use syntax::{Sequence};

/// The Rant version according to the crate metadata.
pub const RANT_VERSION: &str = env!("CARGO_PKG_VERSION");

pub type RantResult<T> = Result<T, RantError>;

/// A Rant execution context.
#[derive(Debug)]
pub struct Rant {
    globals: RantMap
}

/// A compiled Rant program.
#[derive(Debug)]
pub struct RantProgram {
    root: Sequence
}

impl RantProgram {
    pub(crate) fn new(root: Sequence) -> Self {
        RantProgram {
            root
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
    /// Stack underflow
    StackUnderflow,
    /// Variable access error, such as attempting to access a nonexistent variable
    InvalidAccess,
    /// Error in function outside of Rant
    ExternalError,
}