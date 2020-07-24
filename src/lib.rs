mod rantpgm;
mod stdlib;
mod table;
mod value;
mod runtime;
pub mod compiler;

pub use rantpgm::*;
pub use value::*;

/// The Rant version according to the crate metadata.
pub const RANT_VERSION: &'static str = env!("CARGO_PKG_VERSION");

/// A Rant execution context.
pub struct RantEngine {

}

pub type RantResult<T> = Result<T, RantError>;

pub enum RantError {
    RuntimeError(String),
    ValueConversionError {
        from: &'static str,
        to: &'static str,
        message: Option<String>
    },
    Other
}