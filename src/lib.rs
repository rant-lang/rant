pub mod compiler;
mod rantpgm;
mod value;
mod table;
mod vm;

pub use rantpgm::*;
pub use value::*;

/// The Rant version according to the crate metadata.
pub const RANT_VERSION: &'static str = env!("CARGO_PKG_VERSION");

/// A Rant execution context.
pub struct RantEngine {

}