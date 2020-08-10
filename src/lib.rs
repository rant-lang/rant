#![allow(dead_code)]
#![allow(unused_macros)]

mod runtime;
mod syntax;
mod convert;
mod random;
mod util;
mod collections;
pub mod stdlib;
pub mod value;
pub mod compiler;

pub use collections::*;
pub use value::*;
pub use convert::*;
use syntax::{Sequence};
use std::{path::Path, rc::Rc};
use compiler::{CompileResult, RantCompiler};
use runtime::VM;
use random::RantRng;

/// The build version according to the crate metadata at the time of compiling.
pub const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");

/// The Rant language version implemented by this library.
pub const RANT_VERSION: &str = "4.0";

pub(crate) type RantString = smartstring::alias::CompactString;
pub type RantResult<T> = Result<T, RantError>;

/// A Rant execution context.
#[derive(Debug)]
pub struct Rant {
  rng: Rc<RantRng>,
  globals: RantMap
}

impl Rant {
  pub fn new() -> Self {
    Self {
      globals: RantMap::new(),
      rng: Rc::new(RantRng::new(0))
    }
  }
  
  pub fn with_seed(seed: u64) -> Self {
    Self {
      globals: RantMap::new(),
      rng: Rc::new(RantRng::new(seed))
    }
  }
}

impl Default for Rant {
  fn default() -> Self {
    Self::new()
  }
}

impl Rant {
  pub fn compile_str(&self, source: &str) -> CompileResult {
    RantCompiler::compile_string(source)
  }
  
  pub fn compile_file<P: AsRef<Path>>(&self, path: P) -> CompileResult {
    RantCompiler::compile_file(path)
  }
  
  pub fn seed(&self) -> u64 {
    self.rng.seed()
  }
  
  pub fn set_seed(&mut self, seed: u64) {
    self.rng = Rc::new(RantRng::new(seed));
  }
  
  pub fn reset_seed(&mut self) {
    let seed = self.rng.seed();
    self.rng = Rc::new(RantRng::new(seed));
  }
  
  pub fn run(&mut self, program: &RantProgram) -> RantResult<String> {
    VM::new(self.rng.clone(), self, program).run()
  }
}

/// A compiled Rant program.
#[derive(Debug)]
pub struct RantProgram {
  root: Rc<Sequence>
}

impl RantProgram {
  pub(crate) fn new(root: Rc<Sequence>) -> Self {
    RantProgram {
      root
    }
  }
}

/// Provides information about errors encountered in Rant.
#[derive(Debug)]
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
#[derive(Debug)]
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
  /// Attempted division by zero
  DivideByZero,
}