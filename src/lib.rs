#![allow(dead_code)]
#![allow(unused_macros)]

mod runtime;
mod lang;
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
use crate::lang::{Sequence};
use crate::compiler::{CompileResult, RantCompiler, Reporter};
use crate::runtime::VM;
use std::{path::Path, rc::Rc, cell::RefCell};
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
  globals: RefCell<RantMap>
}

impl Rant {
  pub fn new() -> Self {
    Self::with_seed(0)
  }
  
  pub fn with_seed(seed: u64) -> Self {
    let mut globals = RantMap::new();
    
    // Load standard library
    stdlib::load_stdlib(&mut globals);

    Self {
      globals: RefCell::new(globals),
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
  /// Compiles a source string using the specified reporter.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile<R: Reporter>(&self, source: &str, reporter: &mut R) -> CompileResult {
    RantCompiler::compile_string(source, reporter)
  }

  /// Compiles a source string without reporting problems.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_quiet(&self, source: &str) -> CompileResult {
    RantCompiler::compile_string(source, &mut ())
  }
  
  /// Compiles a source file using the specified reporter.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_file<P: AsRef<Path>, R: Reporter>(&self, path: P, reporter: &mut R) -> CompileResult {
    RantCompiler::compile_file(path, reporter)
  }

  /// Compiles a source file without reporting problems.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_file_quiet<P: AsRef<Path>>(&self, path: P) -> CompileResult {
    RantCompiler::compile_file(path, &mut ())
  }
  
  /// Gets the current RNG seed.
  pub fn seed(&self) -> u64 {
    self.rng.seed()
  }
  
  /// Re-seeds the RNG with the specified seed.
  pub fn set_seed(&mut self, seed: u64) {
    self.rng = Rc::new(RantRng::new(seed));
  }
  
  /// Resets the RNG back to its initial state with the current seed.
  pub fn reset_seed(&mut self) {
    let seed = self.rng.seed();
    self.rng = Rc::new(RantRng::new(seed));
  }
  
  /// Runs the specified program.
  pub fn run(&mut self, program: &RantProgram) -> RantResult<String> {
    VM::new(self.rng.clone(), self, program).run()
  }
}

/// A compiled Rant program.
#[derive(Debug)]
pub struct RantProgram {
  name: Option<String>,
  root: Rc<Sequence>
}

impl RantProgram {
  pub(crate) fn new(root: Rc<Sequence>) -> Self {
    Self {
      name: None,
      root,
    }
  }

  pub fn with_name<S: ToString>(self, name: S) -> Self {
    Self {
      root: self.root,
      name: Some(name.to_string())
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
  /// Too few/many arguments were passed to a function
  ArgumentMismatch,
  /// Tried to invoke a non-function
  CannotInvokeValue,
  /// Error occurred while indexing value
  IndexError(ValueIndexError),
  /// Error occurred while keying value
  KeyError(ValueKeyError),
}