//! # Rant
//!
//! Rant is a language for procedural text generation.
//! It is designed to help you write more dynamic and expressive templates, dialogue, stories, names, test data, and much more.
//!
//! For language documentation, see the [Rant Reference](https://docs.rant-lang.org).
//! 
//! ## The Rant context
//!
//! All programs are run through a Rant context, represented by the [`Rant`] struct.
//! It allows you to execute Rant programs, define and retrieve global variables, manipulate the RNG, and compile Rant code.
//! 
//! ## Reading compiler errors
//! 
//! You will notice that the `Err` variant of the `Rant::compile*` methods is `()` instead of providing an error list. Instead, 
//! errors and warnings are reported via implementors of the [`Reporter`] trait, which allows the user to control what happens to messages emitted by the compiler.
//! Currently, Rant has two built-in `Reporter` implementations: the unit type `()`, and `Vec<CompilerMessage>`.
//! You can also make your own custom reporters to suit your specific needs.
//!
//! [`Rant`]: struct.Rant.html
//! [`Reporter`]: compiler/trait.Reporter.html
//! [`Vec<CompilerMessage>`]: compiler/struct.CompilerMessage.html


#![allow(dead_code)]
#![allow(unused_macros)]

mod runtime;
mod lang;
mod convert;
mod random;
mod util;
mod collections;
mod stdlib;
mod value;
pub mod compiler;

pub use collections::*;
pub use value::*;
pub use convert::*;
use crate::lang::{Sequence};
use crate::compiler::{RantCompiler, Reporter, ErrorKind as CompilerErrorKind};
use crate::runtime::*;
use std::{path::Path, rc::Rc, cell::RefCell};
use random::RantRng;

/// The build version according to the crate metadata at the time of compiling.
pub const BUILD_VERSION: &str = env!("CARGO_PKG_VERSION");

/// The Rant language version implemented by this library.
pub const RANT_VERSION: &str = "4.0";

pub(crate) type RantString = smartstring::alias::CompactString;

/// A Rant execution context.
#[derive(Debug)]
pub struct Rant {
  rng: Rc<RantRng>,
  debug_mode: bool,
  globals: RantMapRef,
}

impl Rant {
  /// Creates a new Rant context with the default seed (0) and loads the standard library.
  pub fn new() -> Self {
    Self::with_seed(0)
  }
  
  /// Creates a new Rant context with the specified seed and loads the standard library.
  pub fn with_seed(seed: u64) -> Self {
    Self::with_options(RantOptions {
      seed,
      .. Default::default()
    })
  }

  /// Creates a new Rant context with the specified options.
  pub fn with_options(options: RantOptions) -> Self {
    let mut rant = Self {
      debug_mode: options.debug_mode,
      globals: Rc::new(RefCell::new(RantMap::new())),
      rng: Rc::new(RantRng::new(options.seed))
    };
    if options.use_stdlib {
      rant.load_stdlib();
    }
    rant.set_default_globals();
    rant
  }

  fn load_stdlib(&mut self) {
    let mut globals = self.globals.borrow_mut();
    // Load standard library
    stdlib::load_stdlib(&mut globals);    
  }

  fn set_default_globals(&mut self) {
    let mut globals = self.globals.borrow_mut();
    // Add standard variables
    // TODO: Make these read-only
    globals.raw_set("RANT_VERSION", RantValue::String(RANT_VERSION.to_owned()));
    globals.raw_set("_GLOBALS", RantValue::Map(Rc::clone(&self.globals)));
  }
}

impl Default for Rant {
  /// Creates a default `Rant` instance.
  fn default() -> Self {
    Self::new()
  }
}

impl Rant {
  /// Compiles a source string using the specified reporter.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile<R: Reporter>(&self, source: &str, reporter: &mut R) -> Result<RantProgram, CompilerErrorKind> {
    RantCompiler::compile_string(source, reporter)
  }

  /// Compiles a source string without reporting problems.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_quiet(&self, source: &str) -> Result<RantProgram, CompilerErrorKind> {
    RantCompiler::compile_string(source, &mut ())
  }
  
  /// Compiles a source file using the specified reporter.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_file<P: AsRef<Path>, R: Reporter>(&self, path: P, reporter: &mut R) -> Result<RantProgram, CompilerErrorKind> {
    RantCompiler::compile_file(path, reporter)
  }

  /// Compiles a source file without reporting problems.
  #[must_use = "compiling a program without storing or running it achieves nothing"]
  pub fn compile_file_quiet<P: AsRef<Path>>(&self, path: P) -> Result<RantProgram, CompilerErrorKind> {
    RantCompiler::compile_file(path, &mut ())
  }

  /// Gets the global variable map of the Rant context.
  pub fn globals(&self) -> RantMapRef {
    Rc::clone(&self.globals)
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
  pub fn run(&mut self, program: &RantProgram) -> RuntimeResult<String> {
    VM::new(self.rng.clone(), self, program).run()
  }
}

/// Provides options for customizing the creation of a `Rant` instance.
pub struct RantOptions {
  /// Specifies whether the standard library should be loaded.
  pub use_stdlib: bool,
  /// Enables debug mode, which includes additional debug information in compiled programs and more detailed runtime error data.
  pub debug_mode: bool,
  /// The initial seed to pass to the RNG. Defaults to 0.
  pub seed: u64,
}

impl Default for RantOptions {
  fn default() -> Self {
    Self {
      use_stdlib: true,
      debug_mode: false,
      seed: 0,
    }
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

  /// Consumes a program, assigns the specified name to it, and returns it.
  pub fn with_name<S: ToString>(self, name: S) -> Self {
    Self {
      root: self.root,
      name: Some(name.to_string())
    }
  }

  /// Gets the name of the program, if any.
  pub fn name(&self) -> Option<String> {
    self.name.clone()
  }
}