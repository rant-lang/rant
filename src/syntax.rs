//! # Syntax module
//! The `syntax` module contains Rant's AST implementation and supporting data structures.

use std::fmt::Display;

/// Printflags indicate to the compiler whether a given program element is likely to print something or not.
#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PrintFlag {
    /// Use default printing behavior.
    None,
    /// Treat the marked element as printing.
    Hint,
    /// Treat the marked element as non-printing.
    Sink
}

/// Component in a variable accessor chain
#[derive(Debug)]
pub enum IdentifierComponent {
    /// Name of variable or map item
    Name(String),
    /// List index
    Index(usize)
}

/// An identifier consisting of a chain of variable names and indices
#[derive(Debug)]
pub struct Identifier {
    parts: Vec<IdentifierComponent>
}

/// Rant Syntax Tree
#[derive(Debug)]
pub enum RST {
    Sequence(Vec<RST>),
    Block(PrintFlag, Vec<RST>),
    List(Vec<RST>),
    Map(Vec<(RST, RST)>),
    Box{ flag: PrintFlag, params: Vec<String>, block: Vec<RST> },
    AnonFunctionCall{ flag: PrintFlag, funcgen: Box<RST>, args: Vec<RST> },
    FunctionCall{ flag: PrintFlag, id: Identifier, args: Vec<RST> },
    VarDef(Identifier, Option<Box<RST>>),
    VarGet(Identifier),
    VarSet(Identifier, Box<RST>),
    Fragment(String),
    Whitespace(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Nop
}

impl RST {
    pub fn display_name(&self) -> &'static str {
        match self {
            RST::Sequence(_) =>                     "sequence",
            RST::Block(..) =>                       "block",
            RST::List(_) =>                         "list",
            RST::Map(_) =>                          "map",
            RST::Box { .. } =>                      "box",
            RST::AnonFunctionCall { .. } =>         "anonymous function call",
            RST::FunctionCall { .. } =>             "function call",
            RST::Fragment(_) =>                     "fragment",
            RST::Whitespace(_) =>                   "whitespace",
            RST::Integer(_) =>                      "integer",
            RST::Float(_) =>                        "float",
            RST::Boolean(_) =>                      "boolean",
            RST::Nop =>                             "nothing",
            RST::VarDef(..) =>                      "variable definition",
            RST::VarGet(_) =>                       "variable",
            RST::VarSet(..) =>                      "variable assignment"
        }
    }

    pub fn is_printing(&self) -> bool {
        matches!(self, 
            RST::Block(PrintFlag::Hint, _) |
            RST::AnonFunctionCall { flag: PrintFlag::Hint, .. } |
            RST::FunctionCall { flag: PrintFlag::Hint, .. } |
            RST::Integer(_) |
            RST::Float(_) |
            RST::Boolean(_) |
            RST::Fragment(_) |
            RST::Whitespace(_) |
            RST::VarGet(_)
        )
    }
}

impl Display for RST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}