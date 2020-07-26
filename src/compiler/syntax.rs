use std::fmt::Display;

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
    Block(Vec<RST>),
    HintedBlock(Vec<RST>),
    List(Vec<RST>),
    Map(Vec<(RST, RST)>),
    Box{ params: Vec<String>, block: Vec<RST> },
    AnonFunctionCall{ funcgen: Box<RST>, args: Vec<RST> },
    HintedAnonFunctionCall{ funcgen: Box<RST>, args: Vec<RST> },
    FunctionCall{ id: Identifier, args: Vec<RST> },
    HintedFunctionCall{ id: Identifier, args: Vec<RST> },
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
            RST::Block(_) =>                        "block",
            RST::HintedBlock(_) =>                  "hinted block",
            RST::List(_) =>                         "list",
            RST::Map(_) =>                          "map",
            RST::Box { .. } =>                      "box",
            RST::AnonFunctionCall { .. } =>         "anonymous function call",
            RST::HintedAnonFunctionCall { .. } =>   "hinted anonymous function call",
            RST::FunctionCall { .. } =>             "function call",
            RST::HintedFunctionCall { .. } =>       "hinted function call",
            RST::Fragment(_) =>                     "fragment",
            RST::Whitespace(_) =>                   "whitespace",
            RST::Integer(_) =>                      "integer",
            RST::Float(_) =>                        "float",
            RST::Boolean(_) =>                      "boolean",
            RST::Nop =>                             "nothing",
            RST::VarDef(_, _) =>                    "variable definition",
            RST::VarGet(_) =>                       "variable",
            RST::VarSet(_, _) =>                    "variable assignment"
        }
    }

    pub fn is_printing(&self) -> bool {
        match self {
            RST::HintedBlock(_) |
            RST::HintedAnonFunctionCall { .. } |
            RST::HintedFunctionCall { .. } |
            RST::Integer(_) |
            RST::Float(_) |
            RST::Boolean(_) |
            RST::Fragment(_) |
            RST::Whitespace(_) |
            RST::VarGet(_)
            => true,
            _ => false
        }
    }
}

impl Display for RST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}