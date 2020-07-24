use std::fmt::Display;

/// Rant Syntax Tree
pub enum RST {
    Sequence(Vec<RST>),
    Block(Vec<RST>),
    HintedBlock(Vec<RST>),
    List(Vec<RST>),
    Map(Vec<(RST, RST)>),
    Box{ params: Vec<String>, block: Vec<RST> },
    AnonFunctionCall{ name: Box<RST>, args: Vec<RST> },
    HintedAnonFunctionCall{ name: Box<RST>, args: Vec<RST> },
    FunctionCall{ name: String, args: Vec<RST> },
    HintedFunctionCall{ name: String, args: Vec<RST> },
    Fragment(String),
    Whitespace(String),
    Integer(u64),
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
            RST::Whitespace(_) => true,
            _ => false
        }
    }
}

impl Display for RST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}