use std::{fmt::Display, ops::Range};

/// Describes the location and nature of a syntax error.
#[derive(Debug)]
pub struct SyntaxError {
    span: Range<usize>,
    info: SyntaxErrorType
}

impl SyntaxError {
    pub(crate) fn new(info: SyntaxErrorType, span: Range<usize>) -> Self {
        Self {
            info,
            span
        }
    }

    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn info(&self) -> &SyntaxErrorType {
        &self.info
    }

    pub fn consume(self) -> (Range<usize>, SyntaxErrorType) {
        (self.span, self.info)
    }
}

/// The information describing a syntax error as seen by the parser.
#[derive(Debug)]
pub enum SyntaxErrorType {
    UnexpectedToken(String),
    ExpectedToken(String),
    UnclosedBlock,
    UnclosedFunctionCall,
    UnclosedFunctionSignature,
    UnclosedStringLiteral,
    MissingFunctionBody,
    UnclosedFunctionBody,
    ParamsAfterVariadic,
    InvalidParameter(String),
    MissingIdentifier,
    InvalidIdentifier(String),
    DuplicateParameter(String),
    LocalPathStartsWithIndex,
    InvalidSinkOn(&'static str),
    InvalidHintOn(&'static str),
    InvalidSink,
    InvalidHint,
}

impl SyntaxErrorType {
    pub fn code(&self) -> &'static str {
        match self {
            // Common errors (0000 - 0019)
            SyntaxErrorType::UnexpectedToken(_) =>                              "RE-0000",
            SyntaxErrorType::ExpectedToken(_) =>                                "RE-0001",
            SyntaxErrorType::UnclosedBlock =>                                   "RE-0002",
            SyntaxErrorType::UnclosedFunctionCall =>                            "RE-0003",
            SyntaxErrorType::UnclosedFunctionSignature =>                       "RE-0004",
            SyntaxErrorType::ParamsAfterVariadic =>                             "RE-0005",
            SyntaxErrorType::MissingFunctionBody =>                             "RE-0006",
            SyntaxErrorType::UnclosedFunctionBody =>                            "RE-0007",
            SyntaxErrorType::InvalidParameter(_) =>                             "RE-0008",
            SyntaxErrorType::DuplicateParameter(_) =>                           "RE-0009",
            SyntaxErrorType::UnclosedStringLiteral =>                           "RE-0010",

            // Access path errors (0020 - 0029)
            SyntaxErrorType::MissingIdentifier =>                               "RE-0020",
            SyntaxErrorType::InvalidIdentifier(_) =>                            "RE-0021",
            SyntaxErrorType::LocalPathStartsWithIndex =>                        "RE-0022",

            // Hint/sink errors (0030 - 0039)
            SyntaxErrorType::InvalidSink | SyntaxErrorType::InvalidSinkOn(_) => "RE-0030",
            SyntaxErrorType::InvalidHint | SyntaxErrorType::InvalidHintOn(_) => "RE-0031",
        }
    }

    /// Gets a message describing the error.
    pub fn message(&self) -> String {
        match self {
            SyntaxErrorType::UnclosedBlock => "unclosed block; expected '}'".to_owned(),
            SyntaxErrorType::UnclosedFunctionCall => "unclosed function call; expected ']'".to_owned(),
            SyntaxErrorType::UnclosedStringLiteral => "unclosed string literal".to_owned(),
            SyntaxErrorType::ExpectedToken(token) => format!("expected token: '{}'", token),
            SyntaxErrorType::UnexpectedToken(token) => format!("unexpected token: '{}'", token),
            SyntaxErrorType::MissingIdentifier => "missing identifier".to_owned(),
            SyntaxErrorType::LocalPathStartsWithIndex => "variable access path cannot start with an index".to_owned(),
            SyntaxErrorType::InvalidSinkOn(elname) => format!("sink is not valid on {}", elname),
            SyntaxErrorType::InvalidHintOn(elname) => format!("hint is not valid on {}", elname),
            SyntaxErrorType::InvalidSink => "sink is not valid".to_owned(),
            SyntaxErrorType::InvalidHint => "hint is not valid".to_owned(),
            SyntaxErrorType::InvalidIdentifier(idname) => format!("'{}' is not a valid identifier; identifiers may only contain alphanumeric characters, underscores, and hyphens and must contain at least one non-digit", idname),
            SyntaxErrorType::UnclosedFunctionSignature => "unclosed function signature; expected ']' followed by body block".to_owned(),
            SyntaxErrorType::ParamsAfterVariadic => "variadic parameter must appear last in function signature".to_owned(),
            SyntaxErrorType::MissingFunctionBody => "missing body in function definition".to_owned(),
            SyntaxErrorType::UnclosedFunctionBody => "unclosed function body; expected '}'".to_owned(),
            SyntaxErrorType::InvalidParameter(pname) => format!("invalid parameter '{}'; must be a valid identifier or '*'", pname),
            SyntaxErrorType::DuplicateParameter(pname) => format!("duplicate parameter '{}' in function signature", pname)
        }
    }

    /// Gets a message, usually a brief suggestion, for directly annotating the code causing the error.
    pub fn inline_message(&self) -> String {
        match self {
            SyntaxErrorType::UnclosedBlock => "no matching '}' found".to_owned(),
            SyntaxErrorType::UnclosedStringLiteral => "string literal needs closing delimiter".to_owned(),
            SyntaxErrorType::ExpectedToken(token) => format!("expected '{}'", token),
            SyntaxErrorType::UnexpectedToken(_) => "this probably shouldn't be here".to_owned(),
            SyntaxErrorType::MissingIdentifier => "missing identifier".to_owned(),
            SyntaxErrorType::LocalPathStartsWithIndex => "use a variable name here".to_owned(),
            SyntaxErrorType::InvalidSink | SyntaxErrorType::InvalidSinkOn(_) => "sink not allowed here".to_owned(),
            SyntaxErrorType::InvalidHint | SyntaxErrorType::InvalidHintOn(_) => "hint not allowed here".to_owned(),
            SyntaxErrorType::InvalidIdentifier(_) => "invalid identifier".to_owned(),
            SyntaxErrorType::UnclosedFunctionCall => "no matching ']' found".to_owned(),
            SyntaxErrorType::UnclosedFunctionSignature => "no matching ']' found".to_owned(),
            SyntaxErrorType::ParamsAfterVariadic => "move this to the left of the '*' parameter".to_owned(),
            SyntaxErrorType::MissingFunctionBody => "function body should follow".to_owned(),
            SyntaxErrorType::UnclosedFunctionBody => "no matching '}' found".to_owned(),
            SyntaxErrorType::InvalidParameter(_) => "invalid parameter".to_owned(),
            SyntaxErrorType::DuplicateParameter(_) => "rename parameter to something unique".to_owned()
        }
    }
}

impl Display for SyntaxErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}