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

    pub fn info<'a>(&'a self) -> &'a SyntaxErrorType {
        &self.info
    }
}

/// The information describing a syntax error as seen by the parser.
#[derive(Debug)]
pub enum SyntaxErrorType {
    UnclosedBlock,
    ExpectedToken(String),
    UnexpectedToken(String),
    MissingIdentifier,
    InvalidSink,
    InvalidHint,
}

impl Display for SyntaxErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxErrorType::UnclosedBlock => write!(f, "Unclosed block; expected either '|' or '}}' before end of file"),
            SyntaxErrorType::ExpectedToken(token) => write!(f, "Expected token: '{}'", token),
            SyntaxErrorType::UnexpectedToken(token) => write!(f, "Unexpected token: '{}'", token),
            SyntaxErrorType::MissingIdentifier => write!(f, "Missing identifier"),
            SyntaxErrorType::InvalidSink => write!(f, "Element cannot be sinked"),
            SyntaxErrorType::InvalidHint => write!(f, "Element cannot be hinted")
        }
    }
}