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

    pub fn consume(self) -> (Range<usize>, SyntaxErrorType) {
        (self.span, self.info)
    }
}

/// The information describing a syntax error as seen by the parser.
#[derive(Debug)]
pub enum SyntaxErrorType {
    UnclosedBlock,
    ExpectedToken(String),
    UnexpectedToken(String),
    MissingIdentifier,
    InvalidSinkOn(&'static str),
    InvalidHintOn(&'static str),
    InvalidSink,
    InvalidHint,
}

impl Display for SyntaxErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxErrorType::UnclosedBlock => write!(f, "Unclosed block; expected '}}'"),
            SyntaxErrorType::ExpectedToken(token) => write!(f, "Expected token: '{}'", token),
            SyntaxErrorType::UnexpectedToken(token) => write!(f, "Unexpected token: '{}'", token),
            SyntaxErrorType::MissingIdentifier => write!(f, "Missing identifier"),
            SyntaxErrorType::InvalidSinkOn(elname) => write!(f, "Sink is not valid on {}", elname),
            SyntaxErrorType::InvalidHintOn(elname) => write!(f, "Hint is not valid on {}", elname),
            SyntaxErrorType::InvalidSink => write!(f, "Sink is not valid here"),
            SyntaxErrorType::InvalidHint => write!(f, "Hint is not valid here"),
        }
    }
}