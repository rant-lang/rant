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
    MissingIdentifier,
    InvalidSinkOn(&'static str),
    InvalidHintOn(&'static str),
    InvalidSink,
    InvalidHint,
}

impl SyntaxErrorType {
    pub fn code(&self) -> &'static str {
        match self {
            SyntaxErrorType::UnexpectedToken(_) =>                              "RC0000",
            SyntaxErrorType::ExpectedToken(_) =>                                "RC0001",
            SyntaxErrorType::UnclosedBlock =>                                   "RC0002",
            SyntaxErrorType::MissingIdentifier =>                               "RC0003",
            SyntaxErrorType::InvalidSink | SyntaxErrorType::InvalidSinkOn(_) => "RC0004",
            SyntaxErrorType::InvalidHint | SyntaxErrorType::InvalidHintOn(_) => "RC0005",
        }
    }

    pub fn message(&self) -> String {
        match self {
            SyntaxErrorType::UnclosedBlock => "unclosed block; expected '}'".to_owned(),
            SyntaxErrorType::ExpectedToken(token) => format!("expected token: '{}'", token),
            SyntaxErrorType::UnexpectedToken(token) => format!("unexpected token: '{}'", token),
            SyntaxErrorType::MissingIdentifier => "missing identifier".to_owned(),
            SyntaxErrorType::InvalidSinkOn(elname) => format!("sink is not valid on {}", elname),
            SyntaxErrorType::InvalidHintOn(elname) => format!("hint is not valid on {}", elname),
            SyntaxErrorType::InvalidSink => "sink is not valid".to_owned(),
            SyntaxErrorType::InvalidHint => "hint is not valid".to_owned(),
        }
    }

    pub fn inline_message(&self) -> String {
        match self {
            SyntaxErrorType::UnclosedBlock => "no matching '}' found".to_owned(),
            SyntaxErrorType::ExpectedToken(token) => format!("expected '{}'", token),
            SyntaxErrorType::UnexpectedToken(_) => "this probably shouldn't be here".to_owned(),
            SyntaxErrorType::MissingIdentifier => "missing identifier".to_owned(),
            SyntaxErrorType::InvalidSink | SyntaxErrorType::InvalidSinkOn(_) => "sink not valid here".to_owned(),
            SyntaxErrorType::InvalidHint | SyntaxErrorType::InvalidHintOn(_) => "hint not valid here".to_owned(),
        }
    }
}

impl Display for SyntaxErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}