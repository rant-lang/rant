use crate::{syntax::Sequence, RantProgram};
use error::SyntaxErrorType;
use parser::RantParser;
use line_col::LineColLookup;
use std::{fmt::Display, ops::Range};

pub(crate) mod lexer;
pub(crate) mod reader;
pub(crate) mod parser;
pub(crate) mod error;

pub type CompileResult = Result<RantProgram, Vec<CompilerError>>;

#[derive(Debug)]
pub struct LineCol {
    line: usize,
    col: usize
}

impl LineCol {
    pub fn new((line, col): (usize, usize)) -> Self {
        Self {
            line, col
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }
}

impl Display for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.line, self.col)
    }
}

#[derive(Debug)]
pub struct CompilerError {
    pub info: CompilerErrorType,
    pub index_range: Range<usize>,
    pub first_line_col: LineCol,
    pub last_line_col: LineCol
}

#[derive(Debug)]
pub enum CompilerErrorType {
    SyntaxError(SyntaxErrorType),
    Other
}

impl Display for CompilerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerErrorType::SyntaxError(err) => err.fmt(f),
            CompilerErrorType::Other => write!(f, "(other error"),
        }
    }
}

pub struct RantCompiler {
    // TODO: Add compiler options
}

impl RantCompiler {
    pub fn compile_string(source: &str) -> CompileResult {
        let mut parser = RantParser::new(source);
        match parser.parse() {
            Ok(rst) => Ok(RantProgram::new(Sequence::from(rst))),
            Err(mut errors) => {
                let lookup = LineColLookup::new(source);
                Err(errors.drain(..).map(|err| {
                    let (span, info) = err.consume();
                    let (line_start, col_start) = lookup.get(span.start);
                    let (line_end, col_end) = lookup.get(span.end.saturating_sub(1));
                    CompilerError {
                        info: CompilerErrorType::SyntaxError(info),
                        index_range: span,
                        first_line_col: LineCol::new((line_start, col_start)),
                        last_line_col: LineCol::new((line_end, col_end))
                    }
                }).collect())
            }
        }
    }
}