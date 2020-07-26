use error::SyntaxErrorType;

pub(crate) mod lexer;
pub(crate) mod reader;
pub mod parser;
pub(crate) mod syntax;
pub(crate) mod error;

pub struct CompilerError {
    info: CompilerErrorInfo,
    index: usize,
    line: usize,
    col: usize,
}

pub enum CompilerErrorInfo {
    SyntaxError(SyntaxErrorType),
    Other
}

pub struct RantCompiler {

}