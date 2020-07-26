use parser::SyntaxErrorType;

pub(crate) mod lexer;
pub(crate) mod reader;
pub mod parser;
pub(crate) mod syntax;

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