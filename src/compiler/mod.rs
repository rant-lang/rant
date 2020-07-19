pub(crate) mod lexer;
pub mod reader; // TODO: Hide this after testing is done
pub(crate) mod parser;
pub(crate) mod rst;

pub struct CompilerError {
    info: CompilerErrorInfo,
    index: usize,
    line: usize,
    col: usize,
}

pub enum CompilerErrorInfo {
    UnclosedBlock,
    ExpectedToken(String),
    UnexpectedToken(String),
    OrphanedSink,
    OrphanedHint,
}

pub struct RantCompiler {

}