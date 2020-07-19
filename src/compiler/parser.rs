use super::{rst::RST, reader::RantTokenReader};
use std::ops::Range;

pub type ParseResult<'a> = Result<RST<'a>, Vec<SyntaxError>>;

pub struct SyntaxError {
    span: Range<usize>,
    info: SyntaxErrorType
}

pub enum SyntaxErrorType {
    UnclosedBlock,
    ExpectedToken(String),
    UnexpectedToken(String),
    OrphanedSink,
    OrphanedHint,
}

pub struct RantParser<'source> {
    reader: RantTokenReader<'source>
}

impl<'source> RantParser<'source> {
    pub fn new(source: &'source str) -> Self {
        let reader = RantTokenReader::new(source);
        Self {
            reader
        }
    }
}

impl<'source> RantParser<'source> {
    pub fn read_sequence<'a>(&mut self) -> ParseResult<'a> {
        if let Some((token, span)) = self.reader.next() {
            todo!()
        }
        Ok(RST::Nop)
    }
}