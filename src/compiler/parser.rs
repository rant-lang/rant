use super::{rst::RST, reader::RantTokenReader, CompilerErrorInfo, CompilerError};

pub type RantParseResult<'a> = Result<RST<'a>, Vec<CompilerError>>;

pub struct RantParser<'source> {
    reader: RantTokenReader<'source>
}

impl<'source> RantParser<'source> {
    pub fn new(reader: RantTokenReader<'source>) -> Self {
        Self {
            reader
        }
    }
}

impl<'source> RantParser<'source> {
    pub fn read_sequence<'a>(&mut self) -> RantParseResult<'a> {
        let token = self.reader.next();
        todo!()
    }
}