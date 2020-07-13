/// Rant Syntax Tree
pub enum RST<'a> {
    Sequence(Vec<RST<'a>>),
    ListBlock(Vec<RST<'a>>),
    MapBlock(Vec<(RST<'a>, RST<'a>)>),
    FunctionCall{ name: Box<RST<'a>>, args: Vec<RST<'a>> },
    TextChunk(&'a str),
    Whitespace(&'a str),
    Number(u64)
}