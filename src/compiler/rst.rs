/// Rant Syntax Tree
pub enum RST<'a> {
    Sequence(Vec<RST<'a>>),
    Block(Vec<RST<'a>>),
    List(Vec<RST<'a>>),
    MapBlock(Vec<(RST<'a>, RST<'a>)>),
    Box{ params: Vec<&'a str>, block: Vec<RST<'a>> },
    AnonymousFunctionCall{ name: Box<RST<'a>>, args: Vec<RST<'a>> },
    NamedFunctionCall{ name: &'a str, args: Vec<RST<'a>> },
    Fragment(&'a str),
    Whitespace(&'a str),
    Number(u64)
}