use logos::*;

#[derive(Debug, PartialEq)]
pub enum BlockType {
    Flat,
    Associative
}

#[derive(Logos, Debug, PartialEq)]
pub enum RantToken<'a> {
    #[error]
    #[regex(r"\w+")]
    Fragment,
    #[token("{")]
    LeftBrace,
    #[token("|")]
    Pipe,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(":")]
    Colon,
    #[token(";")]
    Semicolon,
    #[token("@")]
    At,
    #[regex(r"[\s\t\v\f]+", filter_bs, priority = 2)]
    Whitespace(&'a str),
    #[regex(r"[\r\n]+[\r\n\s\t\v\f]*|[\r\n\s\t\v\f]*[\r\n]+", logos::skip, priority = 3)]
    Blackspace,
    #[regex(r"[0-9]+(\.[0-9]+)?", parse_number, priority = 2)]
    Number(f64),
    #[regex(r"\s*#[^\r\n]*\n?", logos::skip, priority = 5)]
    #[regex(r"\s*###([^#]|#[^#]|##[^#])*###", logos::skip, priority = 6)]
    Comment,
    #[regex(r"\\\S", parse_escape, priority = 10)]
    #[regex(r"\\x[0-9a-fA-F][0-9a-fA-F]", parse_code_point_escape, priority = 7)]
    Escape(char),
}

fn filter_bs<'a>(lex: &mut Lexer<'a, RantToken<'a>>) -> Filter<&'a str> {
    if lex.span().start == 0 {
        return Filter::Skip
    }
    Filter::Emit(lex.slice())
}

fn parse_escape<'a>(lex: &mut Lexer<'a, RantToken<'a>>) -> Option<char> {
    let slice = lex.slice();
    Some(match slice.chars().nth(1)? {
        'r' => '\r',
        'n' => '\n',
        't' => '\t',
        '0' => '\0',
        's' => ' ',
        other => other
    })
}

fn parse_code_point_escape<'a>(lex: &mut Lexer<'a, RantToken<'a>>) -> Option<char> {
    let codepoint = u8::from_str_radix(&lex.slice()[2..], 16).ok()?;
    Some(codepoint as char)
}

fn parse_number<'a>(lex: &mut Lexer<'a, RantToken<'a>>) -> Option<f64> {
    let slice = lex.slice();
    let n: f64 = slice.parse().ok()?;
    Some(n)
}

pub fn lex<'a>(src: &'a str) -> Lexer<'a, RantToken> {
    RantToken::lexer(src)
}