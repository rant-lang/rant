use logos::*;
use crate::InternalString;

#[derive(Logos, Debug, PartialEq)]
pub enum RantToken {
  /// Sequence of printable non-whitespace characters
  #[error]
  #[regex(r"[\w\-_]+")]
  Fragment,

  /// Sequence of printable whitespace characters
  #[regex(r"\s+", filter_bs, priority = 2)]
  Whitespace,
  
  /// Sequence of non-printable whitespace characters
  #[regex(r"[\r\n]+\s*|\s*[\r\n]+", logos::skip, priority = 3)]
  Blackspace,
  
  /// `{`
  #[token("{")]
  LeftBrace,
  
  /// `|`
  #[token("|")]
  Pipe,
  
  /// `}`
  #[token("}")]
  RightBrace,

  /// `|>`
  #[token("|>")]
  Compose,

  /// `[]`
  #[token("[]")]
  ComposeValue,

  /// `<>`
  #[token("<>")]
  Defer,
  
  /// `[`
  #[token("[")]
  LeftBracket,
  
  /// `]`
  #[token("]")]
  RightBracket,

  /// `(`
  #[token("(")]
  LeftParen,

  /// `)`
  #[token(")")]
  RightParen,

  /// `~`
  #[token("~")]
  EmptyValue,

  /// `<`
  #[token("<")]
  LeftAngle,

  /// `>`
  #[token(">")]
  RightAngle,
  
  /// `:`
  #[token(":")]
  Colon,

  /// `**`
  #[token("**")]
  Temporal,
  
  /// Labeled temporal operator, e.g. `*a*`
  #[regex(r"\*[\w\d\-_]+\*", parse_temporal_spread_label)]
  TemporalLabeled(InternalString),

  /// `*`
  #[token("*")]
  Star,
  
  /// `+`
  #[token("+")]
  Plus,

  /// `&`
  #[token("&")]
  And,

  /// `=`
  #[token("=")]
  Equals,

  /// `!`
  #[token("!")]
  Bang,
  
  /// `?`
  #[token("?")]
  Question,
  
  /// `;`
  #[token(";")]
  Semi,
  
  /// `@`
  #[token("@", priority = 1)]
  At,

  /// Some charm keyword, e.g. `@return`
  #[regex(r"@[\w\d_-]+", parse_keyword, priority = 2, ignore(case))]
  Keyword(InternalString),
  
  /// `/`
  #[token("/")]
  Slash,

  /// `^`
  #[token("^")]
  Caret,
  
  /// `$`
  #[token("$")]
  Dollar,

  /// `%`
  #[token("%")]
  Percent,
  
  /// `'`
  #[token("'")]
  Hint,
  
  /// `_`
  #[token("_")]
  Sink,
  
  /// Integer literal
  #[regex(r"\-?[0-9]+", parse_integer, priority = 2)]
  Integer(i64),
  
  /// Float literal
  #[regex(r"\-?[0-9]+(\.[0-9]+([Ee][+\-]?\d+)?|[Ee][+\-]?\d+)", parse_float, priority = 3)]
  Float(f64),
  
  /// Represents inline and multi-line comments
  #[regex(r"\s*##([^#]|#[^#])*(##\s*)?", logos::skip, priority = 6)]
  #[regex(r"\s*#([^#][^\r\n]*)?\n?", logos::skip, priority = 5)]
  Comment,
  
  /// Represents any escape sequence
  #[regex(r"\\\S", parse_escape, priority = 10)]
  #[regex(r"\\x[0-9a-fA-F][0-9a-fA-F]", parse_code_point_escape, priority = 7)]
  Escape(char),
  
  /// Represents a verbatim string literal, e.g. `"hello world"`
  #[regex(r#""(""|[^"])*""#, parse_string_literal)]
  StringLiteral(InternalString),
  
  /// Error token indicating an unterminated string literal, e.g. `"foo`
  #[regex(r#""(""|[^"])*"#)]
  UnterminatedStringLiteral,
}

fn parse_temporal_spread_label(lex: &mut Lexer<RantToken>) -> InternalString {
  let slice = lex.slice();
  InternalString::from(&slice[1 .. slice.len() - 1])
}

fn parse_string_literal(lex: &mut Lexer<RantToken>) -> InternalString {
  let literal = lex.slice();
  let literal_content = &literal[1..literal.len() - 1];
  let mut string_content = InternalString::new();
  let mut prev_quote = false;
  for c in literal_content.chars() {
    match c {
      '"' => {
        if prev_quote {
          prev_quote = false;
          string_content.push('"');
        } else {
          prev_quote = true;
        }
      },
      c => {
        string_content.push(c)
      }
    }
  }
  string_content
}

fn parse_keyword(lex: &mut Lexer<RantToken>) -> InternalString {
  let kwd_literal = lex.slice();
  let kwd_content = &kwd_literal[1..];
  InternalString::from(kwd_content)
}

/// Filter function for whitespace lexer rule to exclude whitespace at start of source
fn filter_bs(lex: &mut Lexer<RantToken>) -> Filter<()> {
  if lex.span().start > 0 {
    return Filter::Emit(())
  }
  Filter::Skip
}

fn parse_escape(lex: &mut Lexer<RantToken>) -> Option<char> {
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

fn parse_code_point_escape(lex: &mut Lexer<RantToken>) -> Option<char> {
  let codepoint = u8::from_str_radix(&lex.slice()[2..], 16).ok()?;
  Some(codepoint as char)
}

fn parse_float(lex: &mut Lexer<RantToken>) -> Option<f64> {
  let slice = lex.slice();
  let n = slice.parse().ok()?;
  Some(n)
}

fn parse_integer(lex: &mut Lexer<RantToken>) -> Option<i64> {
  let slice = lex.slice();
  let n = slice.parse().ok()?;
  Some(n)
}