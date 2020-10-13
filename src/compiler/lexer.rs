use logos::*;
use crate::RantString;

#[derive(Logos, Debug, PartialEq)]
pub enum RantToken {
  #[error]
  #[regex(r"[\w\-_]+")]
  Fragment,
  
  #[token("{")]
  LeftBrace,
  
  #[token("|")]
  Pipe,
  
  #[token("}")]
  RightBrace,

  #[token("&")]
  Compose,

  #[token("[]")]
  ComposeValue,
  
  #[token("[")]
  LeftBracket,
  
  #[token("]")]
  RightBracket,

  #[token("(")]
  LeftParen,

  #[token(")")]
  RightParen,

  #[token("~")]
  EmptyValue,

  #[token("<")]
  LeftAngle,

  #[token(">")]
  RightAngle,
  
  #[token(":")]
  Colon,
  
  #[token("*")]
  Star,
  
  #[token("+")]
  Plus,

  #[token("=")]
  Equals,

  #[token("!")]
  Bang,
  
  #[token("?")]
  Question,
  
  #[token(";")]
  Semi,
  
  #[token("@")]
  At,
  
  #[token("/")]
  Slash,

  #[token("^")]
  Caret,
  
  #[token("$")]
  Dollar,
  
  #[token("'")]
  Hint,
  
  #[token("_")]
  Sink,
  
  #[token("true")]
  True,
  
  #[token("false")]
  False,
  
  #[regex(r"\s+", filter_bs, priority = 2)]
  Whitespace,
  
  #[regex(r"[\r\n]+\s*|\s*[\r\n]+", logos::skip, priority = 3)]
  Blackspace,
  
  #[regex(r"\-?[0-9]+", parse_integer, priority = 2)]
  Integer(i64),
  
  #[regex(r"\-?[0-9]+\.[0-9]+", parse_float, priority = 3)]
  Float(f64),
  
  #[regex(r"\s*##([^#]|#[^#])*(##\s*)?", logos::skip, priority = 6)]
  #[regex(r"\s*#([^#][^\r\n]*)?\n?", logos::skip, priority = 5)]
  Comment,
  
  #[regex(r"\\\S", parse_escape, priority = 10)]
  #[regex(r"\\x[0-9a-fA-F][0-9a-fA-F]", parse_code_point_escape, priority = 7)]
  Escape(char),
  
  #[regex(r#""(""|[^"])*""#, parse_string_literal)]
  StringLiteral(RantString),
  
  #[regex(r#""(""|[^"])*"#)]
  UnterminatedStringLiteral,
}

fn parse_string_literal<'a>(lex: &mut Lexer<'a, RantToken>) -> Option<RantString> {
  let literal: String = lex.slice().to_owned();
  let literal_content = &literal[1..literal.len() - 1];
  let mut string_content = RantString::new();
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
  Some(string_content)
}

/// Filter function for whitespace lexer rule to exclude whitespace at start of source
fn filter_bs<'a>(lex: &mut Lexer<'a, RantToken>) -> Filter<()> {
  if lex.span().start > 0 {
    return Filter::Emit(())
  }
  Filter::Skip
}

fn parse_escape<'a>(lex: &mut Lexer<'a, RantToken>) -> Option<char> {
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

fn parse_code_point_escape<'a>(lex: &mut Lexer<'a, RantToken>) -> Option<char> {
  let codepoint = u8::from_str_radix(&lex.slice()[2..], 16).ok()?;
  Some(codepoint as char)
}

fn parse_float<'a>(lex: &mut Lexer<'a, RantToken>) -> Option<f64> {
  let slice = lex.slice();
  let n = slice.parse().ok()?;
  Some(n)
}

fn parse_integer<'a>(lex: &mut Lexer<'a, RantToken>) -> Option<i64> {
  let slice = lex.slice();
  let n = slice.parse().ok()?;
  Some(n)
}