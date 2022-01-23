use logos::*;
use crate::InternalString;

// Module keywords
pub const KW_REQUIRE: &str = "require";

// Control flow keywords
pub const KW_RETURN: &str = "return";
pub const KW_BREAK: &str = "break";
pub const KW_CONTINUE: &str = "continue";
pub const KW_WEIGHT: &str = "weight";
pub const KW_IF: &str = "if";
pub const KW_ELSEIF: &str = "elseif";
pub const KW_ELSE: &str = "else";

// Value constant keywords
pub const KW_TRUE: &str = "true";
pub const KW_FALSE: &str = "false";

// Hinting keywords
pub const KW_TEXT: &str = "text";

// Output modifier keywords
pub const KW_EDIT: &str = "edit";

// Infix operator keywords
pub const KW_AND: &str = "and";
pub const KW_OR: &str = "or";
pub const KW_NEG: &str = "neg";
pub const KW_NOT: &str = "not";
pub const KW_NAND: &str = "nand";
pub const KW_NOR: &str = "nor";
pub const KW_XOR: &str = "xor";
pub const KW_EQ: &str = "eq";
pub const KW_NEQ: &str = "neq";
pub const KW_GT: &str = "gt";
pub const KW_GE: &str = "ge";
pub const KW_LT: &str = "lt";
pub const KW_LE: &str = "le";

pub fn is_valid_keyword_name(kw_name: &str) -> bool {
  matches!(kw_name, 
    KW_REQUIRE |
    KW_RETURN | KW_BREAK | KW_CONTINUE | KW_WEIGHT | KW_IF | KW_ELSEIF | KW_ELSE |
    KW_TRUE | KW_FALSE | KW_TEXT | KW_EDIT |
    KW_AND | KW_OR | KW_NEG | KW_NOT | KW_NAND | KW_NOR | KW_XOR | 
    KW_EQ | KW_NEQ | KW_GT | KW_GE | KW_LT | KW_LE
  )
}

#[derive(Debug, PartialEq)]
pub struct KeywordInfo {
  pub name: InternalString,
  pub is_valid: bool,
}

/// Represents the contents of a positive float literal token.
#[derive(Debug, PartialEq)]
pub enum PositiveFloatToken {
  Value(f64),
  OutOfRange,
}

/// Represents the contents of a positive integer literal token.
#[derive(Debug, PartialEq)]
pub enum PositiveIntegerToken {
  Value(u64),
  OutOfRange,
}

#[derive(Logos, Debug, PartialEq)]
pub enum RantToken {
  /// Sequence of printable non-whitespace characters that isn't a number
  /// This regex is so crazy because simply doing [\w\-_]+ would accidentally capture negative numbers
  #[error]
  #[regex(r"([0-9]+(\.[0-9]+([Ee][+\-]?\d+)?|[Ee][+\-]?\d+)?[\p{L}\-_]|[\w_][\p{L}\-_]|\-[\p{L}\-_])[\w\-_]*", priority = 1)]
  Fragment,

  /// Sequence of printable whitespace characters
  #[regex(r"\s+", filter_bs, priority = 2)]
  Whitespace,
  
  /// Sequence of non-printable whitespace characters
  #[regex(r"[\r\n]+\s*|\s*[\r\n]+", logos::skip, priority = 3)]
  Blackspace,

  #[token("-", priority = 10)]
  Minus,
  
  /// `{`
  #[token("{")]
  LeftBrace,
  
  /// `|`
  #[token("|")]
  VertBar,
  
  /// `}`
  #[token("}")]
  RightBrace,

  /// `|>`
  #[token("|>")]
  PipeOp,

  /// `[]`
  #[token("[]")]
  PipeValue,
  
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

  /// `<>`
  #[token("<>")]
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
  DoubleStar,
  
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
  Semicolon,
  
  /// `@`
  #[token("@", priority = 1)]
  At,

  /// Keyword, e.g. `@return`
  #[regex(r"@[a-z0-9_-]+", parse_keyword, priority = 2, ignore(case))]
  Keyword(KeywordInfo),
  
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
  
  /// ```
  #[token("`")]
  Hint,
  
  /// `~`
  #[token("~")]
  Sink,
  
  /// Unsigned integer literal
  #[regex(r"[0-9]+", parse_integer, priority = 2)]
  IntegerPositive(PositiveIntegerToken),
  
  /// Unsigned floating-point literal
  #[regex(r"[0-9]+(\.[0-9]+([Ee][+\-]?\d+)?|[Ee][+\-]?\d+)", parse_float, priority = 3)]
  FloatPositive(PositiveFloatToken),
  
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

fn parse_keyword(lex: &mut Lexer<RantToken>) -> KeywordInfo {
  let kwd_literal = lex.slice();
  let kwd_content = &kwd_literal[1..];
  KeywordInfo {
    is_valid: is_valid_keyword_name(kwd_content),
    name: InternalString::from(kwd_content),
  }
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

fn parse_float(lex: &mut Lexer<RantToken>) -> PositiveFloatToken {
  let slice = lex.slice();
  match slice.parse() {
    Ok(f) => PositiveFloatToken::Value(f),
    Err(_) => PositiveFloatToken::OutOfRange,
  }
}

fn parse_integer(lex: &mut Lexer<RantToken>) -> PositiveIntegerToken {
  let slice = lex.slice();
  match slice.parse() {
    Ok(i) => PositiveIntegerToken::Value(i),
    Err(_) => PositiveIntegerToken::OutOfRange,
  }
}