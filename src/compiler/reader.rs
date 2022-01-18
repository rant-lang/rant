use logos::*;
use super::lexer::RantToken;
use std::ops::Range;
use crate::InternalString;

pub struct RantTokenReader<'source> {
  lexer: Lexer<'source, RantToken>,
  peeked: Option<(RantToken, Range<usize>)>,
}

impl<'source> RantTokenReader<'source> {
  pub fn new(src: &'source str) -> Self {
    Self {
      lexer: RantToken::lexer(src),
      peeked: None,
    }
  }
  
  pub fn next(&mut self) -> Option<(RantToken, Range<usize>)> {
    // Consume any peeked token before iterating lexer again
    self.peeked.take().or_else(|| self.lexer.next().map(|token| (token, self.lexer.span())))
  }
  
  pub fn skip_one(&mut self) {
    self.next();
  }
  
  // Consumes the next token if it satisfies the predicate and returns a bool indicating whether any token was eaten.
  pub fn eat_where<F: FnOnce(Option<&(RantToken, Range<usize>)>) -> bool>(&mut self, predicate: F) -> bool {
    if predicate(self.peek()) {
      self.skip_one();
      return true
    }
    false
  }

  /// Consumes the next token if it's equal to the specified token and returns a bool indicating whether any token was eaten.
  pub fn eat(&mut self, token: RantToken) -> bool {
    if let Some((peeked, _span)) = self.peek() {
      if peeked.eq(&token) {
        self.skip_one();
        return true
      }
    }
    false
  }

  pub fn eat_kw(&mut self, kwname: &str) -> bool {
    self.eat_where(|t| {
      if let Some((RantToken::Keyword(kw), _)) = t.as_ref() {
        return kw.name.as_str() == kwname
      }
      false
    })
  }

  // Consumes the next token if it satisfies the predicate and returns it if the predicate was satisfied; otherwise, returns `None`.
  pub fn take_where<F: FnOnce(Option<&(RantToken, Range<usize>)>) -> bool>(&mut self, predicate: F) -> Option<(RantToken, Range<usize>)> {
    if predicate(self.peek()) {
      self.next()
    } else {
      None
    }
  }
  
  /// Gets the last token string that was read.
  pub fn last_token_string(&self) -> InternalString {
    InternalString::from(self.lexer.slice())
  }
  
  /// Gets the next non-whitespace token.
  pub fn next_solid(&mut self) -> Option<(RantToken, Range<usize>)> {
    loop {
      match self.next() {
        Some((RantToken::Whitespace, _)) => continue,
        Some((token, span)) => return Some((token, span)),
        None => return None
      }
    }
  }
  
  /// Skips past whitespace tokens.
  pub fn skip_ws(&mut self) {
    while let Some((RantToken::Whitespace, _)) = self.peek() {
      self.next();
    }
  }
  
  /// Gets the starting position of the most recently read token.
  pub fn last_token_pos(&self) -> usize {
    self.lexer.span().start
  }
  
  /// Gets the span of the most recently read token.
  pub fn last_token_span(&self) -> Range<usize> {
    self.lexer.span()
  }
  
  /// Returns a reference to the next token without consuming it.
  pub fn peek(&mut self) -> Option<&(RantToken, Range<usize>)> {
    // If a peek was already performed, return a reference to it
    if self.peeked.is_some() {
      return self.peeked.as_ref();
    }
    
    // If no previous peek was performed for the current iteration, iterate, store, and return reference to token
    let token = self.next();
    self.peeked = token;
    self.peeked.as_ref()
  }
}