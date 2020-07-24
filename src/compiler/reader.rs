use logos::*;
use super::lexer::RantToken;
use std::ops::Range;

pub struct RantTokenReader<'source> {
    lexer: Lexer<'source, RantToken>,
    peeked: Option<(RantToken, Range<usize>)>,
}

impl<'source> Iterator for RantTokenReader<'source> {
    type Item = (RantToken, Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        // Consume any peeked token before iterating lexer again
        self.peeked.take().or_else(|| self.lexer.next().map(|token| (token, self.lexer.span())))
    }
}

impl<'source> RantTokenReader<'source> {
    pub fn new(src: &'source str) -> Self {
        Self {
            lexer: RantToken::lexer(src),
            peeked: None,
        }
    }

    pub fn gen_last_token_string(&self) -> String {
        self.lexer.slice().to_owned()
    }

    /// Get the next non-whitespace token.
    pub fn next_solid(&mut self) -> Option<(RantToken, Range<usize>)> {
        loop {
            match self.lexer.next() {
                Some(RantToken::Whitespace) => continue,
                Some(token) => return Some((token, self.lexer.span())),
                None => return None
            }
        }
    }

    pub fn peek(&mut self) -> Option<&(RantToken, Range<usize>)> {
        // If a peek was already performed, return a reference to it
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        
        // If no previous peek was performed for the current iteration, iterate, store, and return reference to token
        let token = self.next();
        self.peeked = token;
        return self.peeked.as_ref();
    }
}