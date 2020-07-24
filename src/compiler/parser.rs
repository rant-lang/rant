#![allow(dead_code)]

use super::{syntax::RST, reader::RantTokenReader, lexer::RantToken};
use std::ops::Range;

pub type ParseResult<T> = Result<T, SyntaxError>;

/// Describes the location and nature of a syntax error.
pub struct SyntaxError {
    span: Range<usize>,
    info: SyntaxErrorType
}

impl SyntaxError {
    pub(crate) fn new(info: SyntaxErrorType, span: Range<usize>) -> Self {
        Self {
            info,
            span
        }
    }
}

/// The information describing a syntax error as seen by the parser.
pub enum SyntaxErrorType {
    UnclosedBlock,
    ExpectedToken(String),
    UnexpectedToken(String),
    InvalidSink,
    InvalidHint,
}

#[repr(u8)]
enum PrintFlag {
    None,
    Hint,
    Sink
}

enum SequenceParseMode {
    TopLevel,
    BlockElement,
    TagElement
}

enum SequenceEndType {
    ProgramEnd,
    BlockDelim,
    BlockEnd,
    TagEnd
}

/// A parser that turns Rant code into an RST (Rant Syntax Tree).
pub struct RantParser<'source> {
    source: &'source str,
    errors: Vec<SyntaxError>,
    // warnings: Vec<CompilerWarning>
    reader: RantTokenReader<'source>
}

impl<'source> RantParser<'source> {
    pub fn new(source: &'source str) -> Self {
        let reader = RantTokenReader::new(source);
        let errors = vec![];
        Self {
            source,
            errors,
            reader
        }
    }
}

impl<'source> RantParser<'source> {
    pub fn parse(&mut self) -> ParseResult<RST> {
        Ok(self.parse_sequence(SequenceParseMode::TopLevel)?.0)
    }

    fn soft_error(&mut self, error_type: SyntaxErrorType, span: &Range<usize>) {
        self.errors.push(SyntaxError::new(error_type, span.clone()));
    }

    fn parse_sequence<'a>(&mut self, mode: SequenceParseMode) -> ParseResult<(RST, SequenceEndType, bool)> {
        let mut sequence = vec![];
        let mut next_print_flag = PrintFlag::None;
        let mut is_seq_printing = false;
        let mut pending_whitespace = None;

        while let Some((token, span)) = self.reader.next() {
            // Macro for prohibiting hints/sinks before certain tokens
            macro_rules! ban_flags {
                ($b:block) => {
                    match next_print_flag {
                        PrintFlag::None => $b,
                        other => self.soft_error(match other {
                            PrintFlag::Hint => SyntaxErrorType::InvalidHint,
                            PrintFlag::Sink => SyntaxErrorType::InvalidSink,
                            PrintFlag::None => unreachable!()
                        }, &span)
                    }
                };
            }
            
            // Shortcut macro for "unexpected token" error
            macro_rules! unexpected_token_error {
                () => {
                    self.soft_error(SyntaxErrorType::UnexpectedToken(self.reader.gen_last_token_string()), &span)
                };
            }

            macro_rules! whitespace {
                (allow) => {
                    if let Some(ws) = pending_whitespace.take() {
                        sequence.push(RST::Whitespace(ws));
                    }
                };
                (ignore) => {
                    pending_whitespace = None;
                };
                (queue $ws:expr) => {
                    pending_whitespace = Some($ws);
                }
            }

            // Parse next sequence item
            match token {
                // Hint
                RantToken::Hint => ban_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    next_print_flag = PrintFlag::Hint;
                    continue
                }),
                // Sink
                RantToken::Sink => ban_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore);
                    next_print_flag = PrintFlag::Sink;
                    continue
                }),
                // Block
                RantToken::LeftBrace => {
                    let block = self.parse_block(next_print_flag, span.start)?;
                    // Inherit hints from inner blocks
                    if let RST::HintedBlock(_) = block {
                        whitespace!(allow);
                        is_seq_printing = true;
                    }
                    sequence.push(block);
                    
                },
                // Block element delimiter (when in block parsing mode)
                RantToken::Pipe => ban_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return Ok((RST::Sequence(sequence), SequenceEndType::BlockDelim, is_seq_printing))
                        },
                        _ => unexpected_token_error!()
                    }
                }),
                // Block end (when in block parsing mode)
                RantToken::RightBrace => ban_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return Ok((RST::Sequence(sequence), SequenceEndType::BlockEnd, is_seq_printing))
                        },
                        _ => unexpected_token_error!()
                    }
                }),
                // Fragment
                RantToken::Fragment => ban_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    let frag = self.reader.gen_last_token_string();
                    sequence.push(RST::Fragment(frag));
                }),
                // Whitespace (only if sequence isn't empty)
                RantToken::Whitespace if sequence.len() > 0 => ban_flags!({
                    // Don't set is_printing here; whitespace tokens always appear with other printing tokens
                    let ws = self.reader.gen_last_token_string();
                    whitespace!(queue ws);
                }),
                // Escape sequences
                // TODO: Combine these with adjacent fragments somehow
                RantToken::Escape(ch) => ban_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    sequence.push(RST::Fragment(ch.to_string()));
                }),
                // ???
                _ => {
                    unexpected_token_error!();
                }
            }

            // Clear hint/sink
            next_print_flag = PrintFlag::None;
        }

        // This should only get hit for top-level blocks
        if sequence.len() > 0 {
            Ok((RST::Sequence(sequence), SequenceEndType::ProgramEnd, is_seq_printing))
        } else {
            Ok((RST::Nop, SequenceEndType::ProgramEnd, is_seq_printing))
        }
    }

    fn parse_block<'a>(&mut self, print: PrintFlag, start_index: usize) -> ParseResult<RST> {
        let mut hinted = match print {
            PrintFlag::Hint => true,
            _ => false
        };

        let mut sequences = vec![];
        loop {
            let (seq, seq_end, is_seq_printing) = self.parse_sequence(SequenceParseMode::BlockElement)?;
            hinted |= is_seq_printing;

            match seq_end {
                SequenceEndType::BlockDelim => {
                    sequences.push(seq);
                },
                SequenceEndType::BlockEnd => {
                    sequences.push(seq);
                    break;
                },
                SequenceEndType::ProgramEnd => {
                    // Hard error if block isn't closed
                    return Err(SyntaxError::new(SyntaxErrorType::UnclosedBlock, start_index .. self.source.len()))
                },
                _ => unreachable!()
            }
        }
        
        if hinted {
            Ok(RST::HintedBlock(sequences))
        } else {
            Ok(RST::Block(sequences))
        }
    }
}