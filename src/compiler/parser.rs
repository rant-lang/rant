#![allow(dead_code)]

use super::{syntax::{RST, PrintFlag}, reader::RantTokenReader, lexer::RantToken, error::*};
use std::ops::Range;

type ParseResult<T> = Result<T, SyntaxError>;

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
    pub fn parse(&mut self) -> Result<RST, Vec<SyntaxError>> {
        match self.parse_sequence(SequenceParseMode::TopLevel) {
            Ok(..) if self.errors.len() > 0 => Err(self.errors.drain(..).collect()),
            Ok((rst, ..)) => Ok(rst),
            Err(hard_error) => Err(self.errors.drain(..).chain(std::iter::once(hard_error)).collect())
        }
    }

    fn soft_error(&mut self, error_type: SyntaxErrorType, span: &Range<usize>) {
        self.errors.push(SyntaxError::new(error_type, span.clone()));
    }

    /// Parses a sequence of items. Items are individual elements of a Rant program (fragments, blocks, function calls, etc.)
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

            // TODO: Queued whitespace should not carry between lines
            macro_rules! whitespace {
                (allow) => {
                    if let Some(ws) = pending_whitespace.take() {
                        sequence.push(RST::Whitespace(ws));
                    }
                };
                (queue $ws:expr) => {
                    pending_whitespace = Some($ws);
                };
                (ignore prev) => {
                    pending_whitespace = None;
                };
                (ignore next) => {
                    self.reader.skip_whitespace_tokens();
                };
                (ignore both) => {
                    whitespace!(ignore prev);
                    whitespace!(ignore next);
                };
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
                    whitespace!(ignore prev);
                    next_print_flag = PrintFlag::Sink;
                    continue
                }),

                // Block start
                RantToken::LeftBrace => {
                    // Read in the entire block
                    let block = self.parse_block(next_print_flag)?;

                    // Decide what to do with surrounding whitespace
                    match next_print_flag {                        
                        // If hinted, allow pending whitespace
                        PrintFlag::Hint => {
                            whitespace!(allow);
                            is_seq_printing = true;
                        },

                        // If sinked, hold pending whitespace and do nothing
                        PrintFlag::Sink => {},

                        // If no flag, take a hint
                        PrintFlag::None => {
                            // Inherit hints from inner blocks
                            if let RST::HintedBlock(_) = block {
                                whitespace!(allow);
                                is_seq_printing = true;
                            }
                        }
                    }
                    
                    sequence.push(block);                    
                },

                // Block element delimiter (when in block parsing mode)
                RantToken::Pipe => ban_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore prev);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return if sequence.len() > 0 {
                                Ok((RST::Sequence(sequence), SequenceEndType::BlockDelim, is_seq_printing))
                            } else {
                                Ok((RST::Nop, SequenceEndType::BlockDelim, is_seq_printing))
                            }
                        },
                        _ => unexpected_token_error!()
                    }
                }),

                // Block end (when in block parsing mode)
                RantToken::RightBrace => ban_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore prev);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return if sequence.len() > 0 {
                                Ok((RST::Sequence(sequence), SequenceEndType::BlockEnd, is_seq_printing))
                            } else {
                                Ok((RST::Nop, SequenceEndType::BlockEnd, is_seq_printing))
                            }
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

                // Integers
                RantToken::Integer(n) => ban_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    sequence.push(RST::Integer(n));
                }),

                // Floats
                RantToken::Float(n) => ban_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    sequence.push(RST::Float(n));
                }),

                // Treat unsupported sequence tokens as errors
                _ => {
                    unexpected_token_error!();
                }

            }

            // Clear flag
            next_print_flag = PrintFlag::None;
        }

        // This should only get hit for top-level sequences
        if sequence.len() > 0 {
            Ok((RST::Sequence(sequence), SequenceEndType::ProgramEnd, is_seq_printing))
        } else {
            Ok((RST::Nop, SequenceEndType::ProgramEnd, is_seq_printing))
        }
    }

    /// Parses a block.
    fn parse_block<'a>(&mut self, flag: PrintFlag) -> ParseResult<RST> {
        // Get position of starting brace for error reporting
        let start_index = self.reader.last_token_pos();

        let mut auto_hint = false;
        let mut sequences = vec![];

        loop {
            let (seq, seq_end, is_seq_printing) = self.parse_sequence(SequenceParseMode::BlockElement)?;
            auto_hint |= is_seq_printing;

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
        
        match flag {
            PrintFlag::None if auto_hint => Ok(RST::HintedBlock(sequences)),
            PrintFlag::Hint => Ok(RST::HintedBlock(sequences)),
            PrintFlag::Sink | _ => Ok(RST::Block(sequences)),
        }
    }
}