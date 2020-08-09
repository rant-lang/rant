#![allow(dead_code)]

use super::{reader::RantTokenReader, lexer::RantToken, error::*};
use std::{rc::Rc, ops::Range, collections::HashSet};
use crate::{RantString, syntax::{PrintFlag, RST, Sequence, Block, VarAccessPath, FunctionCall, FunctionDef, VarAccessComponent, Identifier}};

type ParseResult<T> = Result<T, SyntaxError>;

enum SequenceParseMode {
    TopLevel,
    BlockElement,
    TagElement,
    FunctionBody
}

/// Tells what kind of token ended a sequence
enum SequenceEndType {
    ProgramEnd,
    BlockDelim,
    BlockEnd,
    TagEnd
}

/// Checks if an identifier (variable name, arg name, static map key) is valid
fn is_valid_ident(name: &str) -> bool {
    if name.is_empty() { return false }
    let mut has_non_digit = false;
    let is_valid_chars = name.chars().all(|c| {
        has_non_digit |= !c.is_ascii_digit();
        c.is_alphanumeric() || matches!(c, '_' | '-')
    });
    has_non_digit && is_valid_chars
}

/// Makes a range that encompasses both input ranges.
fn super_range(a: &Range<usize>, b: &Range<usize>) -> Range<usize> {
    if a.start < b.end {
        a.start..b.end
    } else if b.start < a.end {
        b.start..a.end
    } else {
        a.clone()
    }
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

// Note about parsing functions: 
// A return value of Ok(..) does not necessarily mean parsing was successful; program is only created if no soft errors are emitted. 
impl<'source> RantParser<'source> {
    pub fn parse(&mut self) -> Result<RST, Vec<SyntaxError>> {
        match self.parse_sequence(SequenceParseMode::TopLevel) {
            Ok(..) if !self.errors.is_empty() => Err(self.errors.drain(..).collect()),
            Ok((seq, ..)) => Ok(RST::Sequence(Rc::new(seq))),
            Err(hard_error) => Err(self.errors.drain(..).chain(std::iter::once(hard_error)).collect())
        }
    }

    fn soft_error(&mut self, error_type: SyntaxErrorType, span: &Range<usize>) {
        self.errors.push(SyntaxError::new(error_type, span.clone()));
    }

    /// Parses a sequence of items. Items are individual elements of a Rant program (fragments, blocks, function calls, etc.)
    fn parse_sequence(&mut self, mode: SequenceParseMode) -> ParseResult<(Sequence, SequenceEndType, bool)> {
        let mut sequence = Sequence::empty();
        let mut next_print_flag = PrintFlag::None;
        let mut last_print_flag_span: Option<Range<usize>> = None;
        let mut is_seq_printing = false;
        let mut pending_whitespace = None;

        while let Some((token, span)) = self.reader.next() {
            // Macro for prohibiting hints/sinks before certain tokens
            macro_rules! no_flags {
                (on $b:block) => {{
                    let elem = $b;
                    if !matches!(next_print_flag, PrintFlag::None) {
                        if let Some(flag_span) = last_print_flag_span.take() {
                            self.soft_error(match next_print_flag {
                                PrintFlag::Hint => SyntaxErrorType::InvalidHintOn(elem.display_name()),
                                PrintFlag::Sink => SyntaxErrorType::InvalidSinkOn(elem.display_name()),
                                PrintFlag::None => unreachable!()
                            }, &flag_span)
                        }
                    }
                    sequence.push(Rc::new(elem));
                }};
                ($b:block) => {
                    if matches!(next_print_flag, PrintFlag::None) {
                        $b
                    } else if let Some(flag_span) = last_print_flag_span.take() {
                        self.soft_error(match next_print_flag {
                            PrintFlag::Hint => SyntaxErrorType::InvalidHint,
                            PrintFlag::Sink => SyntaxErrorType::InvalidSink,
                            PrintFlag::None => unreachable!()
                        }, &flag_span)
                    }
                };
            }

            macro_rules! seq_add {
                ($elem:expr) => {
                    sequence.push(Rc::new($elem));
                }
            }
            
            // Shortcut macro for "unexpected token" error
            macro_rules! unexpected_token_error {
                () => {
                    self.soft_error(SyntaxErrorType::UnexpectedToken(self.reader.last_token_string().to_string()), &span)
                };
            }

            // TODO: Queued whitespace should not carry between lines
            macro_rules! whitespace {
                (allow) => {
                    if let Some(ws) = pending_whitespace.take() {
                        seq_add!(RST::Whitespace(ws));
                    }
                };
                (queue $ws:expr) => {
                    pending_whitespace = Some($ws);
                };
                (ignore prev) => {
                    pending_whitespace = None;
                };
                (ignore next) => {
                    self.reader.skip_ws();
                };
                (ignore both) => {
                    whitespace!(ignore prev);
                    whitespace!(ignore next);
                };
            }

            // Parse next sequence item
            match token {

                // Hint
                RantToken::Hint => no_flags!({
                    whitespace!(allow);
                    is_seq_printing = true;
                    next_print_flag = PrintFlag::Hint;
                    last_print_flag_span = Some(span.clone());
                    continue
                }),

                // Sink
                RantToken::Sink => no_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore prev);
                    next_print_flag = PrintFlag::Sink;
                    last_print_flag_span = Some(span.clone());
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
                            if let Block { flag: PrintFlag::Hint, ..} = block {
                                whitespace!(allow);
                                is_seq_printing = true;
                            }
                        }
                    }
                    
                    seq_add!(RST::Block(block));               
                },

                // Block element delimiter (when in block parsing mode)
                RantToken::Pipe => no_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore prev);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return Ok((sequence, SequenceEndType::BlockDelim, is_seq_printing))
                        },
                        _ => unexpected_token_error!()
                    }
                }),

                // Block end (when in block parsing mode)
                RantToken::RightBrace => no_flags!({
                    // Ignore pending whitespace
                    whitespace!(ignore prev);
                    match mode {
                        SequenceParseMode::BlockElement => {
                            return Ok((sequence, SequenceEndType::BlockEnd, is_seq_printing))
                        },
                        _ => unexpected_token_error!()
                    }
                }),

                // Function creation or call
                RantToken::LeftBracket => {
                    let func_access = self.parse_func_access(next_print_flag)?;

                    // Handle hint/sink behavior
                    match func_access {
                        RST::FunctionCall(FunctionCall { flag, ..}) => {
                            // If the call is hinted, allow whitespace around it
                            if matches!(flag, PrintFlag::Hint) {
                                whitespace!(allow);
                            }
                        },
                        // Definitions are implicitly sinked and ignore surrounding whitespace
                        RST::FunctionDef(_) => {
                            whitespace!(ignore both);
                        },
                        // Do nothing if it's an unsupported node type, e.g. NOP
                        _ => {}
                    }

                    seq_add!(func_access);
                },

                // Fragment
                RantToken::Fragment => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    let frag = self.reader.last_token_string();
                    RST::Fragment(frag)
                }),

                // Whitespace (only if sequence isn't empty)
                RantToken::Whitespace => no_flags!({
                    // Don't set is_printing here; whitespace tokens always appear with other printing tokens
                    if !sequence.is_empty() {
                        let ws = self.reader.last_token_string();
                        whitespace!(queue ws);
                    }
                }),

                // Escape sequences
                // TODO: Combine these with adjacent fragments somehow
                RantToken::Escape(ch) => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    let mut s = RantString::new();
                    s.push(ch);
                    RST::Fragment(s)
                }),

                // Integers
                RantToken::Integer(n) => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    RST::Integer(n)
                }),

                // Floats
                RantToken::Float(n) => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    RST::Float(n)
                }),

                // True
                RantToken::True => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    RST::Boolean(true)
                }),

                // False
                RantToken::False => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    RST::Boolean(false)
                }),

                // Verbatim string literals
                RantToken::StringLiteral(s) => no_flags!(on {
                    whitespace!(allow);
                    is_seq_printing = true;
                    RST::Fragment(s)
                }),

                // Handle unclosed string literals as hard errors
                RantToken::UnterminatedStringLiteral => return Err(SyntaxError::new(SyntaxErrorType::UnclosedStringLiteral, span)),

                // Treat unsupported sequence tokens as errors
                _ => {
                    unexpected_token_error!();
                }

            }

            // Clear flag
            next_print_flag = PrintFlag::None;
        }

        // Reached when the whole program has been read
        // This should only get hit for top-level sequences

        // Make sure there are no dangling printflags
        match next_print_flag {
            PrintFlag::None => {},
            PrintFlag::Hint => {
                if let Some(flag_span) = last_print_flag_span.take() {
                    self.soft_error(SyntaxErrorType::InvalidHint, &flag_span);
                }
            },
            PrintFlag::Sink => {
                if let Some(flag_span) = last_print_flag_span.take() {
                    self.soft_error(SyntaxErrorType::InvalidSink, &flag_span);
                }
            }
        }

        // Return the top-level sequence
        Ok((sequence, SequenceEndType::ProgramEnd, is_seq_printing))
    }

    /// Parses a function definition, anonymous function, or function call.
    fn parse_func_access(&mut self, flag: PrintFlag) -> ParseResult<RST> {
        let start_span = self.reader.last_token_span();
        self.reader.skip_ws();
        // Check if we're defining a function: [$func-name] { ... }
        if self.reader.take_where(|t| matches!(t, Some((RantToken::Dollar, ..)))) {
            // Function definition

            // Name of variable function will be stored in
            let func_name = self.parse_var_access_path()?;
            // List of parameter names for function
            let mut params = vec![];
            // Separate set of all parameter names to check for duplicates
            let mut params_set = HashSet::new();
            // Has variadic param '*' been encountered?
            let mut is_variadic = false;
            // Stores range info for malformed variadic signature error
            let mut nonterm_variadic_err_span = None;

            // At this point there should either be ':' or ']'
            match self.reader.next_solid() {
                // ':' means there are params to be read
                Some((RantToken::Colon, _)) => {
                    // Read the params
                    'read_params: loop {
                        match self.reader.next_solid() {
                            // Regular parameter
                            Some((RantToken::Fragment, span)) => {
                                // Make sure the parameter is in a valid position
                                if let Some((a, _)) = nonterm_variadic_err_span {
                                    nonterm_variadic_err_span = Some((a, span));
                                } else {
                                    // We only care about verifying/recording the param if it's in a valid position
                                    let param_name = Identifier::new(self.reader.last_token_string());
                                    // Make sure it's a valid identifier
                                    if !is_valid_ident(param_name.as_str()) {
                                        self.soft_error(SyntaxErrorType::InvalidIdentifier(param_name.to_string()), &span)
                                    }
                                    // Check for duplicates
                                    // I'd much prefer to store references in params_set, but that's way more annoying to deal with
                                    if !params_set.insert(param_name.clone()) {
                                        self.soft_error(SyntaxErrorType::DuplicateParameter(param_name.to_string()), &span);
                                    }
                                    // Put it in the list
                                    params.push(param_name);
                                }
                                
                                // Check if there are more params or if the signature is done
                                match self.reader.next_solid() {
                                    // ';' means there are more params
                                    Some((RantToken::Semicolon, ..)) => {
                                        continue 'read_params
                                    },
                                    // ']' means end of signature
                                    Some((RantToken::RightBracket, ..)) => {
                                        break 'read_params
                                    },
                                    // Emit a hard error on anything else
                                    Some((_, span)) => {
                                        return Err(SyntaxError::new(SyntaxErrorType::UnexpectedToken(self.reader.last_token_string().to_string()), span))
                                    },
                                    None => {
                                        return Err(SyntaxError::new(SyntaxErrorType::UnclosedFunctionSignature, start_span))
                                    },
                                }
                            },
                            // Variadic parameter
                            Some((RantToken::Star, _)) => {
                                is_variadic = true;
                                // At this point the signature should be done (variadic comes last)
                                // However we can check for more params to report them as an error
                                match self.reader.next_solid() {
                                    // ']' means end of signature
                                    Some((RantToken::RightBracket, ..)) => {
                                        break 'read_params
                                    },
                                    // ';' means more params after variadic (emit soft error after finished)
                                    Some((RantToken::Semicolon, ..)) => {
                                        nonterm_variadic_err_span = match nonterm_variadic_err_span {
                                            Some((a, _)) => Some((a, self.reader.last_token_span())),
                                            None => Some((self.reader.last_token_span(), self.reader.last_token_span()))
                                        };
                                    },
                                    // Emit a hard error on anything else
                                    Some((_, span)) => {
                                        return Err(SyntaxError::new(SyntaxErrorType::UnexpectedToken(self.reader.last_token_string().to_string()), span))
                                    },  
                                    None => {
                                        return Err(SyntaxError::new(SyntaxErrorType::UnclosedFunctionSignature, start_span))
                                    }
                                }
                            },
                            // Error on early close
                            Some((RantToken::RightBracket, span)) => {
                                self.soft_error(SyntaxErrorType::MissingIdentifier, &span);
                                break 'read_params
                            },
                            // Error on anything else
                            Some((.., span)) => {
                                self.soft_error(SyntaxErrorType::InvalidIdentifier(self.reader.last_token_string().to_string()), &span)
                            },
                            None => {
                                return Err(SyntaxError::new(SyntaxErrorType::UnclosedFunctionSignature, start_span));
                            }
                        }
                    }

                    // If it's a malformed variadic, emit a soft error
                    if let Some((a, b)) = nonterm_variadic_err_span.as_ref() {
                        self.soft_error(SyntaxErrorType::ParamsAfterVariadic, &super_range(a, b));
                    }
                },
                // ']' means there are no params-- fall through to the next step
                Some((RantToken::RightBrace, span)) => {},
                // Something weird is here, emit a hard error
                Some((.., span)) => {
                    return Err(SyntaxError::new(SyntaxErrorType::UnexpectedToken(self.reader.last_token_string().to_string()), span))
                },
                None => return Err(SyntaxError::new(SyntaxErrorType::UnclosedFunctionSignature, start_span))
            }

            // Read function body
            match self.reader.next_solid() {
                Some((RantToken::LeftBrace, span)) => {
                    // TODO: Handle captured variables in function bodies
                    let body = Rc::new(self.parse_block(PrintFlag::None)?);


                    todo!()
                },
                // If something that isn't a block is there, emit a soft error and return a NOP
                Some((other, span)) => {
                    self.soft_error(SyntaxErrorType::ExpectedToken("{".to_owned()), &span);
                    Ok(RST::Nop)
                },
                // Hard error on EOF
                None => {
                    Err(SyntaxError::new(SyntaxErrorType::ExpectedToken("{".to_owned()), self.reader.last_token_span()))
                }
            }
        } else {
            // Function call
            let sinked = flag == PrintFlag::Sink;
            let func_name = self.parse_var_access_path()?;
            todo!()
        }
    }

    /// Parses a variable access path.
    fn parse_var_access_path(&mut self) -> ParseResult<VarAccessPath> {
        let mut idparts = vec![];
        let preceding_span = self.reader.last_token_span();
        let first_part = self.reader.next_solid();

        // Parse the first part of the path
        match first_part {
            // The first part of the path may only be a variable name
            Some((RantToken::Fragment, span)) => {
                let varname = Identifier::new(self.reader.last_token_string());
                if is_valid_ident(varname.as_str()) {
                    idparts.push(VarAccessComponent::Name(varname));
                } else {
                    self.soft_error(SyntaxErrorType::InvalidIdentifier(varname.to_string()), &span);
                }
            },
            Some((RantToken::Integer(_), span)) => {
                self.soft_error(SyntaxErrorType::LocalPathStartsWithIndex, &span);
            },
            Some((.., span)) => {
                self.soft_error(SyntaxErrorType::MissingIdentifier, &span);
            },
            None => return Err(SyntaxError::new(SyntaxErrorType::MissingIdentifier, preceding_span))
        }

        // Parse the rest of the path
        loop {
            // We expect a '/' between each component, so check for that first.
            // If it's anything else, terminate the path and return it.
            self.reader.skip_ws();
            if self.reader.take_where(|t| matches!(t, Some((RantToken::Slash, ..)))) {
                // From here we expect to see either another key (fragment) or index (integer).
                // If it's anything else, return a syntax error.
                let component = self.reader.next_solid();
                match component {
                    // Key
                    Some((RantToken::Fragment, span)) => {
                        let varname = Identifier::new(self.reader.last_token_string());
                        if is_valid_ident(varname.as_str()) {
                            idparts.push(VarAccessComponent::Name(varname));
                        } else {
                            self.soft_error(SyntaxErrorType::InvalidIdentifier(varname.to_string()), &span);
                        }
                    },
                    // Index
                    Some((RantToken::Integer(index), span)) => {
                        idparts.push(VarAccessComponent::Index(index));
                    },
                    Some((.., span)) => {
                        // error
                        self.soft_error(SyntaxErrorType::InvalidIdentifier(self.reader.last_token_string().to_string()), &span);
                    },
                    None => return Err(SyntaxError::new(SyntaxErrorType::MissingIdentifier, self.reader.last_token_span()))
                }
            } else {
                return Ok(VarAccessPath::new(idparts))
            }
        }
    }

    /// Parses a block.
    fn parse_block(&mut self, flag: PrintFlag) -> ParseResult<Block> {
        // Get position of starting brace for error reporting
        let start_index = self.reader.last_token_pos();
        // Keeps track of inherited hinting
        let mut auto_hint = false;
        // Block content
        let mut sequences = vec![];

        loop {
            let (seq, seq_end, is_seq_printing) = self.parse_sequence(SequenceParseMode::BlockElement)?;
            auto_hint |= is_seq_printing;

            match seq_end {
                SequenceEndType::BlockDelim => {
                    sequences.push(Rc::new(seq));
                },
                SequenceEndType::BlockEnd => {
                    sequences.push(Rc::new(seq));
                    break;
                },
                SequenceEndType::ProgramEnd => {
                    // Hard error if block isn't closed
                    return Err(SyntaxError::new(SyntaxErrorType::UnclosedBlock, start_index .. self.source.len()))
                },
                _ => unreachable!()
            }
        }

        // Figure out the printflag before returning the block
        if auto_hint && flag != PrintFlag::Sink {
            Ok(Block::new(PrintFlag::Hint, sequences))
        } else {
            Ok(Block::new(flag, sequences))
        }
    }
}