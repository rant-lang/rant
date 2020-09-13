#![allow(dead_code)]

use super::{reader::RantTokenReader, lexer::RantToken, message::*, Problem, Reporter};
use std::{rc::Rc, ops::Range, collections::HashSet};
use crate::{RantString, lang::*};
use line_col::LineColLookup;


type ParseResult<T> = Result<T, ()>;

enum SequenceParseMode {
  /// Parse a sequence like a top-level program.
  ///
  /// Breaks on EOF.
  TopLevel,
  /// Parse a sequence like a block element.
  ///
  /// Breaks on `Pipe` and `RightBrace`.
  BlockElement,
  /// Parse a sequence like a tag (function access) element.
  ///
  /// Breaks on `Semi` and `RightBracket`.
  FunctionArg,
  /// Parse a sequence like a function body.
  ///
  /// Breaks on `RightBrace`.
  FunctionBody,
  /// Parse a sequence like a dynamic key expression.
  ///
  /// Breaks on `RightBrace`.
  DynamicKey,
  /// Parse a sequence like an anonymous function expression.
  ///
  /// Breaks on `Colon` and `RightBracket`.
  AnonFunctionExpr,
  /// Parse a sequence like a variable assignment value.
  ///
  /// Breaks on `RightAngle` and `Semi`.
  VariableAssignment,
  /// Parse a sequence like a collection initializer element.
  ///
  /// Breaks on `Semi` and `RightParen`.
  CollectionInit,
}

/// What type of collection initializer to parse?
enum CollectionInitKind {
  /// Parse a list
  List,
  /// Parse a map
  Map
}

/// Indicates what kind of token terminated a sequence read.
enum SequenceEndType {
  /// Top-level program sequence was terminated by end-of-file.
  ProgramEnd,
  /// Block element sequence was terminated by `Pipe`.
  BlockDelim,
  /// Block element sequence was terminated by `RightBrace`.
  BlockEnd,
  /// Function argument sequence was terminated by `Semi`.
  FunctionArgEndNext,
  /// Function argument sequence was terminated by `RightBracket`.
  FunctionArgEndBreak,
  /// Function body sequence was terminated by `RightBrace`.
  FunctionBodyEnd,
  /// Dynamic key sequencce was terminated by `RightBrace`.
  DynamicKeyEnd,
  /// Anonymous function expression was terminated by `Colon`.
  AnonFunctionExprToArgs,
  /// Anonymous function expression was terminated by `RightBracket` and does not expect arguments.
  AnonFuncctionExprNoArgs,
  /// Variable accessor was terminated by `RightAngle`.
  VariableAccessEnd,
  /// Variable assignment expression was terminated by `Semi`. 
  VariableAssignDelim,
  /// Collection initializer was terminated by `RightParen`.
  CollectionInitEnd,
  /// Collection initializer was termianted by `Semi`.
  CollectionInitDelim,
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
#[inline]
fn super_range(a: &Range<usize>, b: &Range<usize>) -> Range<usize> {
  a.start.min(b.start)..a.end.max(b.end)
}

/// A parser that turns Rant code into an RST (Rant Syntax Tree).
pub struct RantParser<'source, 'report, R: Reporter> {
  source: &'source str,
  has_errors: bool,
  reader: RantTokenReader<'source>,
  lookup: LineColLookup<'source>,
  reporter: &'report mut R,
  debug_enabled: bool,
}

impl<'source, 'report, R: Reporter> RantParser<'source, 'report, R> {
  pub fn new(source: &'source str, reporter: &'report mut R, debug_enabled: bool) -> Self {
    let reader = RantTokenReader::new(source);
    let lookup = LineColLookup::new(source);
    Self {
      source,
      has_errors: false,
      reader,
      lookup,
      reporter,
      debug_enabled,
    }
  }
}

impl<'source, 'report, R: Reporter> RantParser<'source, 'report, R> {
  /// Top-level parsing function invoked by the compiler.
  pub fn parse(&mut self) -> Result<RST, ()> {
    let result = self.parse_sequence(SequenceParseMode::TopLevel);
    match result {
      // Err if parsing "succeeded" but there are soft syntax errors
      Ok(..) if self.has_errors => Err(()),
      // Ok if parsing succeeded and there are no syntax errors
      Ok((seq, ..)) => Ok(RST::Sequence(Rc::new(seq))),
      // Err on hard syntax error
      Err(()) => Err(())
    }
  }
  
  /// Reports a syntax error, allowing parsing to continue but causing the final compilation to fail. 
  fn syntax_error(&mut self, error_type: Problem, span: &Range<usize>) {
    let (line, col) = self.lookup.get(span.start);
    self.has_errors = true;
    self.reporter.report(CompilerMessage::new(error_type, Severity::Error, Some(Position::new(line, col, span.clone()))));
  }
  
  /// Emits an "unexpected token" error for the most recently read token.
  #[inline]
  fn unexpected_last_token_error(&mut self) {
    self.syntax_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &self.reader.last_token_span())
  }
  
  /// Parses a sequence of items. Items are individual elements of a Rant program (fragments, blocks, function calls, etc.)
  fn parse_sequence(&mut self, mode: SequenceParseMode) -> ParseResult<(Sequence, SequenceEndType, bool)> {
    let mut sequence = Sequence::empty();
    let mut next_print_flag = PrintFlag::None;
    let mut last_print_flag_span: Option<Range<usize>> = None;
    let mut is_seq_printing = false;
    let mut pending_whitespace = None;
    let debug = self.debug_enabled;
    
    while let Some((token, span)) = self.reader.next() {
      macro_rules! inject_debug_info {
        () => {
          if debug {
            let (line, col) = self.lookup.get(span.start);
            sequence.push(Rc::new(RST::DebugInfoUpdateOuter(DebugInfo::Location { line, col })));
          }
        }
      }
      
      // Macro for prohibiting hints/sinks before certain tokens
      macro_rules! no_flags {
        (on $b:block) => {{
          let elem = $b;
          if !matches!(next_print_flag, PrintFlag::None) {
            if let Some(flag_span) = last_print_flag_span.take() {
              self.syntax_error(match next_print_flag {
                PrintFlag::Hint => Problem::InvalidHintOn(elem.display_name()),
                PrintFlag::Sink => Problem::InvalidSinkOn(elem.display_name()),
                PrintFlag::None => unreachable!()
              }, &flag_span)
            }
          }
          inject_debug_info!();
          sequence.push(Rc::new(elem));
        }};
        ($b:block) => {
          if matches!(next_print_flag, PrintFlag::None) {
            $b
          } else if let Some(flag_span) = last_print_flag_span.take() {
            self.syntax_error(match next_print_flag {
              PrintFlag::Hint => Problem::InvalidHint,
              PrintFlag::Sink => Problem::InvalidSink,
              PrintFlag::None => unreachable!()
            }, &flag_span)
          }
        };
      }
      
      macro_rules! seq_add {
        ($elem:expr) => {{
          inject_debug_info!();
          sequence.push(Rc::new($elem));
        }}
      }
      
      // Shortcut macro for "unexpected token" error
      macro_rules! unexpected_token_error {
        () => {
          self.syntax_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span)
        };
      }
      
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
        
        // Defer operator
        RantToken::Star => {
          self.reader.skip_ws();
          let block = self.parse_block(true, next_print_flag)?;
          
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
          
          seq_add!(RST::BlockValue(Rc::new(block)));
        },
        
        // Block start
        RantToken::LeftBrace => {
          // Read in the entire block
          let block = self.parse_block(false, next_print_flag)?;
          
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
              return Ok((sequence.with_name_str("block element"), SequenceEndType::BlockDelim, is_seq_printing))
            },
            SequenceParseMode::DynamicKey => {
              self.syntax_error(Problem::DynamicKeyBlockMultiElement, &span);
            },
            SequenceParseMode::FunctionBody => {
              self.syntax_error(Problem::FunctionBodyBlockMultiElement, &span);
            },
            _ => unexpected_token_error!()
          }
        }),
        
        // Block/func body/dynamic key end
        RantToken::RightBrace => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          match mode {
            SequenceParseMode::BlockElement => {
              return Ok((sequence.with_name_str("block element"), SequenceEndType::BlockEnd, is_seq_printing))
            },
            SequenceParseMode::FunctionBody => {
              return Ok((sequence.with_name_str("function body"), SequenceEndType::FunctionBodyEnd, true))
            },
            SequenceParseMode::DynamicKey => {
              return Ok((sequence.with_name_str("dynamic key"), SequenceEndType::DynamicKeyEnd, true))
            }
            _ => unexpected_token_error!()
          }
        }),
        
        // Map initializer
        RantToken::At => no_flags!(on {
          match self.reader.next_solid() {
            Some((RantToken::LeftParen, _)) => {
              self.parse_collection_initializer(CollectionInitKind::Map, &span)?
            },
            _ => {
              self.syntax_error(Problem::ExpectedToken("(".to_owned()), &self.reader.last_token_span());
              RST::EmptyVal
            },
          }
        }),
        
        // List initializer
        RantToken::LeftParen => no_flags!(on {
          self.parse_collection_initializer(CollectionInitKind::List, &span)?
        }),
        
        // Collection init termination
        RantToken::RightParen => no_flags!({
          match mode {
            SequenceParseMode::CollectionInit => {
              return Ok((sequence, SequenceEndType::CollectionInitEnd, true))
            },
            _ => unexpected_token_error!()
          }
        }),
        
        // Function creation or call
        RantToken::LeftBracket => {
          let func_access = self.parse_func_access(next_print_flag)?;
          
          // Handle hint/sink behavior
          match func_access {
            RST::FuncCall(FunctionCall { flag, ..}) => {
              // If the call is hinted, allow whitespace around it
              if matches!(flag, PrintFlag::Hint) {
                whitespace!(allow);
              }
            },
            // Definitions are implicitly sinked and ignore surrounding whitespace
            RST::FuncDef(_) => {
              whitespace!(ignore both);
            },
            // Do nothing if it's an unsupported node type, e.g. NOP
            _ => {}
          }
          
          seq_add!(func_access);
        },
        
        // Can be terminator for function args and anonymous function expressions
        RantToken::RightBracket => no_flags!({
          match mode {
            SequenceParseMode::AnonFunctionExpr => return Ok((sequence, SequenceEndType::AnonFuncctionExprNoArgs, true)),
            SequenceParseMode::FunctionArg => return Ok((sequence, SequenceEndType::FunctionArgEndBreak, true)),
            _ => unexpected_token_error!()
          }
        }),
        
        // Variable access start
        RantToken::LeftAngle => no_flags!({
          let accessors = self.parse_accessor()?;
          for accessor in accessors {
            match accessor {
              RST::VarGet(..) => {
                is_seq_printing = true;
                whitespace!(allow);
              },
              RST::VarSet(..) | RST::VarDef(..) => {
                // whitespace!(ignore both);
              },
              _ => unreachable!()
            }
            seq_add!(accessor);
          }
        }),
        
        // Variable access end
        RantToken::RightAngle => no_flags!({
          match mode {
            SequenceParseMode::VariableAssignment => return Ok((sequence, SequenceEndType::VariableAccessEnd, true)),
            _ => unexpected_token_error!()
          }
        }),
        
        // These symbols are only used in special contexts and can be safely printed
        RantToken::Bang | RantToken::Question | RantToken::Slash | RantToken::Plus | RantToken::Dollar | RantToken::Equals
        => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          let frag = self.reader.last_token_string();
          RST::Fragment(frag)
        }),
        
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
          if is_seq_printing {
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
        
        // None
        RantToken::NoneValue => no_flags!(on {
          RST::EmptyVal
        }),
        
        // Verbatim string literals
        RantToken::StringLiteral(s) => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          RST::Fragment(s)
        }),
        
        // Colon can be either fragment or argument separator.
        RantToken::Colon => no_flags!({
          match mode {
            SequenceParseMode::AnonFunctionExpr => return Ok((sequence.with_name_str("anonymous function expression"), SequenceEndType::AnonFunctionExprToArgs, true)),
            _ => seq_add!(RST::Fragment(RantString::from(":")))
          }
        }),
        
        // Semicolon can be a fragment, collection element separator, or argument separator.
        RantToken::Semi => no_flags!({
          match mode {
            // If we're inside a function argument, terminate the sequence
            SequenceParseMode::FunctionArg => return Ok((sequence.with_name_str("argument"), SequenceEndType::FunctionArgEndNext, true)),
            // Collection initializer
            SequenceParseMode::CollectionInit => return Ok((sequence.with_name_str("collection item"), SequenceEndType::CollectionInitDelim, true)),
            // Variable assignment expression
            SequenceParseMode::VariableAssignment => return Ok((sequence.with_name_str("variable assignment"), SequenceEndType::VariableAssignDelim, true)),
            // If we're anywhere else, just print the semicolon like normal text
            _ => seq_add!(RST::Fragment(RantString::from(";")))
          }
        }),
        
        // Handle unclosed string literals as hard errors
        RantToken::UnterminatedStringLiteral => {
          self.syntax_error(Problem::UnclosedStringLiteral, &span); 
          return Err(())
        },
        _ => unexpected_token_error!(),
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
          self.syntax_error(Problem::InvalidHint, &flag_span);
        }
      },
      PrintFlag::Sink => {
        if let Some(flag_span) = last_print_flag_span.take() {
          self.syntax_error(Problem::InvalidSink, &flag_span);
        }
      }
    }
    
    // Return the top-level sequence
    Ok((sequence.with_name_str("program"), SequenceEndType::ProgramEnd, is_seq_printing))
  }
  
  /// Parses a list/map initializer.
  fn parse_collection_initializer(&mut self, kind: CollectionInitKind, start_span: &Range<usize>) -> ParseResult<RST> {
    match kind {
      CollectionInitKind::List => {
        self.reader.skip_ws();
        
        // Exit early on empty list
        if self.reader.eat_where(|token| matches!(token, Some((RantToken::RightParen, ..)))) {
          return Ok(RST::ListInit(Rc::new(vec![])))
        }
        
        let mut sequences = vec![];
        
        loop {
          self.reader.skip_ws();
          
          let (seq, seq_end, _) = self.parse_sequence(SequenceParseMode::CollectionInit)?;
          
          match seq_end {
            SequenceEndType::CollectionInitDelim => {
              sequences.push(Rc::new(seq));
            },
            SequenceEndType::CollectionInitEnd => {
              sequences.push(Rc::new(seq));
              break
            },
            SequenceEndType::ProgramEnd => {
              self.syntax_error(Problem::UnclosedList, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            },
            _ => unreachable!()
          }
        }
        Ok(RST::ListInit(Rc::new(sequences)))
      },
      CollectionInitKind::Map => {
        let mut pairs = vec![];
        
        loop {
          let key_expr = match self.reader.next_solid() {
            // Allow blocks as dynamic keys
            Some((RantToken::LeftBrace, _)) => {
              MapKeyExpr::Dynamic(Rc::new(self.parse_dynamic_key(false)?))
            },
            // Allow fragments as keys if they are valid identifiers
            Some((RantToken::Fragment, span)) => {
              let key = self.reader.last_token_string();
              if !is_valid_ident(key.as_str()) {
                self.syntax_error(Problem::InvalidIdentifier(key.to_string()), &span);
              }
              MapKeyExpr::Static(key)
            },
            // Allow string literals as static keys
            Some((RantToken::StringLiteral(s), _)) => {
              MapKeyExpr::Static(s)
            },
            // End of map
            Some((RantToken::RightParen, _)) => break,
            // Soft error on anything weird
            Some(_) => {
              self.unexpected_last_token_error();
              MapKeyExpr::Static(self.reader.last_token_string())
            },
            // Hard error on EOF
            None => {
              self.syntax_error(Problem::UnclosedMap, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            }
          };
          
          self.reader.skip_ws();
          if !self.reader.eat_where(|tok| matches!(tok, Some((RantToken::Equals, ..)))) {
            self.syntax_error(Problem::ExpectedToken("=".to_owned()), &self.reader.last_token_span());
            return Err(())
          }
          self.reader.skip_ws();
          let (value_expr, value_end, _) = self.parse_sequence(SequenceParseMode::CollectionInit)?;
          
          match value_end {
            SequenceEndType::CollectionInitDelim => {
              pairs.push((key_expr, Rc::new(value_expr)));
            },
            SequenceEndType::CollectionInitEnd => {
              pairs.push((key_expr, Rc::new(value_expr)));
              break
            },
            SequenceEndType::ProgramEnd => {
              self.syntax_error(Problem::UnclosedMap, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            },
            _ => unreachable!()
          }
        }
        
        Ok(RST::MapInit(Rc::new(pairs)))
      },
    }
    
  }
  
  fn parse_func_params(&mut self, start_span: &Range<usize>) -> ParseResult<Vec<Parameter>> {
    // List of parameter names for function
    let mut params = vec![];
    // Separate set of all parameter names to check for duplicates
    let mut params_set = HashSet::new();
    // Most recently used parameter varity in this signature
    let mut last_varity = Varity::Required;
    // Keep track of whether we've encountered any variadic params
    let mut is_sig_variadic = false;
    
    // At this point there should either be ':' or ']'
    match self.reader.next_solid() {
      // ':' means there are params to be read
      Some((RantToken::Colon, _)) => {
        // Read the params
        'read_params: loop {
          match self.reader.next_solid() {
            // Regular parameter
            Some((RantToken::Fragment, span)) => {              
              // We only care about verifying/recording the param if it's in a valid position
              let param_name = Identifier::new(self.reader.last_token_string());
              // Make sure it's a valid identifier
              if !is_valid_ident(param_name.as_str()) {
                self.syntax_error(Problem::InvalidIdentifier(param_name.to_string()), &span)
              }
              // Check for duplicates
              // I'd much prefer to store references in params_set, but that's way more annoying to deal with
              if !params_set.insert(param_name.clone()) {
                self.syntax_error(Problem::DuplicateParameter(param_name.to_string()), &span);
              }                
              
              // Get varity of parameter
              self.reader.skip_ws();
              let (varity, full_param_span) = if let Some((varity_token, varity_span)) = 
              self.reader.take_where(|t| matches!(t, 
                Some((RantToken::Question, _)) |
                Some((RantToken::Star, _)) | 
                Some((RantToken::Plus, _)))) 
              {
                (match varity_token {
                  // Optional parameter
                  RantToken::Question => Varity::Optional,
                  // Optional variadic parameter
                  RantToken::Star => Varity::VariadicStar,
                  // Required variadic parameter
                  RantToken::Plus => Varity::VariadicPlus,
                  _ => unreachable!()
                }, super_range(&span, &varity_span))
              } else {
                (Varity::Required, span)
              };
              
              let is_param_variadic = varity.is_variadic();
                
              // Check for varity issues
              if is_sig_variadic && is_param_variadic {
                // Soft error on multiple variadics
                self.syntax_error(Problem::MultipleVariadicParams, &full_param_span);
              } else if !Varity::is_valid_order(last_varity, varity) {
                // Soft error on bad varity order
                self.syntax_error(Problem::InvalidParamOrder(last_varity.to_string(), varity.to_string()), &full_param_span);
              }
              
              // Add parameter to list
              params.push(Parameter {
                name: param_name,
                varity
              });
              
              last_varity = varity;
              is_sig_variadic |= is_param_variadic;
                
              // Check if there are more params or if the signature is done
              match self.reader.next_solid() {
                // ';' means there are more params
                Some((RantToken::Semi, ..)) => {
                  continue 'read_params
                },
                // ']' means end of signature
                Some((RantToken::RightBracket, ..)) => {
                  break 'read_params
                },
                // Emit a hard error on anything else
                Some((_, span)) => {
                  self.syntax_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
                  return Err(())
                },
                None => {
                  self.syntax_error(Problem::UnclosedFunctionSignature, &start_span);
                  return Err(())
                },
              }
            },
            // Error on early close
            Some((RantToken::RightBracket, span)) => {
              self.syntax_error(Problem::MissingIdentifier, &span);
              break 'read_params
            },
            // Error on anything else
            Some((.., span)) => {
              self.syntax_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span)
            },
            None => {
              self.syntax_error(Problem::UnclosedFunctionSignature, &start_span);
              return Err(())
            }
          }
        }
      },
      // ']' means there are no params-- fall through to the next step
      Some((RantToken::RightBracket, _)) => {},
      // Something weird is here, emit a hard error
      Some((.., span)) => {
        self.syntax_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
        return Err(())
      },
      // Nothing is here, emit a hard error
      None => {
        self.syntax_error(Problem::UnclosedFunctionSignature, &start_span);
        return Err(())
      }
    }
      
    Ok(params)
  }
    
  /// Parses a function definition, anonymous function, or function call.
  fn parse_func_access(&mut self, flag: PrintFlag) -> ParseResult<RST> {
    let start_span = self.reader.last_token_span();
    self.reader.skip_ws();
    // Check if we're defining a function (with [$ ...]) or creating a closure (with [? ...])
    if let Some((func_access_type_token, _func_access_type_span)) 
    = self.reader.take_where(|t| matches!(t, Some((RantToken::Dollar, ..)) | Some((RantToken::Question, ..)))) {
      match func_access_type_token {
        // Function definition
        RantToken::Dollar => {
          // Name of variable function will be stored in
          let func_id = self.parse_access_path()?;
          // Function params
          let params = self.parse_func_params(&start_span)?;
          // Read function body
          // TODO: Handle captured variables in function bodies
          self.reader.skip_ws();          
          let body = Rc::new(self.parse_func_body()?.with_name_str(format!("[{}]", func_id).as_str()));
          
          Ok(RST::FuncDef(FunctionDef {
            id: Rc::new(func_id),
            params: Rc::new(params),
            body,
            capture_vars: Rc::new(vec![]),
          }))
        },
        // Closure
        RantToken::Question => {
          // Closure params
          let params = self.parse_func_params(&start_span)?;
          self.reader.skip_ws();
          // Read function body
          // TODO: Handle captured variables in closure bodies
          let body = Rc::new(self.parse_func_body()?.with_name_str("closure"));
          
          Ok(RST::Closure(ClosureExpr {
            capture_vars: Rc::new(vec![]),
            expr: body,
            params: Rc::new(params),
          }))
        },
        _ => unreachable!()
      }
    } else {
      // Function call
      self.reader.skip_ws();
      let mut func_args = vec![];
      let is_anonymous = self.reader.eat_where(|t| matches!(t, Some((RantToken::Bang, ..))));
      self.reader.skip_ws();
      
      // What kind of function call is this?
      if is_anonymous {
        // Anonymous function call
        let (func_expr, func_expr_end, _) = self.parse_sequence(SequenceParseMode::AnonFunctionExpr)?;
        // Parse arguments if available
        match func_expr_end {
          SequenceEndType::AnonFuncctionExprNoArgs => {
            // No args, fall through
          },
          SequenceEndType::AnonFunctionExprToArgs => {
            loop {
              let (arg_seq, arg_end, _) = self.parse_sequence(SequenceParseMode::FunctionArg)?;
              func_args.push(Rc::new(arg_seq));
              match arg_end {
                SequenceEndType::FunctionArgEndNext => continue,
                SequenceEndType::FunctionArgEndBreak => break,
                _ => unreachable!()
              }
            }
          },
          _ => unreachable!()
        }
        
        // Create final node for anon function call
        let afcall = AnonFunctionCall {
          expr: Rc::new(func_expr),
          args: Rc::new(func_args),
          flag
        };
        
        Ok(RST::AnonFuncCall(afcall))
      } else {
        // Named function call
        let func_name = self.parse_access_path()?;
        if let Some((token, _)) = self.reader.next_solid() {
          match token {
            RantToken::RightBracket => {
              // No args, fall through
            },
            RantToken::Colon => {
              // Parse arguments
              loop {
                let (arg_seq, arg_end, _) = self.parse_sequence(SequenceParseMode::FunctionArg)?;
                func_args.push(Rc::new(arg_seq));
                match arg_end {
                  SequenceEndType::FunctionArgEndNext => continue,
                  SequenceEndType::FunctionArgEndBreak => break,
                  SequenceEndType::ProgramEnd => {
                    self.syntax_error(Problem::UnclosedFunctionCall, &self.reader.last_token_span());
                    return Err(())
                  }
                  _ => unreachable!()
                }
              }
            },
            _ => {
              self.unexpected_last_token_error();
              return Err(())
            }
          }
          
          // Create final node for function call
          let fcall = FunctionCall {
            id: Rc::new(func_name),
            arguments: Rc::new(func_args),
            flag
          };
          
          Ok(RST::FuncCall(fcall))
        } else {
          // Found EOF instead of end of function call, emit hard error
          self.syntax_error(Problem::UnclosedFunctionCall, &self.reader.last_token_span());
          Err(())
        }
      }
    }
  }
    
  #[inline]
  fn parse_access_path_kind(&mut self) -> AccessPathKind {    
    if let Some((token, _span)) = self.reader.take_where(
      |t| matches!(t, Some((RantToken::Slash, _)) | Some((RantToken::Caret, _))
    )) {
      match token {
        // Accessor is explicit global
        RantToken::Slash => {
          AccessPathKind::ExplicitGlobal
        },
        // Accessor is for parent scope (descope operator)
        RantToken::Caret => {
          let mut descope_count = 1;
          loop {
            if !self.reader.eat_where(|t| matches!(t, Some((RantToken::Caret, _)))) {
              break AccessPathKind::Descope(descope_count)
            }
            descope_count += 1;
          }
        },
        _ => unreachable!()
      }
    } else {
      AccessPathKind::Local
    }
  }
    
  // TODO: Anonymous getters/setters
  /// Parses an accessor.
  #[inline]
  fn parse_access_path(&mut self) -> ParseResult<AccessPath> {
    self.reader.skip_ws();
    let mut idparts = vec![];
    let preceding_span = self.reader.last_token_span();
    
    // Check for global/descope specifiers
    let access_kind = self.parse_access_path_kind();
    
    let first_part = self.reader.next_solid();
    
    // Parse the first part of the path
    match first_part {
      // The first part of the path may only be a variable name
      Some((RantToken::Fragment, span)) => {
        let varname = Identifier::new(self.reader.last_token_string());
        if is_valid_ident(varname.as_str()) {
          idparts.push(AccessPathComponent::Name(varname));
        } else {
          self.syntax_error(Problem::InvalidIdentifier(varname.to_string()), &span);
        }
      },
      // An expression can also be used to provide the variable
      Some((RantToken::LeftBrace, _)) => {
        let dynamic_key_expr = self.parse_dynamic_key(false)?;
        idparts.push(AccessPathComponent::Expression(Rc::new(dynamic_key_expr)));
      },
      Some((RantToken::Integer(_), span)) => {
        self.syntax_error(Problem::LocalPathStartsWithIndex, &span);
      },
      Some((.., span)) => {
        self.syntax_error(Problem::MissingIdentifier, &span);
      },
      None => {
        self.syntax_error(Problem::MissingIdentifier, &preceding_span);
        return Err(())
      }
    }
    
    // Parse the rest of the path
    loop {
      // We expect a '/' between each component, so check for that first.
      // If it's anything else, terminate the path and return it.
      self.reader.skip_ws();
      if self.reader.eat_where(|t| matches!(t, Some((RantToken::Slash, ..)))) {
        // From here we expect to see either another key (fragment) or index (integer).
        // If it's anything else, return a syntax error.
        let component = self.reader.next_solid();
        match component {
          // Key
          Some((RantToken::Fragment, span)) => {
            let varname = Identifier::new(self.reader.last_token_string());
            if is_valid_ident(varname.as_str()) {
              idparts.push(AccessPathComponent::Name(varname));
            } else {
              self.syntax_error(Problem::InvalidIdentifier(varname.to_string()), &span);
            }
          },
          // Index
          Some((RantToken::Integer(index), _)) => {
            idparts.push(AccessPathComponent::Index(index));
          },
          // Dynamic key
          Some((RantToken::LeftBrace, _)) => {
            let dynamic_key_expr = self.parse_dynamic_key(false)?;
            idparts.push(AccessPathComponent::Expression(Rc::new(dynamic_key_expr)));
          },
          Some((.., span)) => {
            // error
            self.syntax_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span);
          },
          None => {
            self.syntax_error(Problem::MissingIdentifier, &self.reader.last_token_span());
            return Err(())
          }
        }
      } else {
        return Ok(AccessPath::new(idparts, access_kind))
      }
    }
  }
    
  /// Parses a dynamic key.
  fn parse_dynamic_key(&mut self, expect_opening_brace: bool) -> ParseResult<Sequence> {
    if expect_opening_brace && !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.syntax_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    let start_span = self.reader.last_token_span();
    let (seq, seq_end, _) = self.parse_sequence(SequenceParseMode::DynamicKey)?;
    
    match seq_end {
      SequenceEndType::DynamicKeyEnd => {},
      SequenceEndType::ProgramEnd => {
        // Hard error if block isn't closed
        let err_span = start_span.start .. self.source.len();
        self.syntax_error(Problem::UnclosedBlock, &err_span);
        return Err(())
      },
      _ => unreachable!()
    }
    
    Ok(seq)
  }
    
  /// Parses a function body.
  fn parse_func_body(&mut self) -> ParseResult<Sequence> {
    if !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.syntax_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    let start_span = self.reader.last_token_span();
    let (seq, seq_end, _) = self.parse_sequence(SequenceParseMode::FunctionBody)?;
    
    match seq_end {
      SequenceEndType::FunctionBodyEnd => {},
      SequenceEndType::ProgramEnd => {
        // Hard error if block isn't closed
        let err_span = start_span.start .. self.source.len();
        self.syntax_error(Problem::UnclosedBlock, &err_span);
        return Err(())
      },
      _ => unreachable!()
    }
    
    Ok(seq)
  }
    
  /// Parses a block.
  fn parse_block(&mut self, expect_opening_brace: bool, flag: PrintFlag) -> ParseResult<Block> {
    if expect_opening_brace && !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.syntax_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    // Get position of starting brace for error reporting
    let start_pos = self.reader.last_token_pos();
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
          break
        },
        SequenceEndType::ProgramEnd => {
          // Hard error if block isn't closed
          let err_span = start_pos .. self.source.len();
          self.syntax_error(Problem::UnclosedBlock, &err_span);
          return Err(())
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
  
  /// Parses an identifier.
  fn parse_ident(&mut self) -> ParseResult<Identifier> {
    if let Some((token, span)) = self.reader.next_solid() {
      match token {
        RantToken::Fragment => {
          let idstr = self.reader.last_token_string();
          if !is_valid_ident(idstr.as_str()) {
            self.syntax_error(Problem::InvalidIdentifier(idstr.to_string()), &span);
          }
          Ok(Identifier::new(idstr))
        },
        _ => {
          self.unexpected_last_token_error();
          Err(())
        }
      }
    } else {
      self.syntax_error(Problem::MissingIdentifier, &self.reader.last_token_span());
      Err(())
    }
  }
    
  /// Parses one or more accessors (getter/setter/definition).
  fn parse_accessor(&mut self) -> ParseResult<Vec<RST>> {
    let mut accessors = vec![];
    
    'read: loop {
      self.reader.skip_ws();

      // Check if the accessor ends here as long as there's at least one component
      if !accessors.is_empty() && self.reader.eat_where(|t| matches!(t, Some((RantToken::RightAngle, ..)))) {
        break
      }
      
      let is_def = self.reader.eat_where(|t| matches!(t, Some((RantToken::Dollar, ..))));
      self.reader.skip_ws();
      
      // Check if it's a definition. If not, it's a getter or setter
      if is_def {
        // Check for accessor modifiers
        let access_kind = self.parse_access_path_kind();
        self.reader.skip_ws();
        // Read name of variable we're defining
        let var_name = self.parse_ident()?;
        
        if let Some((token, _)) = self.reader.next_solid() {
          match token {
            // Empty definition
            RantToken::RightAngle => {
              accessors.push(RST::VarDef(var_name, access_kind, None));
              break 'read
            },
            // Accessor delimiter
            RantToken::Semi => {
              accessors.push(RST::VarDef(var_name, access_kind, None));
              continue 'read;
            },
            // Definition and assignment
            RantToken::Equals => {
              self.reader.skip_ws();
              let (var_assign_expr, end_type, ..) = self.parse_sequence(SequenceParseMode::VariableAssignment)?;
              accessors.push(RST::VarDef(var_name, access_kind, Some(Rc::new(var_assign_expr))));
              match end_type {
                SequenceEndType::VariableAssignDelim => {
                  continue 'read
                },
                SequenceEndType::VariableAccessEnd => {
                  break 'read
                },
                SequenceEndType::ProgramEnd => {
                  self.syntax_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // Ran into something we don't support
            _ => {
              self.unexpected_last_token_error();
              return Err(())
            }
          }
        } else {
          self.syntax_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
          return Err(())
        }
      } else {
        // Read the path to what we're accessing
        let var_path = self.parse_access_path()?;
        
        if let Some((token, _)) = self.reader.next_solid() {
          match token {
            // If we hit a '>' here, it's a getter
            RantToken::RightAngle => {
              accessors.push(RST::VarGet(Rc::new(var_path)));
              break 'read;
            },
            // If we hit a ';' here, it's a getter with a continuation
            RantToken::Semi => {
              accessors.push(RST::VarGet(Rc::new(var_path)));
              continue 'read;
            },
            // If we hit a '=' here, it's a setter
            RantToken::Equals => {
              self.reader.skip_ws();
              let (var_assign_rhs, end_type, _) = self.parse_sequence(SequenceParseMode::VariableAssignment)?;
              accessors.push(RST::VarSet(Rc::new(var_path), Rc::new(var_assign_rhs)));
              match end_type {
                // Accessor was terminated
                SequenceEndType::VariableAccessEnd => {                  
                  break 'read;
                },
                // Expression ended with delimiter
                SequenceEndType::VariableAssignDelim => {
                  continue 'read;
                },
                // Error
                SequenceEndType::ProgramEnd => {
                  self.syntax_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // Anything else is an error
            _ => {
              self.unexpected_last_token_error();
              return Err(())
            }
          }
        } else {
          self.syntax_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
          return Err(())
        }
      }
    }
    
    Ok(accessors)
  }
}