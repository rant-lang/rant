#![allow(dead_code)]
#![allow(clippy::ptr_arg)]

use super::{reader::RantTokenReader, lexer::RantToken, message::*, Problem, Reporter};
use crate::{RantProgramInfo, InternalString, lang::*};
use fnv::FnvBuildHasher;
use line_col::LineColLookup;
use quickscope::ScopeMap;
use std::{collections::{HashMap, HashSet}, ops::Range, rc::Rc};

type ParseResult<T> = Result<T, ()>;

const MAIN_PROGRAM_SCOPE_NAME: &str = "main scope";

/// Provides context to the sequence parser; determines valid terminating tokens among other context-sensitive features.
#[derive(Copy, Clone, PartialEq)]
enum SequenceParseMode {
  /// Parse a sequence like a top-level program.
  ///
  /// Breaks on EOF.
  TopLevel,
  /// Parse a sequence like a block element.
  ///
  /// Breaks on `Pipe`, `Colon`, and `RightBrace`.
  BlockElement,
  /// Parse a sequence like a function argument.
  ///
  /// Breaks on `Semi`, `Compose`, and `RightBracket`.
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
  /// Parse a sequence like an accessor fallback value.
  ///
  /// Breaks on `RightAngle` and `Semi`.
  AccessorFallbackValue,
  /// Parse a sequence like a collection initializer element.
  ///
  /// Breaks on `Semi` and `RightParen`.
  CollectionInit,
  /// Parses a single item only.
  ///
  /// Breaks automatically or on EOF.
  SingleItem,
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
  /// Block element sequence is key and was terminated by `Colon`.
  BlockAssocDelim,
  /// Block element sequence was terminated by `Pipe`.
  BlockDelim,
  /// Block element sequence was terminated by `RightBrace`.
  BlockEnd,
  /// Function argument sequence was terminated by `Semi`.
  FunctionArgEndNext,
  /// Function argument sequence was terminated by `RightBracket`.
  FunctionArgEndBreak,
  /// Function argument sequence was terminated by `Compose`.
  FunctionArgEndToCompose,
  /// Function body sequence was terminated by `RightBrace`.
  FunctionBodyEnd,
  /// Dynamic key sequencce was terminated by `RightBrace`.
  DynamicKeyEnd,
  /// Anonymous function expression was terminated by `Colon`.
  AnonFunctionExprToArgs,
  /// Anonymous function expression was terminated by `RightBracket` and does not expect arguments.
  AnonFunctionExprNoArgs,
  /// Anonymous function expression was terminated by `Compose`.
  AnonFunctionExprToCompose,
  /// Variable accessor was terminated by `RightAngle`.
  VariableAccessEnd,
  /// Variable assignment expression was terminated by `Semi`. 
  VariableAssignDelim,
  /// Accessor fallback value was termianted by `RightAngle`.
  AccessorFallbackValueToEnd,
  /// Accessor fallback value was terminated by `Semi`.
  AccessorFallbackValueToDelim,
  /// Collection initializer was terminated by `RightParen`.
  CollectionInitEnd,
  /// Collection initializer was termianted by `Semi`.
  CollectionInitDelim,
  /// A single item was parsed using `SingleItem` mode.
  SingleItemEnd,
  /// A single module path was parsed using `RequirePath` mode.
  RequirePathEnd,
}

/// Used to track variable usages during compilation.
struct VarStats {
  def_span: Range<usize>,
  writes: usize,
  reads: usize,
  is_const: bool,
  role: VarRole,
}

#[derive(Copy, Clone, PartialEq)]
enum VarRole {
  Normal,
  Function,
  Parameter,
  ComposeValue,
}

/// Returns a range that encompasses both input ranges.
#[inline]
fn super_range(a: &Range<usize>, b: &Range<usize>) -> Range<usize> {
  a.start.min(b.start)..a.end.max(b.end)
}

#[derive(Debug)]
enum ParsedSequenceExtras {
  WeightedBlockElement {
    weight_expr: Rc<Sequence>
  }
}

/// Contains information about a successfully parsed sequence and its context.
struct ParsedSequence {
  sequence: Sequence,
  end_type: SequenceEndType,
  is_printing: bool,
  extras: Option<ParsedSequenceExtras>,
}

/// A parser that turns Rant code into an RST (Rant Syntax Tree).
pub struct RantParser<'source, 'report, R: Reporter> {
  /// A string slice containing the source code being parsed.
  source: &'source str,
  /// Flag set if there are compiler errors.
  has_errors: bool,
  /// The token stream used by the parser.
  reader: RantTokenReader<'source>,
  /// The line/col lookup for error reporting.
  lookup: LineColLookup<'source>,
  /// The error reporter.
  reporter: &'report mut R,
  /// Enables additional debug information.
  debug_enabled: bool,
  /// A string describing the origin (containing program) of a program element.
  info: Rc<RantProgramInfo>,
  /// Keeps track of active variables in each scope while parsing.
  var_stack: ScopeMap<Identifier, VarStats>,
  /// Keeps track of active variable capture frames.
  capture_stack: Vec<(usize, HashSet<Identifier, FnvBuildHasher>)>,
}

impl<'source, 'report, R: Reporter> RantParser<'source, 'report, R> {
  pub fn new(source: &'source str, reporter: &'report mut R, debug_enabled: bool, info: &Rc<RantProgramInfo>) -> Self {
    Self {
      source,
      has_errors: false,
      reader: RantTokenReader::new(source),
      lookup: LineColLookup::new(source),
      reporter,
      debug_enabled,
      info: Rc::clone(info),
      var_stack: Default::default(),
      capture_stack: Default::default(),
    }
  }
}

impl<'source, 'report, R: Reporter> RantParser<'source, 'report, R> {
  /// Top-level parsing function invoked by the compiler.
  pub fn parse(&mut self) -> Result<Rc<Sequence>, ()> {
    let result = self.parse_sequence(SequenceParseMode::TopLevel);
    match result {
      // Err if parsing "succeeded" but there are soft syntax errors
      Ok(..) if self.has_errors => Err(()),
      // Ok if parsing succeeded and there are no syntax errors
      Ok(ParsedSequence { sequence, .. }) => Ok(Rc::new(sequence)),
      // Err on hard syntax error
      Err(()) => Err(())
    }
  }
  
  /// Reports a syntax error, allowing parsing to continue but causing the final compilation to fail. 
  fn report_error(&mut self, problem: Problem, span: &Range<usize>) {
    let (line, col) = self.lookup.get(span.start);
    self.has_errors = true;
    self.reporter.report(CompilerMessage::new(problem, Severity::Error, Some(Position::new(line, col, span.clone()))));
  }

  /// Reports a warning, but allows compiling to succeed.
  fn report_warning(&mut self, problem: Problem, span: &Range<usize>) {
    let (line, col) = self.lookup.get(span.start);
    self.reporter.report(CompilerMessage::new(problem, Severity::Warning, Some(Position::new(line, col, span.clone()))));
  }
  
  /// Emits an "unexpected token" error for the most recently read token.
  #[inline]
  fn unexpected_last_token_error(&mut self) {
    self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &self.reader.last_token_span())
  }

  /// Parses a sequence of items. Items are individual elements of a Rant program (fragments, blocks, function calls, etc.)
  #[inline]
  fn parse_sequence(&mut self, mode: SequenceParseMode) -> ParseResult<ParsedSequence> {
    self.var_stack.push_layer();
    let parse_result = self.parse_sequence_inner(mode);
    self.analyze_top_vars();
    self.var_stack.pop_layer();
    parse_result
  }
  
  /// Inner logic of `parse_sequence()`. Intended to be wrapped in other specialized sequence-parsing functions.
  #[inline(always)]
  fn parse_sequence_inner(&mut self, mode: SequenceParseMode) -> ParseResult<ParsedSequence> {    
    let mut sequence = Sequence::empty(&self.info);
    let mut next_print_flag = PrintFlag::None;
    let mut last_print_flag_span: Option<Range<usize>> = None;
    let mut is_seq_printing = false;
    let mut pending_whitespace = None;
    let debug = self.debug_enabled;

    macro_rules! check_dangling_printflags {
      () => {
        // Make sure there are no dangling printflags
        match next_print_flag {
          PrintFlag::None => {},
          PrintFlag::Hint => {
            if let Some(flag_span) = last_print_flag_span.take() {
              self.report_error(Problem::InvalidHint, &flag_span);
            }
          },
          PrintFlag::Sink => {
            if let Some(flag_span) = last_print_flag_span.take() {
              self.report_error(Problem::InvalidSink, &flag_span);
            }
          }
        }
      }
    }
    
    while let Some((token, span)) = self.reader.next() {
      macro_rules! inject_debug_info {
        () => {
          if debug {
            let (line, col) = self.lookup.get(span.start);
            sequence.push(Rc::new(Rst::DebugCursor(DebugInfo::Location { line, col })));
          }
        }
      }
      
      // Macro for prohibiting hints/sinks before certain tokens
      macro_rules! no_flags {
        (on $b:block) => {{
          let elem = $b;
          if !matches!(next_print_flag, PrintFlag::None) {
            if let Some(flag_span) = last_print_flag_span.take() {
              self.report_error(match next_print_flag {
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
            self.report_error(match next_print_flag {
              PrintFlag::Hint => Problem::InvalidHint,
              PrintFlag::Sink => Problem::InvalidSink,
              PrintFlag::None => unreachable!()
            }, &flag_span)
          }
        };
      }

      macro_rules! emit {
        ($elem:expr) => {{
          inject_debug_info!();
          sequence.push(Rc::new($elem));
        }}
      }

      macro_rules! emit_last_string {
        () => {{
          inject_debug_info!();
          sequence.push(Rc::new(Rst::Fragment(InternalString::from(self.reader.last_token_string()))));
        }}
      }
      
      // Shortcut macro for "unexpected token" error
      macro_rules! unexpected_token_error {
        () => {
          self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span)
        };
      }
      
      macro_rules! whitespace {
        (allow) => {
          if is_seq_printing {
            if let Some(ws) = pending_whitespace.take() {
              emit!(Rst::Whitespace(ws));
            }
          } else {
            pending_whitespace = None;
          }
        };
        (queue next) => {{
          if let Some((RantToken::Whitespace, ..)) = self.reader.take_where(|tt| matches!(tt, Some((RantToken::Whitespace, ..)))) {
            pending_whitespace = Some(self.reader.last_token_string());
          }
        }};
        (queue $ws:expr) => {
          pending_whitespace = Some($ws);
        };
        (ignore prev) => {{
          #![allow(unused_assignments)]
          pending_whitespace = None;
        }};
        (ignore next) => {
          self.reader.skip_ws();
        };
        (ignore both) => {{
          whitespace!(ignore prev);
          whitespace!(ignore next);
        }};
      }

      /// Eats as many fragments / escape sequences as possible and combines their string representations into the input `String`.
      macro_rules! consume_fragments {
        ($s:ident) => {
          while let Some((token, _)) = self.reader.take_where(|t| matches!(t, Some((RantToken::Escape(..), ..)) | Some((RantToken::Fragment, ..)))) {
            match token {
              RantToken::Escape(ch) => $s.push(ch),
              _ => $s.push_str(&self.reader.last_token_string()),
            }
          }
        }
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

        RantToken::Charm(kwd) => {
          whitespace!(ignore both);
          let ParsedSequence {
            sequence: charm_sequence,
            end_type: charm_end_type,
            is_printing: is_charm_printing,
            extras: mut charm_extras
          } = self.parse_sequence(mode)?;
          let charm_sequence_name = charm_sequence.name.clone();
          let charm_sequence = (!charm_sequence.is_empty()).then(|| Rc::new(charm_sequence));
          match kwd.as_str() {
            "return" => emit!(Rst::Return(charm_sequence)),
            "continue" => emit!(Rst::Continue(charm_sequence)),
            "break" => emit!(Rst::Break(charm_sequence)),
            "weight" => {
              if mode == SequenceParseMode::BlockElement {
                charm_extras = Some(ParsedSequenceExtras::WeightedBlockElement {
                  weight_expr: charm_sequence.unwrap_or_else(|| Rc::new(Sequence::empty(&self.info)))
                });
              } else {
                self.report_error(Problem::WeightNotAllowed, &span);
              }
            },
            other => {
              self.report_error(Problem::InvalidKeyword(other.to_string()), &span);
            }
          };
          check_dangling_printflags!();
          return Ok(ParsedSequence {
            sequence: if let Some(charm_sequence_name) = charm_sequence_name {
              sequence.with_name(charm_sequence_name)
            } else {
              sequence
            },
            end_type: charm_end_type,
            is_printing: is_charm_printing || is_seq_printing,
            extras: charm_extras,
          })
        },
        
        // Defer operator
        RantToken::Defer => {
          self.reader.skip_ws();
          let block = self.parse_block(true, next_print_flag)?;
          
          // Decide what to do with surrounding whitespace
          match next_print_flag {                        
            // If hinted, allow pending whitespace
            PrintFlag::Hint => {
              whitespace!(allow);
              is_seq_printing = true;
            },
            
            // If sinked, remove surrounding whitespace
            PrintFlag::Sink => whitespace!(ignore both),
            
            // If no flag, take a hint
            PrintFlag::None => {
              // Inherit hints from inner blocks
              if let Block { flag: PrintFlag::Hint, ..} = block {
                whitespace!(allow);
                is_seq_printing = true;
              }
            }
          }
          
          emit!(Rst::BlockValue(Rc::new(block)));
        },
        
        // Block start
        RantToken::LeftBrace => {
          // Read in the entire block
          let block = self.parse_block(false, next_print_flag)?;

          // Decide what to do with previous whitespace
          match next_print_flag {                        
            // If hinted, allow pending whitespace
            PrintFlag::Hint => {
              whitespace!(allow);
              is_seq_printing = true;
            },
            
            // If sinked, delete pending whitespace
            PrintFlag::Sink => whitespace!(ignore both),
            
            // If no flag, infer from block contents
            PrintFlag::None => {
              // Inherit hints from inner blocks
              if let Block { flag: PrintFlag::Hint, ..} = block {
                whitespace!(allow);
                is_seq_printing = true;
              }
            }
          }
          
          emit!(Rst::Block(Rc::new(block)));
        },

        // Compose operator
        RantToken::Compose => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          match mode {
            SequenceParseMode::FunctionArg => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("argument"),
                end_type: SequenceEndType::FunctionArgEndToCompose,
                is_printing: is_seq_printing,
                extras: None,
              })
            },
            SequenceParseMode::AnonFunctionExpr => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("anonymous function expression"),
                end_type: SequenceEndType::AnonFunctionExprToCompose,
                is_printing: is_seq_printing,
                extras: None,
              })
            },
            _ => unexpected_token_error!()
          }
        }),

        // Compose value
        RantToken::ComposeValue => no_flags!({
          if let Some(compval) = self.var_stack.get_mut(COMPOSE_VALUE_NAME) {
            emit!(Rst::ComposeValue);
            compval.reads += 1;
          } else {
            self.report_error(Problem::NothingToCompose, &span);
          }
        }),
        
        // Block element delimiter (when in block parsing mode)
        RantToken::Pipe => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          match mode {
            SequenceParseMode::BlockElement => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("block element"),
                end_type: SequenceEndType::BlockDelim,
                is_printing: is_seq_printing,
                extras: None,
              })
            },
            SequenceParseMode::DynamicKey => {
              self.report_error(Problem::DynamicKeyBlockMultiElement, &span);
            },
            SequenceParseMode::FunctionBody => {
              self.report_error(Problem::FunctionBodyBlockMultiElement, &span);
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
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("block element"),
                end_type: SequenceEndType::BlockEnd,
                is_printing: true,
                extras: None,
              })
            },
            SequenceParseMode::FunctionBody => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("function body"),
                end_type: SequenceEndType::FunctionBodyEnd,
                is_printing: true,
                extras: None,
              })
            },
            SequenceParseMode::DynamicKey => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("dynamic key"),
                end_type: SequenceEndType::DynamicKeyEnd,
                is_printing: true,
                extras: None,
              })
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
              self.report_error(Problem::ExpectedToken("(".to_owned()), &self.reader.last_token_span());
              Rst::EmptyVal
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
              return Ok(ParsedSequence {
                sequence,
                end_type: SequenceEndType::CollectionInitEnd,
                is_printing: true,
                extras: None,
              })
            },
            _ => unexpected_token_error!()
          }
        }),
        
        // Function creation or call
        RantToken::LeftBracket => {
          let func_access = self.parse_func_access(next_print_flag)?;
          
          // Handle hint/sink behavior
          match func_access {
            Rst::FuncCall(FunctionCall { flag, ..}) => {
              // If the call is not sinked, allow whitespace around it
              match flag {
                PrintFlag::Hint => {
                  is_seq_printing = true;
                  whitespace!(allow);
                },
                _ => whitespace!(ignore both)
              }
            },
            // Definitions are implicitly sinked and ignore surrounding whitespace
            Rst::FuncDef(_) => {
              whitespace!(ignore both);
            },
            // Do nothing if it's an unsupported node type, e.g. NOP
            _ => {}
          }
          
          emit!(func_access);
        },
        
        // Can be terminator for function args and anonymous function expressions
        RantToken::RightBracket => no_flags!({
          match mode {
            SequenceParseMode::AnonFunctionExpr => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("anonymous function expression"),
              end_type: SequenceEndType::AnonFunctionExprNoArgs,
              is_printing: true,
              extras: None,
            }),
            SequenceParseMode::FunctionArg => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("argument"),
              end_type: SequenceEndType::FunctionArgEndBreak,
              is_printing: true,
              extras: None,
            }),
            _ => unexpected_token_error!()
          }
        }),
        
        // Variable access start
        RantToken::LeftAngle => no_flags!({
          let accessors = self.parse_accessor()?;
          for accessor in accessors {
            match accessor {
              Rst::VarGet(..) => {
                is_seq_printing = true;
                whitespace!(allow);
              },
              Rst::VarSet(..) | Rst::VarDef(..) | Rst::ConstDef(..) => {
                // whitespace!(ignore both);
              },
              _ => unreachable!()
            }
            emit!(accessor);
          }
        }),
        
        // Variable access end
        RantToken::RightAngle => no_flags!({
          match mode {
            SequenceParseMode::VariableAssignment => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("setter value"),
              end_type: SequenceEndType::VariableAccessEnd,
              is_printing: true,
              extras: None,
            }),
            SequenceParseMode::AccessorFallbackValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("fallback value"),
              end_type: SequenceEndType::AccessorFallbackValueToEnd,
              is_printing: true,
              extras: None,
            }),
            _ => unexpected_token_error!()
          }
        }),
        
        // These symbols are only used in special contexts and can be safely printed
        RantToken::Bang | RantToken::Question | RantToken::Slash | RantToken::Plus | RantToken::Dollar | RantToken::Equals | RantToken::Percent
        => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          let frag = self.reader.last_token_string();
          Rst::Fragment(frag)
        }),
        
        // Fragment
        RantToken::Fragment => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          let mut frag = self.reader.last_token_string();
          consume_fragments!(frag);
          Rst::Fragment(frag)
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
        RantToken::Escape(ch) => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          let mut frag = InternalString::new();
          frag.push(ch);
          consume_fragments!(frag);
          Rst::Fragment(frag)
        }),
        
        // Integers
        RantToken::Integer(n) => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          Rst::Integer(n)
        }),
        
        // Floats
        RantToken::Float(n) => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          Rst::Float(n)
        }),
        
        // True
        RantToken::True => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          Rst::Boolean(true)
        }),
        
        // False
        RantToken::False => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          Rst::Boolean(false)
        }),
        
        // Empty
        RantToken::EmptyValue => no_flags!(on {
          Rst::EmptyVal
        }),
        
        // Verbatim string literals
        RantToken::StringLiteral(s) => no_flags!(on {
          whitespace!(allow);
          is_seq_printing = true;
          Rst::Fragment(s)
        }),
        
        // Colon can be either fragment or argument separator.
        RantToken::Colon => no_flags!({
          match mode {
            SequenceParseMode::AnonFunctionExpr => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("anonymous function expression"),
              end_type: SequenceEndType::AnonFunctionExprToArgs,
              is_printing: true,
              extras: None,
            }),
            _ => emit_last_string!(),
          }
        }),
        
        // Semicolon can be a fragment, collection element separator, or argument separator.
        RantToken::Semi => no_flags!({
          match mode {
            // If we're inside a function argument, terminate the sequence
            SequenceParseMode::FunctionArg => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("argument"),
              end_type: SequenceEndType::FunctionArgEndNext,
              is_printing: true,
              extras: None,
            }),
            // Collection initializer
            SequenceParseMode::CollectionInit => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("collection item"),
              end_type: SequenceEndType::CollectionInitDelim,
              is_printing: true,
              extras: None,
            }),
            // Variable assignment expression
            SequenceParseMode::VariableAssignment => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("variable assignment"),
              end_type: SequenceEndType::VariableAssignDelim,
              is_printing: true,
              extras: None,
            }),
            // Accessor fallback value
            SequenceParseMode::AccessorFallbackValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("fallback"),
              end_type: SequenceEndType::AccessorFallbackValueToDelim,
              is_printing: true,
              extras: None,
            }),
            // If we're anywhere else, just print the semicolon like normal text
            _ => emit_last_string!(),
          }
        }),
        
        // Handle unclosed string literals as hard errors
        RantToken::UnterminatedStringLiteral => {
          self.report_error(Problem::UnclosedStringLiteral, &span); 
          return Err(())
        },
        _ => unexpected_token_error!(),
      }

      // If in Single Item mode, return the sequence immediately without looping
      if mode == SequenceParseMode::SingleItem {
        return Ok(ParsedSequence {
          sequence,
          end_type: SequenceEndType::SingleItemEnd,
          is_printing: is_seq_printing,
          extras: None,
        })
      }
      
      // Clear flag
      next_print_flag = PrintFlag::None;
    }
    
    // Reached when the whole program has been read
    // This should only get hit for top-level sequences
    
    check_dangling_printflags!();
    
    // Return the top-level sequence
    Ok(ParsedSequence {
      sequence: sequence.with_name_str(MAIN_PROGRAM_SCOPE_NAME),
      end_type: SequenceEndType::ProgramEnd,
      is_printing: is_seq_printing,
      extras: None,
    })
  }
  
  /// Parses a list/map initializer.
  fn parse_collection_initializer(&mut self, kind: CollectionInitKind, start_span: &Range<usize>) -> ParseResult<Rst> {
    match kind {
      CollectionInitKind::List => {
        self.reader.skip_ws();
        
        // Exit early on empty list
        if self.reader.eat_where(|token| matches!(token, Some((RantToken::RightParen, ..)))) {
          return Ok(Rst::ListInit(Rc::new(vec![])))
        }
        
        let mut sequences = vec![];
        
        loop {
          self.reader.skip_ws();
          
          let ParsedSequence { sequence, end_type: seq_end, .. } = self.parse_sequence(SequenceParseMode::CollectionInit)?;
          
          match seq_end {
            SequenceEndType::CollectionInitDelim => {
              sequences.push(Rc::new(sequence));
            },
            SequenceEndType::CollectionInitEnd => {
              sequences.push(Rc::new(sequence));
              break
            },
            SequenceEndType::ProgramEnd => {
              self.report_error(Problem::UnclosedList, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            },
            _ => unreachable!()
          }
        }
        Ok(Rst::ListInit(Rc::new(sequences)))
      },
      CollectionInitKind::Map => {
        let mut pairs = vec![];
        
        loop {
          let key_expr = match self.reader.next_solid() {
            // Allow blocks as dynamic keys
            Some((RantToken::LeftBrace, _)) => {
              MapKeyExpr::Dynamic(Rc::new(self.parse_dynamic_expr(false)?))
            },
            // Allow fragments as keys if they are valid identifiers
            Some((RantToken::Fragment, span)) => {
              let key = self.reader.last_token_string();
              if !is_valid_ident(key.as_str()) {
                self.report_error(Problem::InvalidIdentifier(key.to_string()), &span);
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
              self.report_error(Problem::UnclosedMap, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            }
          };
          
          self.reader.skip_ws();
          if !self.reader.eat_where(|tok| matches!(tok, Some((RantToken::Equals, ..)))) {
            self.report_error(Problem::ExpectedToken("=".to_owned()), &self.reader.last_token_span());
            return Err(())
          }
          self.reader.skip_ws();
          let ParsedSequence { 
            sequence: value_expr, 
            end_type: value_expr_end, 
            .. 
          } = self.parse_sequence(SequenceParseMode::CollectionInit)?;
          
          match value_expr_end {
            SequenceEndType::CollectionInitDelim => {
              pairs.push((key_expr, Rc::new(value_expr)));
            },
            SequenceEndType::CollectionInitEnd => {
              pairs.push((key_expr, Rc::new(value_expr)));
              break
            },
            SequenceEndType::ProgramEnd => {
              self.report_error(Problem::UnclosedMap, &super_range(start_span, &self.reader.last_token_span()));
              return Err(())
            },
            _ => unreachable!()
          }
        }
        
        Ok(Rst::MapInit(Rc::new(pairs)))
      },
    }
    
  }
  
  fn parse_func_params(&mut self, start_span: &Range<usize>) -> ParseResult<Vec<(Parameter, Range<usize>)>> {
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
                self.report_error(Problem::InvalidIdentifier(param_name.to_string()), &span)
              }
              // Check for duplicates
              // I'd much prefer to store references in params_set, but that's way more annoying to deal with
              if !params_set.insert(param_name.clone()) {
                self.report_error(Problem::DuplicateParameter(param_name.to_string()), &span);
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
                self.report_error(Problem::MultipleVariadicParams, &full_param_span);
              } else if !Varity::is_valid_order(last_varity, varity) {
                // Soft error on bad varity order
                self.report_error(Problem::InvalidParamOrder(last_varity.to_string(), varity.to_string()), &full_param_span);
              }

              let param = Parameter {
                name: param_name,
                varity
              };
              
              // Add parameter to list
              params.push((param, full_param_span));
              
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
                  self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
                  return Err(())
                },
                None => {
                  self.report_error(Problem::UnclosedFunctionSignature, &start_span);
                  return Err(())
                },
              }
            },
            // Error on early close
            Some((RantToken::RightBracket, span)) => {
              self.report_error(Problem::MissingIdentifier, &span);
              break 'read_params
            },
            // Error on anything else
            Some((.., span)) => {
              self.report_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span)
            },
            None => {
              self.report_error(Problem::UnclosedFunctionSignature, &start_span);
              return Err(())
            }
          }
        }
      },
      // ']' means there are no params-- fall through to the next step
      Some((RantToken::RightBracket, _)) => {},
      // Something weird is here, emit a hard error
      Some((.., span)) => {
        self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
        return Err(())
      },
      // Nothing is here, emit a hard error
      None => {
        self.report_error(Problem::UnclosedFunctionSignature, &start_span);
        return Err(())
      }
    }
      
    Ok(params)
  }
    
  /// Parses a function definition, anonymous function, or function call.
  fn parse_func_access(&mut self, flag: PrintFlag) -> ParseResult<Rst> {
    let start_span = self.reader.last_token_span();
    self.reader.skip_ws();
    // Check if we're defining a function (with [$|% ...]) or creating a closure (with [? ...])
    if let Some((func_access_type_token, func_access_type_span)) 
    = self.reader.take_where(|t| matches!(t, Some((RantToken::Dollar, ..)) | Some((RantToken::Percent, ..)) | Some((RantToken::Question, ..)))) {
      match func_access_type_token {
        // Function definition
        tt @ RantToken::Dollar | tt @ RantToken::Percent => {
          let is_const = matches!(tt, RantToken::Percent);
          // Name of variable function will be stored in
          let (func_path, _func_path_span) = self.parse_access_path(false)?;

          // Warn user if non-variable function definition is marked as a constant
          if is_const && !func_path.is_variable() {
            self.report_warning(Problem::NestedFunctionDefMarkedConstant, &func_access_type_span);
          }

          // Function params
          let params = self.parse_func_params(&start_span)?;
          let end_func_sig_span = self.reader.last_token_span();
          // Read function body
          self.reader.skip_ws();          
          let (body, captures) = self.parse_func_body(&params)?;

          // Track variable
          if let Some(id) = &func_path.var_name() {
            let func_def_span = super_range(&start_span, &end_func_sig_span);
            self.track_variable(id, &func_path.kind(), is_const, VarRole::Function, &func_def_span);
          }
          
          Ok(Rst::FuncDef(FunctionDef {
            body: Rc::new(body.with_name_str(format!("[{}]", func_path).as_str())),
            path: Rc::new(func_path),
            params: Rc::new(params.into_iter().map(|(p, _)| p).collect()),
            capture_vars: Rc::new(captures),
            is_const,
          }))
        },
        // Closure
        RantToken::Question => {
          // Closure params
          let params = self.parse_func_params(&start_span)?;
          self.reader.skip_ws();
          // Read function body
          let (body, captures) = self.parse_func_body(&params)?;
          
          Ok(Rst::Closure(ClosureExpr {
            capture_vars: Rc::new(captures),
            expr: Rc::new(body.with_name_str("closure")),
            params: Rc::new(params.into_iter().map(|(p, _)| p).collect()),
          }))
        },
        _ => unreachable!()
      }
    } else {
      // Function calls, both composed and otherwise
      
      // List of calls in chain. This will only contain one call if it's non-composed (chain of one).
      let mut calls: Vec<FunctionCall> = vec![];
      // Flag indicating whether call is composed (has multiple chained function calls)
      let mut is_composed = false;
      // Indicates whether the last call in the chain has been parsed
      let mut is_finished = false;
      // Indicates whether the chain has any temporal calls
      let mut is_chain_temporal = false;

      // Read all calls in chain
      while !is_finished {
        self.reader.skip_ws();
        // Argument list for current call
        let mut func_args = vec![];
        // Currently tracked temporal labels
        let mut temporal_index_labels: HashMap<InternalString, usize> = Default::default();
        // Next temporal index to be consumed
        let mut cur_temporal_index: usize = 0;
        // Anonymous call flag
        let is_anonymous = self.reader.eat_where(|t| matches!(t, Some((RantToken::Bang, ..))));
        // Temporal call flag
        let mut is_temporal = false;
        // Do the user-supplied args use the compose value?
        let mut is_compval_used = false;
        
        /// Reads arguments until a terminating / delimiting token is reached.
        macro_rules! parse_args {
          () => {{
            #[allow(unused_assignments)] // added because rustc whines about `spread_mode` being unused; that is a LIE
            loop {
              self.reader.skip_ws();
              let mut spread_mode = ArgumentSpreadMode::NoSpread;

              // Check for spread operators
              match self.reader.take_where(|t| matches!(t, Some((RantToken::Star, ..)) | Some((RantToken::Temporal, ..)) | Some((RantToken::TemporalLabeled(_), ..)))) {
                // Parametric spread
                Some((RantToken::Star, ..)) => {
                  self.reader.skip_ws();
                  spread_mode = ArgumentSpreadMode::Parametric;
                },
                // Unlabeled temporal spread
                Some((RantToken::Temporal, ..)) => {
                  is_temporal = true;
                  self.reader.skip_ws();
                  spread_mode = ArgumentSpreadMode::Temporal { label: cur_temporal_index };
                  cur_temporal_index += 1;
                },
                // Labeled temporal spread
                Some((RantToken::TemporalLabeled(label_str), ..)) => {
                  is_temporal = true;
                  self.reader.skip_ws();
                  let label_index = if let Some(label_index) = temporal_index_labels.get(&label_str) {
                    *label_index
                  } else {
                    let label_index = cur_temporal_index;
                    temporal_index_labels.insert(label_str.clone(), label_index);
                    cur_temporal_index += 1;
                    label_index
                  };
                  spread_mode = ArgumentSpreadMode::Temporal { label: label_index };
                },
                Some(_) => unreachable!(),
                None => {},
              }

              // Parse argument
              let ParsedSequence {
                sequence: arg_seq,
                end_type: arg_end,
                ..
              } = if is_composed {
                self.var_stack.push_layer();
                // Track compose value inside arguement scope
                let compval_stats = VarStats {
                  writes: 1,
                  reads: 0,
                  def_span: Default::default(), // we'll never need this anyway
                  is_const: true,
                  role: VarRole::ComposeValue,
                };
                self.var_stack.define(Identifier::from(COMPOSE_VALUE_NAME), compval_stats);
                let parsed_arg_expr = self.parse_sequence_inner(SequenceParseMode::FunctionArg)?;
                is_compval_used |= self.var_stack.get(COMPOSE_VALUE_NAME).unwrap().reads > 0;
                self.analyze_top_vars();
                self.var_stack.pop_layer();
                parsed_arg_expr
              } else {
                self.parse_sequence(SequenceParseMode::FunctionArg)?
              };

              let arg = ArgumentExpr {
                expr: Rc::new(arg_seq),
                spread_mode,
              };
              func_args.push(arg);
              match arg_end {
                SequenceEndType::FunctionArgEndNext => continue,
                SequenceEndType::FunctionArgEndBreak => {
                  is_finished = true;
                  break
                },
                SequenceEndType::FunctionArgEndToCompose => {
                  is_composed = true;
                  break
                },
                SequenceEndType::ProgramEnd => {
                  self.report_error(Problem::UnclosedFunctionCall, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            }
          }}
        }

        /// If the composition value wasn't used, inserts it as the first argument.
        macro_rules! fallback_compose {
          () => {
            if calls.len() > 0 && !is_compval_used {
              let arg = ArgumentExpr {
                expr: Rc::new(Sequence::one(Rst::ComposeValue, &self.info)),
                spread_mode: ArgumentSpreadMode::NoSpread,
              };
              func_args.insert(0, arg);
            }
          }
        }
        
        self.reader.skip_ws();

        // What kind of function call is this?
        if is_anonymous {
          // Anonymous function call
          
          self.var_stack.push_layer();
          // Track compose value inside anonymous function access scope
          let compval_stats = VarStats {
            writes: 1,
            reads: 0,
            def_span: Default::default(), // we'll never need this anyway
            is_const: true,
            role: VarRole::ComposeValue,
          };
          self.var_stack.define(Identifier::from(COMPOSE_VALUE_NAME), compval_stats);
          let ParsedSequence {
            sequence: func_expr,
            end_type: func_expr_end,
            ..
          } = self.parse_sequence_inner(SequenceParseMode::AnonFunctionExpr)?;
          is_compval_used |= self.var_stack.get(COMPOSE_VALUE_NAME).unwrap().reads > 0;
          self.analyze_top_vars();
          self.var_stack.pop_layer();
          
          // Parse arguments if available
          match func_expr_end {
            // No args, fall through
            SequenceEndType::AnonFunctionExprNoArgs => {
              is_finished = true;
            },
            // Parse arguments
            SequenceEndType::AnonFunctionExprToArgs => parse_args!(),
            // Compose without args
            SequenceEndType::AnonFunctionExprToCompose => {
              is_composed = true;
            }
            _ => unreachable!()
          }

          fallback_compose!();
          
          // Create final node for anon function call
          let fcall = FunctionCall {
            target: FunctionCallTarget::Expression(Rc::new(func_expr)),
            arguments: Rc::new(func_args),
            flag,
            is_temporal,
          };

          calls.push(fcall);
        } else {
          // Named function call
          let (func_path, func_path_span) = self.parse_access_path(false)?;
          if let Some((token, _)) = self.reader.next_solid() {
            match token {
              // No args, fall through
              RantToken::RightBracket => {
                is_finished = true;
              },
              // Parse arguments
              RantToken::Colon => parse_args!(),
              // Compose without args
              RantToken::Compose => {
                is_composed = true;
              }
              _ => {
                self.unexpected_last_token_error();
                return Err(())
              }
            }

            fallback_compose!();
            
            // Record access to function
            self.track_variable_access(&func_path, false, &func_path_span);
            
            // Create final node for function call
            let fcall = FunctionCall {
              target: FunctionCallTarget::Path(Rc::new(func_path)),
              arguments: Rc::new(func_args),
              flag,
              is_temporal,
            };

            calls.push(fcall);
          } else {
            // Found EOF instead of end of function call, emit hard error
            self.report_error(Problem::UnclosedFunctionCall, &self.reader.last_token_span());
            return Err(())
          }
        }

        is_chain_temporal |= is_temporal;
      }

      // Return the finished node
      Ok(if is_composed {
        Rst::ComposedCall(ComposedFunctionCall {
          flag,
          is_temporal: is_chain_temporal,
          steps: Rc::new(calls),
        })
      } else {
        Rst::FuncCall(calls.drain(..).next().unwrap())
      })
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
  
  /// Parses an access path.
  #[inline]
  fn parse_access_path(&mut self, allow_anonymous: bool) -> ParseResult<(AccessPath, Range<usize>)> {
    self.reader.skip_ws();
    let mut idparts = vec![];
    let start_span = self.reader.last_token_span();
    let mut access_kind = AccessPathKind::Local;

    if allow_anonymous && self.reader.eat_where(|t| matches!(t, Some((RantToken::Bang, ..)))) {
      self.reader.skip_ws();
      let ParsedSequence {
        sequence: anon_expr,
        end_type: anon_end_type,
        ..
      } = self.parse_sequence(SequenceParseMode::SingleItem)?;
      match anon_end_type {
        SequenceEndType::SingleItemEnd => {
          idparts.push(AccessPathComponent::AnonymousValue(Rc::new(anon_expr)));
        },
        SequenceEndType::ProgramEnd => {
          self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
          return Err(())
        },
        _ => unreachable!(),
      }
    } else {
      // Check for global/descope specifiers
      access_kind = self.parse_access_path_kind();
      
      let first_part = self.reader.next_solid();
      
      // Parse the first part of the path
      match first_part {
        // The first part of the path may only be a variable name (for now)
        Some((RantToken::Fragment, span)) => {
          let varname = Identifier::new(self.reader.last_token_string());
          if is_valid_ident(varname.as_str()) {
            idparts.push(AccessPathComponent::Name(varname));
          } else {
            self.report_error(Problem::InvalidIdentifier(varname.to_string()), &span);
          }
        },
        // An expression can also be used to provide the variable
        Some((RantToken::LeftBrace, _)) => {
          let dynamic_key_expr = self.parse_dynamic_expr(false)?;
          idparts.push(AccessPathComponent::DynamicKey(Rc::new(dynamic_key_expr)));
        },
        // TODO: Check for dynamic slices here too!
        // First path part can't be a slice
        Some((RantToken::Colon, span)) => {
          self.reader.take_where(|t| matches!(t, Some((RantToken::Integer(_), ..))));
          self.report_error(Problem::AccessPathStartsWithSlice, &super_range(&span, &self.reader.last_token_span()));
        }
        // Prevent other slice forms
        Some((RantToken::Integer(_), span)) => {
          self.reader.skip_ws();
          if self.reader.eat_where(|t| matches!(t, Some((RantToken::Colon, ..)))) {
            self.report_error(Problem::AccessPathStartsWithSlice, &super_range(&span, &self.reader.last_token_span()));
          } else {
            self.report_error(Problem::AccessPathStartsWithIndex, &span);
          }
        },
        Some((.., span)) => {
          self.report_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span);
        },
        None => {
          self.report_error(Problem::MissingIdentifier, &start_span);
          return Err(())
        }
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
              self.report_error(Problem::InvalidIdentifier(varname.to_string()), &span);
            }
          },
          // Index or slice with static from-bound
          Some((RantToken::Integer(i), _)) => {
            self.reader.skip_ws();
            // Look for a colon to see if it's a slice
            if self.reader.eat_where(|t| matches!(t, Some((RantToken::Colon, ..)))) {
              self.reader.skip_ws();
              match self.reader.peek() {
                // Between-slice with static from- + to-bounds
                Some((RantToken::Integer(j), ..)) => {
                  let j = *j;
                  self.reader.skip_one();
                  idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Static(i), SliceIndex::Static(j))));
                },
                // Between-slice with static from-bound + dynamic to-bound
                Some((RantToken::LeftBrace, ..)) => {
                  let to_expr = Rc::new(self.parse_dynamic_expr(true)?);
                  idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Static(i), SliceIndex::Dynamic(to_expr))));
                },
                // From-slice with static from-bound
                Some((RantToken::Slash, ..)) | 
                Some((RantToken::RightAngle, ..)) | 
                Some((RantToken::Equals, ..)) | 
                Some((RantToken::Question, ..)) |
                Some((RantToken::Semi, ..)) => {
                  idparts.push(AccessPathComponent::Slice(SliceExpr::From(SliceIndex::Static(i))));
                },
                // Found something weird as the to-bound, emit an error
                Some(_) => {
                  self.reader.next();
                  let token = self.reader.last_token_string().to_string();
                  self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
                },
                None => {
                  self.report_error(Problem::UnclosedVariableAccess, &super_range(&start_span, &self.reader.last_token_span()));
                  return Err(())
                }
              }
            } else {
              // No colon, so it's an index
              idparts.push(AccessPathComponent::Index(i));
            }
          },
          // Full- or to-slice
          Some((RantToken::Colon, _)) => {
            self.reader.skip_ws();
            match self.reader.peek() {
              // To-slice with static bound
              Some((RantToken::Integer(to), ..)) => {
                let to = *to;
                self.reader.skip_one();
                idparts.push(AccessPathComponent::Slice(SliceExpr::To(SliceIndex::Static(to))));
              },
              // To-slice with dynamic bound
              Some((RantToken::LeftBrace, ..)) => {
                let to_expr = Rc::new(self.parse_dynamic_expr(true)?);
                idparts.push(AccessPathComponent::Slice(SliceExpr::To(SliceIndex::Dynamic(to_expr))));
              },
              // Full-slice
              Some((RantToken::Slash, ..)) | 
              Some((RantToken::RightAngle, ..)) | 
              Some((RantToken::Equals, ..)) | 
              Some((RantToken::Question, ..)) |
              Some((RantToken::Semi, ..)) => {
                idparts.push(AccessPathComponent::Slice(SliceExpr::Full));
              },
              // Found something weird as the to-bound, emit an error
              Some(_) => {
                self.reader.next();
                let token = self.reader.last_token_string().to_string();
                self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
              },
              None => {
                self.report_error(Problem::UnclosedVariableAccess, &super_range(&start_span, &self.reader.last_token_span()));
                return Err(())
              }
            }
          },
          // Dynamic key or slice with dynamic from-bound
          Some((RantToken::LeftBrace, _)) => {
            let expr = Rc::new(self.parse_dynamic_expr(false)?);
            self.reader.skip_ws();
            // Look for a colon to see if it's a slice
            if self.reader.eat_where(|t| matches!(t, Some((RantToken::Colon, ..)))) {
              self.reader.skip_ws();
              match self.reader.peek() {
                // Between-slice with a dynamic from-bound + static to-bound
                Some((RantToken::Integer(to), ..)) => {
                  let to = *to;
                  self.reader.skip_one();
                  idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Dynamic(expr), SliceIndex::Static(to))));
                },
                // Between-slice with dynamic from- + to-bounds
                Some((RantToken::LeftBrace, ..)) => {
                  let to_expr = Rc::new(self.parse_dynamic_expr(true)?);
                  idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Dynamic(expr), SliceIndex::Dynamic(to_expr))));
                },
                // From-slice with dynamic bound
                Some((RantToken::Slash, ..)) |
                Some((RantToken::RightAngle, ..)) | 
                Some((RantToken::Equals, ..)) | 
                Some((RantToken::Question, ..)) |
                Some((RantToken::Semi, ..)) => {
                  idparts.push(AccessPathComponent::Slice(SliceExpr::From(SliceIndex::Dynamic(expr))));
                },
                // Found something weird as the to-bound, emit an error
                Some(_) => {
                  self.reader.next();
                  let token = self.reader.last_token_string().to_string();
                  self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
                },
                None => {
                  self.report_error(Problem::UnclosedVariableAccess, &super_range(&start_span, &self.reader.last_token_span()));
                  return Err(())
                }
              }
            } else {
              // No colon, so it's an dynamic key
              idparts.push(AccessPathComponent::DynamicKey(expr));
            }
          },
          Some((.., span)) => {
            // error
            self.report_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span);
          },
          None => {
            self.report_error(Problem::MissingIdentifier, &self.reader.last_token_span());
            return Err(())
          }
        }
      } else {
        return Ok((AccessPath::new(idparts, access_kind), start_span.start .. self.reader.last_token_span().start))
      }
    }
  }
    
  /// Parses a dynamic expression (a linear block).
  fn parse_dynamic_expr(&mut self, expect_opening_brace: bool) -> ParseResult<Sequence> {
    if expect_opening_brace && !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.report_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    let start_span = self.reader.last_token_span();
    let ParsedSequence { sequence, end_type, .. } = self.parse_sequence(SequenceParseMode::DynamicKey)?;
    
    match end_type {
      SequenceEndType::DynamicKeyEnd => {},
      SequenceEndType::ProgramEnd => {
        // Hard error if block isn't closed
        let err_span = start_span.start .. self.source.len();
        self.report_error(Problem::UnclosedBlock, &err_span);
        return Err(())
      },
      _ => unreachable!()
    }
    
    Ok(sequence)
  }

  /// Parses a function body and captures variables.
  fn parse_func_body(&mut self, params: &Vec<(Parameter, Range<usize>)>) -> ParseResult<(Sequence, Vec<Identifier>)> {
    if !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.report_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    let start_span = self.reader.last_token_span();

    // Since we're about to push another var_stack frame, we can use the current depth of var_stack as the index
    let capture_height = self.var_stack.depth();

    // Push a new capture frame
    self.capture_stack.push((capture_height, Default::default()));

    // Push a new variable frame
    self.var_stack.push_layer();

    // Define each parameter as a variable in the current var_stack frame so they are not accidentally captured
    for (param, span) in params {
      self.var_stack.define(param.name.clone(), VarStats {
        reads: 0,
        writes: 1,
        def_span: span.clone(),
        is_const: true,
        role: VarRole::Parameter,
      });
    }

    // parse_sequence_inner() is used here so that the new stack frame can be customized before use
    let parse_result = self.parse_sequence_inner(SequenceParseMode::FunctionBody);

    // Run static analysis on variable/param usage
    self.analyze_top_vars();

    self.var_stack.pop_layer();

    // Pop the topmost capture frame and grab the set of captures
    let (_, mut capture_set) = self.capture_stack.pop().unwrap();

    match parse_result {
      Ok(ParsedSequence { sequence, end_type, .. }) => {
        match end_type {
          SequenceEndType::FunctionBodyEnd => {},
          SequenceEndType::ProgramEnd => {
            // Hard error if block isn't closed
            let err_span = start_span.start .. self.source.len();
            self.report_error(Problem::UnclosedBlock, &err_span);
            return Err(())
          },
          _ => unreachable!()
        }
  
        Ok((sequence, capture_set.drain().collect()))
      },
      Err(()) => {
        Err(())
      }
    }
  }
    
  /// Parses a block.
  fn parse_block(&mut self, expect_opening_brace: bool, flag: PrintFlag) -> ParseResult<Block> {
    if expect_opening_brace && !self.reader.eat_where(|t| matches!(t, Some((RantToken::LeftBrace, _)))) {
      self.report_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }
    
    // Get position of starting brace for error reporting
    let start_pos = self.reader.last_token_pos();
    // Keeps track of inherited hinting
    let mut auto_hint = false;
    // Is the block weighted?
    let mut is_weighted = false;
    // Block content
    let mut elements = vec![];
    
    loop {
      let ParsedSequence { 
        sequence, 
        end_type, 
        is_printing, 
        extras 
      } = self.parse_sequence(SequenceParseMode::BlockElement)?;
      
      auto_hint |= is_printing;

      let element = BlockElement {
        main: Rc::new(sequence),
        weight: if let Some(ParsedSequenceExtras::WeightedBlockElement { weight_expr }) = extras {
          is_weighted = true;
          // Optimize constant weights
          Some(match (weight_expr.len(), weight_expr.first().map(Rc::as_ref)) {
            (1, Some(Rst::Integer(n))) => BlockWeight::Constant(*n as f64),
            (1, Some(Rst::Float(n))) => BlockWeight::Constant(*n),
            _ => BlockWeight::Dynamic(weight_expr)
          })
        } else {
          None
        },
      };
      
      match end_type {
        SequenceEndType::BlockDelim => {
          elements.push(element);
        },
        SequenceEndType::BlockEnd => {
          elements.push(element);
          break
        },
        SequenceEndType::ProgramEnd => {
          // Hard error if block isn't closed
          let err_span = start_pos .. self.source.len();
          self.report_error(Problem::UnclosedBlock, &err_span);
          return Err(())
        },
        _ => unreachable!()
      }
    }
    
    // Figure out the printflag before returning the block
    if auto_hint && flag != PrintFlag::Sink {
      Ok(Block::new(PrintFlag::Hint, is_weighted, elements))
    } else {
      Ok(Block::new(flag, is_weighted, elements))
    }
  }
  
  /// Parses an identifier.
  fn parse_ident(&mut self) -> ParseResult<Identifier> {
    if let Some((token, span)) = self.reader.next_solid() {
      match token {
        RantToken::Fragment => {
          let idstr = self.reader.last_token_string();
          if !is_valid_ident(idstr.as_str()) {
            self.report_error(Problem::InvalidIdentifier(idstr.to_string()), &span);
          }
          Ok(Identifier::new(idstr))
        },
        _ => {
          self.unexpected_last_token_error();
          Err(())
        }
      }
    } else {
      self.report_error(Problem::MissingIdentifier, &self.reader.last_token_span());
      Err(())
    }
  }

  #[inline]
  fn track_variable(&mut self, id: &Identifier, access_kind: &AccessPathKind, is_const: bool, role: VarRole, def_span: &Range<usize>) {
    // Check if there's already a variable with this name
    let (prev_tracker, requested_depth, found_depth) = match access_kind {        
      AccessPathKind::Local => {
        (self.var_stack.get(id), 0, self.var_stack.depth_of(id))
      },
      AccessPathKind::Descope(n) => {
        let (v, d) = self.var_stack
          .get_parent_depth(id, *n)
          .map(|(v, d)| (Some(v), Some(d)))
          .unwrap_or_default();
        (v, *n, d)
      },
      AccessPathKind::ExplicitGlobal => {
        let rd = self.var_stack.depth();
        let (v, d) = self.var_stack
          .get_parent_depth(id, rd)
          .map(|(v, d)| (Some(v), Some(d)))
          .unwrap_or_default();
        (v, rd, d)
      },
    };

    // Check for constant redef
    if let Some(prev_tracker) = prev_tracker {
      if prev_tracker.is_const && found_depth == Some(requested_depth) {
        self.report_error(Problem::ConstantRedefinition(id.to_string()), def_span);
      }
    }

    // Create variable tracking info
    let v = VarStats {
      writes: 0,
      reads: 0,
      def_span: def_span.clone(),
      is_const,
      role,
    };

    // Add to stack
    match access_kind {
      AccessPathKind::Local => {
        self.var_stack.define(id.clone(), v);
      },
      AccessPathKind::Descope(n) => {
        self.var_stack.define_parent(id.clone(), v, *n);
      },
      AccessPathKind::ExplicitGlobal => {
        self.var_stack.define_parent(id.clone(), v, self.var_stack.depth());
      },
    }
  }

  #[inline]
  fn track_variable_access(&mut self, path: &AccessPath, is_write: bool, span: &Range<usize>) {
    // Handle access stats
    if let Some(id) = &path.var_name() {
      let tracker = match path.kind() {
        AccessPathKind::Local => {
          self.var_stack.get_mut(id)
        },
        AccessPathKind::Descope(n) => {
          self.var_stack.get_parent_mut(id, n)
        },
        AccessPathKind::ExplicitGlobal => {
          self.var_stack.get_parent_mut(id, self.var_stack.depth())
        }
      };

      // Update tracker
      if let Some(tracker) = tracker {
        if is_write {
          tracker.writes += 1;

          if tracker.is_const {
            self.report_error(Problem::ConstantReassignment(id.to_string()), span);
          }
        } else {
          tracker.reads += 1;
        }
      }
    }
    
    // Handle captures
    if path.kind().is_local() {
      // At least one capture frame must exist
      if let Some((capture_frame_height, captures)) = self.capture_stack.last_mut() {
        // Must be accessing a variable
        if let Some(id) = path.var_name() {
          // Variable must not exist in the current scope of the active function
          if self.var_stack.height_of(&id).unwrap_or_default() < *capture_frame_height {
            captures.insert(id);
          }
        }
      }
    }
  }

  #[inline]
  fn analyze_top_vars(&mut self) {
    let mut unused_vars: Vec<(String, VarRole, Range<usize>)> = vec![];

    // Can't warn inside the loop due to bOrRoWiNg RuLeS!
    // Have to store the warning contents in a vec first...
    for (id, tracker) in self.var_stack.iter_top() {
      if tracker.reads == 0 {
        unused_vars.push((id.to_string(), tracker.role, tracker.def_span.clone()));
      }
    }

    // Generate warnings
    unused_vars.sort_by(|(.., a_span), (.., b_span)| a_span.start.cmp(&b_span.start));
    for (name, role, span) in unused_vars {
      match role {
        VarRole::Normal => self.report_warning(Problem::UnusedVariable(name), &span),
        VarRole::Parameter => self.report_warning(Problem::UnusedParameter(name), &span),
        VarRole::Function => self.report_warning(Problem::UnusedFunction(name), &span),
        // Ignore any other roles
        _ => {},
      }
    }
  }
    
  /// Parses one or more accessors (getter/setter/definition).
  #[inline(always)]
  fn parse_accessor(&mut self) -> ParseResult<Vec<Rst>> {
    let mut accessors = vec![];

    macro_rules! add_accessor {
      ($rst:expr) => {{
        let rst = $rst;
        accessors.push(rst);
      }}
    }
    
    'read: loop {      
      self.reader.skip_ws();

      // Check if the accessor ends here as long as there's at least one component
      if !accessors.is_empty() && self.reader.eat_where(|t| matches!(t, Some((RantToken::RightAngle, ..)))) {
        break
      }
      
      let (is_def, is_const_def) = if let Some((def_token, ..)) 
      = self.reader.take_where(|t| matches!(t, Some((RantToken::Dollar, ..)) | Some((RantToken::Percent, ..)))) {
        match def_token {
          // Variable declaration
          RantToken::Dollar => (true, false),
          // Constant declaration
          RantToken::Percent => (true, true),
          _ => unreachable!()
        }
      } else {
        (false, false)
      };

      let access_start_span = self.reader.last_token_span();

      self.reader.skip_ws();
      
      // Check if it's a definition. If not, it's a getter or setter
      if is_def {
        // Check for accessor modifiers
        let access_kind = self.parse_access_path_kind();
        self.reader.skip_ws();
        // Read name of variable we're defining
        let var_name = self.parse_ident()?;

        let def_span = access_start_span.start .. self.reader.last_token_span().end;
        
        if let Some((token, _token_span)) = self.reader.next_solid() {
          match token {
            // Empty definition
            RantToken::RightAngle => {              
              if is_const_def {
                self.track_variable(&var_name, &access_kind, true, VarRole::Normal, &def_span);
                add_accessor!(Rst::ConstDef(var_name, access_kind, None));
              } else {
                self.track_variable(&var_name, &access_kind, false, VarRole::Normal, &def_span);
                add_accessor!(Rst::VarDef(var_name, access_kind, None));
              }
              break 'read
            },
            // Accessor delimiter
            RantToken::Semi => {
              if is_const_def {
                self.track_variable(&var_name, &access_kind, true, VarRole::Normal, &def_span);
                add_accessor!(Rst::ConstDef(var_name, access_kind, None));
              } else {
                self.track_variable(&var_name, &access_kind, false, VarRole::Normal, &def_span);
                add_accessor!(Rst::VarDef(var_name, access_kind, None));
              }
              continue 'read;
            },
            // Definition and assignment
            RantToken::Equals => {
              self.reader.skip_ws();
              let ParsedSequence { 
                sequence: setter_expr, 
                end_type: setter_end_type, 
                .. 
              } = self.parse_sequence(SequenceParseMode::VariableAssignment)?;

              let def_span = access_start_span.start .. self.reader.last_token_span().start;
              if is_const_def {
                self.track_variable(&var_name, &access_kind, true, VarRole::Normal, &def_span);
                add_accessor!(Rst::ConstDef(var_name, access_kind, Some(Rc::new(setter_expr))));
              } else {
                self.track_variable(&var_name, &access_kind, false, VarRole::Normal, &def_span);
                add_accessor!(Rst::VarDef(var_name, access_kind, Some(Rc::new(setter_expr))));
              }
              
              match setter_end_type {
                SequenceEndType::VariableAssignDelim => {
                  continue 'read
                },
                SequenceEndType::VariableAccessEnd => {
                  break 'read
                },
                SequenceEndType::ProgramEnd => {
                  self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
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
          self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
          return Err(())
        }
      } else {
        // Read the path to what we're accessing
        let (var_path, var_path_span) = self.parse_access_path(true)?;
        
        if let Some((token, _)) = self.reader.next_solid() {
          match token {
            // If we hit a '>', it's a getter
            RantToken::RightAngle => {
              self.track_variable_access(&var_path, false, &var_path_span);
              add_accessor!(Rst::VarGet(Rc::new(var_path), None));
              break 'read;
            },
            // If we hit a ';', it's a getter with another accessor chained after it
            RantToken::Semi => {
              self.track_variable_access(&var_path, false, &var_path_span);
              add_accessor!(Rst::VarGet(Rc::new(var_path), None));
              continue 'read;
            },
            // If we hit a `?`, it's a getter with a fallback
            RantToken::Question => {
              self.reader.skip_ws();
              let ParsedSequence {
                sequence: fallback_expr,
                end_type: fallback_end_type,
                ..
              } = self.parse_sequence(SequenceParseMode::AccessorFallbackValue)?;

              self.track_variable_access(&var_path, false, &var_path_span);
              add_accessor!(Rst::VarGet(Rc::new(var_path), Some(Rc::new(fallback_expr))));

              match fallback_end_type {
                SequenceEndType::AccessorFallbackValueToDelim => continue 'read,
                SequenceEndType::AccessorFallbackValueToEnd => break 'read,
                // Error
                SequenceEndType::ProgramEnd => {
                  self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // If we hit a '=' here, it's a setter
            RantToken::Equals => {
              self.reader.skip_ws();
              let ParsedSequence {
                sequence: setter_rhs_expr,
                end_type: setter_rhs_end,
                ..
              } = self.parse_sequence(SequenceParseMode::VariableAssignment)?;
              let assign_end_span = self.reader.last_token_span();
              let setter_span = super_range(&access_start_span, &assign_end_span);
              // Don't allow setters directly on anonymous values
              if var_path.is_anonymous() && var_path.len() == 1 {
                self.report_error(Problem::AnonValueAssignment, &setter_span);
              }

              self.track_variable_access(&var_path, true, &setter_span);
              add_accessor!(Rst::VarSet(Rc::new(var_path), Rc::new(setter_rhs_expr)));

              match setter_rhs_end {
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
                  self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
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
          self.report_error(Problem::UnclosedVariableAccess, &self.reader.last_token_span());
          return Err(())
        }
      }
    }
    
    Ok(accessors)
  }
}