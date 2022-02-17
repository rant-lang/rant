#![allow(dead_code)]
#![allow(clippy::ptr_arg)]

use super::{reader::RantTokenReader, lexer::*, message::*, Problem, Reporter};
use crate::{InternalString, RantProgramInfo, lang::*};
use fnv::FnvBuildHasher;
use line_col::LineColLookup;
use quickscope::ScopeMap;
use std::{collections::{HashMap, HashSet}, ops::Range, rc::Rc};
use RantToken::*;

type ParseResult<T> = Result<T, ()>;

const MAIN_PROGRAM_SCOPE_NAME: &str = "main scope";

// Infix precedence categories
const PREC_PREFIX: usize = 9;
const PREC_EXPONENTIAL: usize = 8;
const PREC_MULTIPLICATIVE: usize = 7;
const PREC_ADDITIVE: usize = 6;
const PREC_RELATIONAL: usize = 5;
const PREC_EQUALITY: usize = 4;
const PREC_CONJUNCTIVE: usize = 3;
const PREC_XOR: usize = 2;
const PREC_DISJUNCTIVE: usize = 1;
const PREC_SEQUENCE: usize = 0;

#[derive(Copy, Clone, PartialEq)]
enum Op {
  Infix(InfixOp),
  Prefix(PrefixOp),
}

#[derive(Copy, Clone, PartialEq)]
enum PrefixOp {
  Negate,
  LogicNot,
}

#[derive(Copy, Clone, PartialEq)]
enum InfixOp {
  Add,
  Subtract,
  Multiply,
  Divide,
  Modulo,
  Power,
  LogicAnd,
  LogicOr,
  LogicXor,
  Equals,
  NotEquals,
  Greater,
  GreatOrEqual,
  Less,
  LessOrEqual,
}

impl InfixOp {
  #[inline]
  fn precedence(&self) -> usize {
    match self {
      Self::Power =>         PREC_EXPONENTIAL,
      Self::Multiply =>      PREC_MULTIPLICATIVE,
      Self::Divide =>        PREC_MULTIPLICATIVE,
      Self::Modulo =>        PREC_MULTIPLICATIVE,
      Self::Add =>           PREC_ADDITIVE,
      Self::Subtract =>      PREC_ADDITIVE,
      Self::Greater =>       PREC_RELATIONAL,
      Self::GreatOrEqual =>  PREC_RELATIONAL,
      Self::Less =>          PREC_RELATIONAL,
      Self::LessOrEqual =>   PREC_RELATIONAL,
      Self::Equals =>        PREC_EQUALITY,
      Self::NotEquals =>     PREC_EQUALITY,
      Self::LogicAnd =>      PREC_CONJUNCTIVE,
      Self::LogicXor =>      PREC_XOR,
      Self::LogicOr =>       PREC_DISJUNCTIVE,
    }
  }
}

#[derive(Copy, Clone, PartialEq)]
enum OpType {
  Prefix,
  Infix,
}

impl Op {

  #[inline]
  fn is_kw_supported(kw_name: &str) -> bool {
    matches!(kw_name, KW_NEG | KW_NOT | KW_EQ | KW_NEQ | KW_GT | KW_GE | KW_LT | KW_LE)
  }

  #[inline]
  fn op_type(&self) -> OpType {
    match self {
      Self::Infix(_) => OpType::Infix,
      Self::Prefix(_) => OpType::Prefix,
    }
  }

  #[inline(always)]
  fn from_token(token: &RantToken) -> Option<Op> {
    Some(match token {
      Plus =>               Self::Infix(InfixOp::Add),
      Minus =>              Self::Infix(InfixOp::Subtract),
      Star =>               Self::Infix(InfixOp::Multiply),
      Slash =>              Self::Infix(InfixOp::Divide),
      Percent =>            Self::Infix(InfixOp::Modulo),
      DoubleStar =>         Self::Infix(InfixOp::Power),
      And =>                Self::Infix(InfixOp::LogicAnd),
      VertBar =>            Self::Infix(InfixOp::LogicOr),
      Caret =>              Self::Infix(InfixOp::LogicXor),
      Keyword(kw) if kw.is_valid => match kw.name.as_str() {
        KW_NEG =>       Self::Prefix(PrefixOp::Negate),
        KW_NOT =>       Self::Prefix(PrefixOp::LogicNot),
        KW_EQ =>        Self::Infix(InfixOp::Equals),
        KW_NEQ =>       Self::Infix(InfixOp::NotEquals),
        KW_GT =>        Self::Infix(InfixOp::Greater),
        KW_GE =>        Self::Infix(InfixOp::GreatOrEqual),
        KW_LT =>        Self::Infix(InfixOp::Less),
        KW_LE =>        Self::Infix(InfixOp::LessOrEqual),
        _ => return None,
      },
      _ => return None,
    })
  }

  #[inline]
  fn precedence(&self) -> usize {
    match self {
      Self::Prefix(_) =>          PREC_PREFIX,
      Self::Infix(op) =>  op.precedence(),
    }
  }
}

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
  /// Breaks on `Semi`, `PipeOp`, and `RightBracket`.
  FunctionArg,
  /// Parse a sequence like a function body.
  ///
  /// Breaks on `RightBrace`.
  FunctionBodyBlock,
  /// Parse a sequence like a dynamic key expression.
  ///
  /// Breaks on `RightBrace`.
  DynamicKey,
  /// Parse a sequence like a variable assignment value.
  ///
  /// Breaks on `RightAngle` and `Semi`.
  VariableAssignment,
  /// Parse a sequence like an accessor fallback value.
  ///
  /// Breaks on `RightAngle` and `Semi`.
  AccessorFallbackValue,
  /// Parses a sequence like a parameter default value.
  ///
  /// Breaks on `RightBracket` and `Semi`.
  ParamDefaultValue,
  /// Parse a sequence like a collection initializer element.
  ///
  /// Breaks on `Semi` and `RightParen`.
  CollectionInit,
  /// Parses an if-statement condition.
  /// 
  /// Breaks on `Colon`.
  Condition,
  /// Infix right-hand side
  InfixRhs,
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
  /// Function argument sequence was terminated by `PipeOp`.
  FunctionArgEndToPipe,
  /// Function argument sequence was terminated by `RightAngle`.
  FunctionArgEndToAssignPipe,
  /// Function body sequence was terminated by `RightBrace`.
  FunctionBodyEnd,
  /// Dynamic key sequencce was terminated by `RightBrace`.
  DynamicKeyEnd,
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
  /// Parameter default value was terminated by `Semi`, indicating another parameter follows.
  ParamDefaultValueSeparator,
  /// Parameter default value was terminated by `RightBracket`, indicating the end of the signature was reached..
  ParamDefaultValueSignatureEnd,
  /// Sequence terminated by infix operator of lower precedence
  Operator,
  /// Condition was terminated by `Colon`.
  ConditionEnd,
}

/// Used to track variable usages during compilation.
struct VarStats {
  def_span: Range<usize>,
  writes: usize,
  reads: usize,
  /// Indicates whether the reads from this variable are fallible (meaning the variable isn't guaranteed to be defined).
  ///
  /// For optional parameters without a fallback this is `true`.
  has_fallible_read: bool,
  is_const: bool,
  is_auto_hinted: bool,
  role: VarRole,
}

impl VarStats {
  #[inline]
  fn new(role: VarRole, def_span: Range<usize>, is_const: bool) -> Self {
    Self {
      role,
      def_span,
      is_const,
      has_fallible_read: false,
      is_auto_hinted: false,
      reads: 0,
      writes: 0,
    }
  }

  #[inline]
  fn with_auto_hint(self) -> Self {
    Self {
      is_auto_hinted: true,
      .. self
    }
  }

  #[inline]
  fn with_fallible_read(self) -> Self {
    Self {
      has_fallible_read: true,
      .. self
    }
  }

  #[inline]
  fn add_write(&mut self) {
    self.writes += 1;
  }

  #[inline]
  fn add_read(&mut self, is_fallible_read: bool) {
    if matches!(self.role, VarRole::FallibleOptionalArgument) && is_fallible_read {
      self.has_fallible_read = true;
    }
    self.reads += 1;
  }
}

/// Provides context to a variable tracked by the compiler.
#[derive(Copy, Clone, PartialEq)]
enum VarRole {
  /// Just a normal variable.
  Normal,
  /// A variable known to be a function.
  Function,
  /// Function argument.
  Argument,
  /// Optional function argument without a fallback.
  /// If accessed without a fallback expression, should generate a compiler error.
  FallibleOptionalArgument,
  /// Pipeval from a piped call.
  PipeValue,
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

// TODO: Add a helper function for creating simple instances of `ParsedSequence` to reduce code bloat
/// Contains information about a successfully parsed sequence and its context.
struct ParsedSequence {
  sequence: Sequence,
  end_type: SequenceEndType,
  is_auto_hinted: bool,
  extras: Option<ParsedSequenceExtras>,
  next_infix_op: Option<Op>,
}

#[derive(Copy, Clone, PartialEq)]
enum BlockParseMode {
  NeedsStart,
  StartParsed(Option<BlockProtection>),
}

/// Contains information about a successfully parsed block.
struct ParsedBlock {
  is_auto_hinted: bool,
  block: Block
}

/// Contains information about a successfully parsed accessor.
struct ParsedAccessor {
  nodes: Vec<Expression>,
  is_auto_hinted: bool
}

/// Contains information about a successfully parsed function accessor.
struct ParsedFunctionAccessor {
  node: Expression,
  is_auto_hinted: bool
}

/// Contains information about a successfully parsed function parameter.
struct ParsedParameter {
  param: Parameter,
  span: Range<usize>,
  is_auto_hinted: bool
}

/// Contains information about a successfully parsed conditional.
struct ParsedConditional {
  node: Expression,
  is_auto_hinted: bool,
}

/// Contains information about a successfully parsed parenthetical expression or collection initializer.
struct ParsedGroupOrCollection {
  node: Expression,
  is_collection: bool,
  is_auto_hinted: bool,
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
  fn report_unexpected_last_token_error(&mut self) {
    self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &self.reader.last_token_span())
  }

  #[inline]
  fn report_unexpected_token_error(&mut self, span: &Range<usize>) {
    self.report_error(Problem::UnexpectedToken(self.source[span.start .. span.end].to_string()), span)
  }

  #[inline]
  fn report_expected_token_error(&mut self, expected: &str, span: &Range<usize>) {
    self.report_error(Problem::ExpectedToken(expected.to_owned()), span);
  }

  /// Parses a sequence of items with a new variable scope. Items are individual elements of a Rant program (fragments, blocks, function calls, etc.)
  #[inline]
  fn parse_sequence(&mut self, mode: SequenceParseMode) -> ParseResult<ParsedSequence> {
    self.var_stack.push_layer();
    let parsed_seq = self.parse_sequence_inner(mode, PREC_SEQUENCE)?;
    self.analyze_top_vars();
    self.var_stack.pop_layer();
    Ok(parsed_seq)
  }
  
  /// Inner logic of `parse_sequence()`. Intended to be wrapped in other specialized sequence-parsing functions.
  fn parse_sequence_inner(&mut self, mode: SequenceParseMode, precedence: usize) -> ParseResult<ParsedSequence> {
    let mut sequence = Sequence::empty(&self.info);
    let mut next_print_flag = PrintFlag::None;
    let mut last_print_flag_span: Option<Range<usize>> = None;
    let mut is_seq_text = false;
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
    
    #[allow(unused_assignments)] // whitespace macro needs this, but https://github.com/rust-lang/rust/issues/15701
    while let Some((token, span)) = self.reader.next() {
      let _debug_inject_toggle = true;

      macro_rules! whitespace {
        (allow) => {{
          if let Some(ws) = pending_whitespace.take() {
            if is_seq_text {
              emit!(Expression::Whitespace(ws));
            }
          }
        }};
        (queue next) => {{
          if let Some((Whitespace, ..)) = self.reader.take_where(|tt| matches!(tt, Some((Whitespace, ..)))) {
            pending_whitespace = Some(self.reader.last_token_string());
          }
        }};
        (queue $ws:expr) => {
          pending_whitespace = Some($ws);
        };
        (ignore prev) => {{
          pending_whitespace = None;
        }};
        (ignore next) => {
          self.reader.skip_ws()
        };
        (ignore both) => {{
          whitespace!(ignore prev);
          whitespace!(ignore next);
        }};
      }

      macro_rules! no_debug {
        ($e:expr) => {{
          let _debug_inject_toggle = false;
          $e
        }}
      }

      macro_rules! inject_debug_info {
        () => {
          if debug && _debug_inject_toggle {
            let (line, col) = self.lookup.get(span.start);
            sequence.push(Rc::new(Expression::DebugCursor(DebugInfo::Location { line, col })));
          }
        }
      }
      
      // Macro for prohibiting hints/sinks before certain tokens
      macro_rules! no_flags {
        () => {
          if (!matches!(next_print_flag, PrintFlag::None)) {
            if let Some(flag_span) = last_print_flag_span.take() {
              self.report_error(match next_print_flag {
                PrintFlag::Hint => Problem::InvalidHint,
                PrintFlag::Sink => Problem::InvalidSink,
                PrintFlag::None => unreachable!()
              }, &flag_span)
            }
          }
        };
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
          sequence.push(Rc::new(Expression::Fragment(InternalString::from(self.reader.last_token_string()))));
        }}
      }
      
      // Shortcut macro for "unexpected token" error
      macro_rules! unexpected_token_error {
        () => {
          self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span)
        };
      }

      /// Eats as many fragments / escape sequences as possible and combines their string representations into the input `String`.
      macro_rules! consume_fragments {
        ($s:ident) => {
          while let Some((token, span)) = self.reader.take_where(|t| matches!(t, Some((Escape(..) | Fragment, ..)))) {
            match token {
              Escape(ch) => match ch {
                ParsedEscape::Char(ch) => $s.push(ch),
                ParsedEscape::InvalidChar(ch) => {
                  self.report_error(Problem::InvalidEscapeChar(ch), &span);
                },
                ParsedEscape::InvalidUnicode(cp) => {
                  self.report_error(Problem::InvalidEscapeCodePoint(cp), &span);
                },
              },
              _ => $s.push_str(&self.reader.last_token_string()),
            }
          }
        }
      }
      
      // Parse next sequence item
      match token {
        
        // Hint
        Hint => no_flags!({
          whitespace!(allow);
          is_seq_text = true;
          next_print_flag = PrintFlag::Hint;
          last_print_flag_span = Some(span.clone());
          continue
        }),
        
        // Sink
        Sink => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          next_print_flag = PrintFlag::Sink;
          last_print_flag_span = Some(span.clone());
          continue
        }),

        // Prefix keyword
        Keyword(KeywordInfo { name: kw, is_valid: is_kw_valid }) if is_kw_valid && !Op::is_kw_supported(kw.as_str()) => {
          let kwstr = kw.as_str();
          match kwstr {            
            // Boolean constants
            KW_TRUE => no_debug!(no_flags!(on {
              whitespace!(ignore both);
              Expression::Boolean(true)
            })),
            KW_FALSE => no_debug!(no_flags!(on {
              whitespace!(ignore both);
              Expression::Boolean(false)
            })),
            KW_IF => {
              whitespace!(allow);              
              let ParsedConditional {
                node: cond_node,
                is_auto_hinted: cond_auto_hinted,
              } = self.parse_conditional(false)?;

              is_seq_text |= cond_auto_hinted;
              emit!(cond_node);
            },
            // @require statement
            KW_REQUIRE => no_flags!({
              whitespace!(ignore both);
              whitespace!(ignore next);
              if let Some((require_first_arg_token, require_first_arg_token_span)) = self.reader.next() {                
                match require_first_arg_token {
                  RantToken::StringLiteral(require_path) => {
                    let node = Expression::Require {
                      alias: None,
                      path: require_path,
                    };
                    whitespace!(ignore next);
                    emit!(node);
                  },
                  RantToken::Fragment => {
                    let require_alias = self.reader.last_token_string();
                    if !is_valid_ident(require_alias.as_str()) {
                      self.report_error(Problem::InvalidIdentifier(require_alias.to_string()), &require_first_arg_token_span);
                    }
                    whitespace!(ignore next);
                    if !self.reader.eat_where(|t| matches!(t, Some((RantToken::Colon, _)))) {
                      self.report_error(Problem::ExpectedToken(":".to_owned()), &self.reader.last_token_span());
                      continue
                    }
                    if let Some((require_path_token, require_path_token_span)) = self.reader.next_solid() {
                      whitespace!(ignore next);
                      match require_path_token {
                        RantToken::StringLiteral(require_path) => {
                          let node = Expression::Require {
                            alias: Some(require_alias),
                            path: require_path,
                          };
                          emit!(node);
                        },
                        _ => self.report_error(Problem::InvalidRequireArgumentToken, &require_path_token_span)
                      }
                    } else {
                      self.report_error(Problem::MissingRequireArgument, &self.reader.last_token_span());
                    }
                  },
                  _ => unexpected_token_error!()
                }
              } else {
                self.report_error(Problem::MissingRequireArgument, &self.reader.last_token_span());
                return Err(())
              }
            }),

            // Control flow
            KW_RETURN | KW_CONTINUE | KW_BREAK | KW_WEIGHT => {
              whitespace!(ignore both);
              let ParsedSequence {
                sequence: charm_sequence,
                end_type: charm_end_type,
                is_auto_hinted: is_charm_printing,
                extras: mut charm_extras,
                ..
              } = self.parse_sequence(mode)?;
              let charm_sequence_name = charm_sequence.name.clone();
              let charm_sequence = (!charm_sequence.is_empty()).then(|| Rc::new(charm_sequence));
              match kw.as_str() {
                KW_RETURN => emit!(Expression::Return(charm_sequence)),
                KW_CONTINUE => emit!(Expression::Continue(charm_sequence)),
                KW_BREAK => emit!(Expression::Break(charm_sequence)),
                KW_WEIGHT => {
                  if mode == SequenceParseMode::BlockElement {
                    charm_extras = Some(ParsedSequenceExtras::WeightedBlockElement {
                      weight_expr: charm_sequence.unwrap_or_else(|| Rc::new(Sequence::empty(&self.info)))
                    });
                  } else {
                    self.report_error(Problem::WeightNotAllowed, &span);
                  }
                },
                _ => unreachable!()
              };
              check_dangling_printflags!();
              return Ok(ParsedSequence {
                sequence: if let Some(charm_sequence_name) = charm_sequence_name {
                  sequence.with_name(charm_sequence_name)
                } else {
                  sequence
                },
                end_type: charm_end_type,
                is_auto_hinted: is_charm_printing || is_seq_text,
                extras: charm_extras,
                next_infix_op: None,
              })
            },
            _ => self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span),
          }          
        },
        
        // Block start
        LeftBrace => {
          // Read in the entire block
          let parsed_block = self.parse_block(BlockParseMode::StartParsed(None))?;

          // Decide what to do with previous whitespace
          match next_print_flag {                        
            // If hinted, allow pending whitespace
            PrintFlag::Hint => {
              whitespace!(allow);
              is_seq_text = true;
            },
            
            // If sinked, delete pending whitespace
            PrintFlag::Sink => whitespace!(ignore both),
            
            // If no flag, infer from block contents
            PrintFlag::None => {
              if parsed_block.is_auto_hinted {
                whitespace!(allow);
                is_seq_text = true;
              } else {
                whitespace!(ignore both)
              }
            }
          }
          
          emit!(Expression::Block(Rc::new(parsed_block.block)));
        },

        // Pipe operator
        PipeOp => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          match mode {
            SequenceParseMode::FunctionArg => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("argument"),
                end_type: SequenceEndType::FunctionArgEndToPipe,
                is_auto_hinted: is_seq_text,
                extras: None,
                next_infix_op: None,
              })
            },
            _ => unexpected_token_error!()
          }
        }),

        // Pipe value
        PipeValue => {
          emit!(Expression::PipeValue);
          self.track_pipeval_read(&span);
        },
        
        // Block element delimiter (when in block parsing mode)
        VertBar if mode == SequenceParseMode::BlockElement => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          return Ok(ParsedSequence {
            sequence: sequence.with_name_str("block element"),
            end_type: SequenceEndType::BlockDelim,
            is_auto_hinted: is_seq_text,
            extras: None,
            next_infix_op: None,
          })
        }),
        
        // Block/func body/dynamic key end
        RightBrace => no_flags!({
          // Ignore pending whitespace
          whitespace!(ignore prev);
          match mode {
            SequenceParseMode::BlockElement => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("block element"),
                end_type: SequenceEndType::BlockEnd,
                is_auto_hinted: is_seq_text,
                extras: None,
                next_infix_op: None,
              })
            },
            SequenceParseMode::FunctionBodyBlock => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("function body"),
                end_type: SequenceEndType::FunctionBodyEnd,
                is_auto_hinted: is_seq_text,
                extras: None,
                next_infix_op: None,
              })
            },
            _ => unexpected_token_error!()
          }
        }),
        
        // Map initializer or protected block
        At => {
          match self.reader.next_solid() {
            Some((LeftBrace, _)) => {
              // Read in the entire block
              let parsed_block = self.parse_block(BlockParseMode::StartParsed(Some(BlockProtection::Outer)))?;

              // Decide what to do with previous whitespace
              match next_print_flag {                        
                // If hinted, allow pending whitespace
                PrintFlag::Hint => {
                  whitespace!(allow);
                  is_seq_text = true;
                },
                
                // If sinked, delete pending whitespace
                PrintFlag::Sink => whitespace!(ignore both),
                
                // If no flag, infer from block contents
                PrintFlag::None => {
                  if parsed_block.is_auto_hinted {
                    whitespace!(allow);
                    is_seq_text = true;
                  } else {
                    whitespace!(ignore both)
                  }
                }
              }
              
              emit!(Expression::Block(Rc::new(parsed_block.block)));
            }
            _ => {
              // TODO: Use a more descriptive error here
              self.report_unexpected_last_token_error();
              emit!(Expression::NothingVal);
            },
          }
        },
        
        // Group or collection initializer
        LeftParen => {
          let ParsedGroupOrCollection {
            node,
            is_collection,
            is_auto_hinted
          } = self.parse_group_or_collection_init(&span)?;

          if is_collection {
            // Flags are redundant on collection initializers since they're always non-text
            no_flags!();
          } else if is_seq_text && is_auto_hinted {
            whitespace!(allow);
          }

          emit!(node);
        },
        
        // Collection init termination
        RightParen => no_flags!({
          match mode {
            SequenceParseMode::DynamicKey => {
              return Ok(ParsedSequence {
                sequence: sequence.with_name_str("dynamic key"),
                end_type: SequenceEndType::DynamicKeyEnd,
                is_auto_hinted: is_seq_text,
                extras: None,
                next_infix_op: None,
              })
            },
            SequenceParseMode::CollectionInit => {
              return Ok(ParsedSequence {
                sequence,
                end_type: SequenceEndType::CollectionInitEnd,
                is_auto_hinted: true,
                extras: None,
                next_infix_op: None,
              })
            },
            _ => unexpected_token_error!()
          }
        }),
        
        // Function creation or call
        LeftBracket => {
          let ParsedFunctionAccessor {
            node: func_access,
            is_auto_hinted
          } = self.parse_func_access()?;

          match next_print_flag {
            PrintFlag::Sink => whitespace!(ignore next),
            PrintFlag::None => {
              if is_auto_hinted {
                is_seq_text = true;
                whitespace!(allow);
              }
            },
            _ => whitespace!(allow)
          }
          
          // Definitions are implicitly sinked and ignore surrounding whitespace
          if let Expression::FuncDef(_) = func_access {
            no_flags!();
            whitespace!(ignore both);
          }
          
          emit!(func_access);
        },
        
        // Can be terminator for function args and anonymous function expressions
        RightBracket => no_flags!({
          match mode {
            SequenceParseMode::FunctionArg => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("argument"),
              end_type: SequenceEndType::FunctionArgEndBreak,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::ParamDefaultValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("default value"),
              end_type: SequenceEndType::ParamDefaultValueSignatureEnd,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            _ => unexpected_token_error!()
          }
        }),
        
        // Variable access start
        LeftAngle => {
          let ParsedAccessor {
            nodes,
            is_auto_hinted
          } = self.parse_accessor()?;

          if is_auto_hinted && matches!(next_print_flag, PrintFlag::None) {
            is_seq_text = true;
            whitespace!(allow);
          }

          for node in nodes {
            match node {
              Expression::Get(..) => {
                whitespace!(allow);
              },
              Expression::Set(..) | Expression::Define(..) => {
                // whitespace!(ignore both);
              },
              _ => unreachable!()
            }
            emit!(node);
          }
        },
        
        // Variable access end
        RightAngle => no_flags!({
          match mode {
            SequenceParseMode::FunctionArg => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("argument"),
              end_type: SequenceEndType::FunctionArgEndToAssignPipe,
              is_auto_hinted: is_seq_text,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::VariableAssignment => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("setter value"),
              end_type: SequenceEndType::VariableAccessEnd,
              is_auto_hinted: is_seq_text,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::AccessorFallbackValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("fallback value"),
              end_type: SequenceEndType::AccessorFallbackValueToEnd,
              is_auto_hinted: is_seq_text,
              extras: None,
              next_infix_op: None,
            }),
            _ => unexpected_token_error!()
          }
        }),

        // These symbols are only used in special contexts and can be safely printed
        Question | Dollar | Equals | DoubleDot
        => no_flags!(on {
          whitespace!(allow);
          is_seq_text = true;
          let frag = self.reader.last_token_string();
          Expression::Fragment(frag)
        }),
        
        // Fragment
        Fragment => no_flags!(on {
          whitespace!(allow);
          is_seq_text = true;
          let mut frag = self.reader.last_token_string();
          consume_fragments!(frag);
          Expression::Fragment(frag)
        }),
        
        // Whitespace (only if sequence isn't empty)
        Whitespace => no_flags!({
          // Don't set is_printing here; whitespace tokens always appear with other printing tokens
          if is_seq_text {
            let ws = self.reader.last_token_string();
            whitespace!(queue ws);
          }
        }),
        
        // Escape sequences
        Escape(esc) => {
          whitespace!(allow);
          is_seq_text = true;
          let mut frag = InternalString::new();
          match esc {
            ParsedEscape::Char(c) => {
              frag.push(c);
            },
            ParsedEscape::InvalidChar(c) => {
              self.report_error(Problem::InvalidEscapeChar(c), &span);    
            },
            ParsedEscape::InvalidUnicode(cp) => {
              self.report_error(Problem::InvalidEscapeCodePoint(cp), &span);
            },
          }
          consume_fragments!(frag);
          emit!(Expression::Fragment(frag))
        },

        // Minus (considered a const negation only if it's in text or at the start of a sequence)
        Minus if is_seq_text || sequence.is_empty() => {
          whitespace!(allow);
          whitespace!(queue next);
          // Check if it's supposed to be a negative number
          if let Some((number_token, number_token_span)) = self.reader.take_where(|t| matches!(t, Some((RantToken::IntegerPositive(_) | RantToken::FloatPositive(_), _)))) {
            whitespace!(ignore prev);
            emit!(match number_token {
              RantToken::IntegerPositive(nt) => match nt {
                PositiveIntegerToken::Value(n) => match self.try_sign_unsigned_int(n, true, &number_token_span) {
                  Ok(n) => Expression::Integer(n),
                  Err(_) => Expression::NothingVal
                },
                PositiveIntegerToken::OutOfRange => {
                  self.report_error(Problem::IntegerLiteralOutOfRange, &span);
                  Expression::NothingVal
                }
              },
              RantToken::FloatPositive(nt) => match nt {
                PositiveFloatToken::Value(n) => Expression::Float(-n),
                PositiveFloatToken::OutOfRange => {
                  self.report_error(Problem::FloatLiteralOutOfRange, &span);
                  Expression::NothingVal
                },
              },
              _ => unreachable!()
            });
          } else {
            whitespace!(allow);
          }
        },
        
        // Integers
        IntegerPositive(nt) => no_flags!(on {
          whitespace!(allow);
          match nt {
            PositiveIntegerToken::Value(n) => match cast::i64(n) {
              Ok(n) => Expression::Integer(n),
              Err(_) => {
                self.report_error(Problem::IntegerLiteralOutOfRange, &span);
                Expression::NothingVal
              }
            },
            PositiveIntegerToken::OutOfRange => {
              self.report_error(Problem::IntegerLiteralOutOfRange, &span);
              Expression::NothingVal
            }
          }
        }),
        
        // Floats
        FloatPositive(nt) => no_flags!(on {
          whitespace!(allow);
          match nt {
            PositiveFloatToken::Value(n) => Expression::Float(n),
            PositiveFloatToken::OutOfRange => {
              self.report_error(Problem::FloatLiteralOutOfRange, &span);
              Expression::NothingVal
            }
          }
        }),
        
        // Empty
        NothingLiteral => no_flags!(on {
          Expression::NothingVal
        }),
        
        // Verbatim string literals
        StringLiteral(s) => {
          whitespace!(allow);
          if next_print_flag == PrintFlag::Sink {
            whitespace!(ignore next);
          } else {
            is_seq_text = true;
          }
          emit!(Expression::Fragment(s))
        },
        
        // Colon can be either fragment or argument separator.
        Colon => no_flags!({
          match mode {
            SequenceParseMode::Condition => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("condition"),
              end_type: SequenceEndType::ConditionEnd,
              is_auto_hinted: is_seq_text,
              extras: None,
              next_infix_op: None,
            }),
            _ => emit_last_string!(),
          }
        }),
        
        // Semicolon can be a fragment, collection element separator, or argument separator.
        Semicolon => no_flags!({
          match mode {
            SequenceParseMode::FunctionArg => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("argument"),
              end_type: SequenceEndType::FunctionArgEndNext,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::CollectionInit => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("collection item"),
              end_type: SequenceEndType::CollectionInitDelim,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::VariableAssignment => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("variable assignment"),
              end_type: SequenceEndType::VariableAssignDelim,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::AccessorFallbackValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("fallback"),
              end_type: SequenceEndType::AccessorFallbackValueToDelim,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            SequenceParseMode::ParamDefaultValue => return Ok(ParsedSequence {
              sequence: sequence.with_name_str("default value"),
              end_type: SequenceEndType::ParamDefaultValueSeparator,
              is_auto_hinted: true,
              extras: None,
              next_infix_op: None,
            }),
            // If we're anywhere else, just print the semicolon like normal text
            _ => emit_last_string!(),
          }
        }),
        
        // Handle unclosed string literals as hard errors
        UnterminatedStringLiteral => {
          self.report_error(Problem::UnclosedStringLiteral, &span); 
          return Err(())
        },
        token => {          
          // Check for ordered operator
          // TODO: Split out this horror into its own function PLEASE
          if let Some(mut next_op) = Op::from_token(&token) {
            let mut op_end_type = SequenceEndType::Operator;
            loop {
              let op_precedence = next_op.precedence();
              match next_op {
                // Check for prefix operator
                Op::Prefix(prefix_op) => {
                  // Read operand for prefix op
                  let ParsedSequence {
                    sequence: operand_seq,
                    next_infix_op: operand_next_lower_infix_op,
                    is_auto_hinted: operand_is_text,
                    end_type: operand_end_type,
                    ..
                  } = self.parse_sequence_inner(mode, op_precedence)?;

                  op_end_type = operand_end_type;
                  is_seq_text |= operand_is_text;

                  // Make sure operand is not empty
                  if operand_seq.is_empty() {
                    self.report_error(Problem::MissingOperand, &span);
                  }

                  let operand = Rc::new(operand_seq);

                  let prefix_op_node = match prefix_op {
                    PrefixOp::Negate => {
                      whitespace!(allow);
                      Expression::Negate(operand)
                    },
                    PrefixOp::LogicNot => Expression::LogicNot(operand),
                  };
                  whitespace!(ignore prev);
                  emit!(prefix_op_node);

                  // Check if operand sequence signaled a lower precedence operator was read
                  // It might not be lower than the current context precedence,
                  // so we'll loop back and check that.
                  if let Some(operand_next_lower_infix_op) = operand_next_lower_infix_op {
                    next_op = operand_next_lower_infix_op;
                    continue
                  } else {
                    // Since there's no more operators it means we're done reading, so return the sequence.
                    return Ok(ParsedSequence {
                      sequence,
                      end_type: op_end_type,
                      is_auto_hinted: is_seq_text,
                      extras: None,
                      next_infix_op: None,
                    })
                  }
                },
                // Check for infix operator
                Op::Infix(infix_op) => {
                  whitespace!(ignore both);
                  // Make sure LHS is not empty
                  if sequence.is_empty() {
                    self.report_error(Problem::MissingLeftOperand, &span);
                  }
                  
                  // If this operator has lower or equal precedence, stop reading the sequence.
                  if op_precedence <= precedence {
                    break
                  }

                  // If the next operator has equal or higher precedence, replace the current sequence with the operation.
                  // The current sequence will then become the LHS, and the RHS is parsed according to the operator's precedence.

                  // Read RHS with higher precedence
                  let ParsedSequence {
                    sequence: rhs_seq,
                    next_infix_op: rhs_next_lower_infix_op,
                    is_auto_hinted: rhs_is_text,
                    end_type: rhs_end_type,
                    ..
                  } = self.parse_sequence_inner(mode, op_precedence)?;

                  op_end_type = rhs_end_type;

                  // Make sure RHS is not empty
                  if rhs_seq.is_empty() {
                    self.report_error(Problem::MissingRightOperand, &span);
                  }

                  // Take the current sequence as the LHS
                  let lhs = Rc::new(sequence);
                  let rhs = Rc::new(rhs_seq);              

                  // Produce infix operation node
                  let op_node = match infix_op {
                    InfixOp::Add => Expression::Add(lhs, rhs),
                    InfixOp::Subtract => Expression::Subtract(lhs, rhs),
                    InfixOp::Multiply => Expression::Multiply(lhs, rhs),
                    InfixOp::Divide => Expression::Divide(lhs, rhs),
                    InfixOp::Modulo => Expression::Modulo(lhs, rhs),
                    InfixOp::Power => Expression::Power(lhs, rhs),
                    InfixOp::LogicAnd => Expression::LogicAnd(lhs, rhs),
                    InfixOp::LogicOr => Expression::LogicOr(lhs, rhs),
                    InfixOp::LogicXor => Expression::LogicXor(lhs, rhs),
                    InfixOp::Equals => Expression::Equals(lhs, rhs),
                    InfixOp::NotEquals => Expression::NotEquals(lhs, rhs),
                    InfixOp::Greater => Expression::Greater(lhs, rhs),
                    InfixOp::GreatOrEqual => Expression::GreaterOrEqual(lhs, rhs),
                    InfixOp::Less => Expression::Less(lhs, rhs),
                    InfixOp::LessOrEqual => Expression::LessOrEqual(lhs, rhs),
                  };

                  // Replace current sequence with operation (containing old sequence as LHS)
                  sequence = Sequence::one(op_node, &self.info);              
                  is_seq_text |= rhs_is_text;
                  
                  // Check if RHS sequence signaled a lower precedence operator was read
                  // It might not be lower than the current context precedence,
                  // so we'll loop back and check that.
                  if let Some(rhs_next_lower_infix_op) = rhs_next_lower_infix_op {
                    next_op = rhs_next_lower_infix_op;
                    continue
                  } else {
                    // Since there's no more operators it means we're done reading, so return the sequence.
                    return Ok(ParsedSequence {
                      sequence,
                      end_type: op_end_type,
                      is_auto_hinted: is_seq_text,
                      extras: None,
                      next_infix_op: None,
                    })
                  }
                }
              }
            }

            // If the next operator has *lower* precedence, stop reading and return current sequence to the upper parser.
            // We return the operator as next_infix_op along with the sequence to tell upper parser which operator is next.
            // The upper parsers will continue to short-circuit as long as the next_infix_op's precedence is lower than theirs.
            return Ok(ParsedSequence {
              sequence,
              end_type: op_end_type,
              is_auto_hinted: is_seq_text,
              extras: None,
              next_infix_op: Some(next_op),
            })
          } else {
            match token {
              // Check if it's an unsupported keyword
              Keyword(kw) if !kw.is_valid => self.report_error(Problem::InvalidKeyword(kw.name.to_string()), &span),
              // If it's none of the above, we don't want it there
              _ => unexpected_token_error!()
            }
          }
        },
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
      is_auto_hinted: is_seq_text,
      extras: None,
      next_infix_op: None,
    })
  }

  fn track_pipeval_read(&mut self, span: &Range<usize>) {
    if let Some(pipeval) = self.var_stack.get_mut(PIPE_VALUE_NAME) {
      pipeval.add_read(false);
      // Handle capturing
      if let Some((capture_frame_height, captures)) = self.capture_stack.last_mut() {
        // Variable must not exist in the current scope of the active function
        if self.var_stack.height_of(PIPE_VALUE_NAME).unwrap_or_default() < *capture_frame_height {
          captures.insert(PIPE_VALUE_NAME.into());
        }
      }
    } else {
      self.report_error(Problem::NothingToPipe, span);
    }
  }

  #[inline]
  fn parse_conditional(&mut self, expect_opening_if: bool) -> ParseResult<ParsedConditional> {
    let mut conditions = vec![];
    let mut fallback = None;
    let mut is_auto_hinted = false;

    if expect_opening_if && !self.reader.eat_kw(KW_IF) {
      self.report_expected_token_error("@if", &self.reader.last_token_span());
    }

    self.reader.skip_ws();

    // Parse main condition
    let ParsedSequence {
      sequence: cond_seq,
      end_type: cond_end_type,
      ..
    } = self.parse_sequence_inner(SequenceParseMode::Condition, PREC_SEQUENCE)?;

    match cond_end_type {
      SequenceEndType::ConditionEnd => {},
      SequenceEndType::ProgramEnd => {
        self.report_error(Problem::UnclosedCondition, &self.reader.last_token_span());
        return Err(())
      },
      _ => unreachable!()
    }

    self.reader.skip_ws();

    let ParsedBlock {
      is_auto_hinted: body_auto_hint,
      block: body_block,
    } = self.parse_block(BlockParseMode::NeedsStart)?;

    is_auto_hinted |= body_auto_hint;

    conditions.push((Rc::new(cond_seq), Rc::new(body_block)));
    
    // Parse else-ifs
    self.reader.skip_ws();            
    while self.reader.eat_kw(KW_ELSEIF) {
      self.reader.skip_ws();

      let ParsedSequence {
        sequence: cond_seq,
        end_type: cond_end_type,
        ..
      } = self.parse_sequence_inner(SequenceParseMode::Condition, PREC_SEQUENCE)?;

      match cond_end_type {
        SequenceEndType::ConditionEnd => {},
        SequenceEndType::ProgramEnd => {
          self.report_error(Problem::UnclosedCondition, &self.reader.last_token_span());
          return Err(())
        },
        _ => unreachable!()
      }

      self.reader.skip_ws();

      let ParsedBlock {
        is_auto_hinted: body_auto_hint,
        block: body_block,
      } = self.parse_block(BlockParseMode::NeedsStart)?;

      is_auto_hinted |= body_auto_hint;

      conditions.push((Rc::new(cond_seq), Rc::new(body_block)));
      self.reader.skip_ws();
    }

    // Parse else
    self.reader.skip_ws();
    if self.reader.eat_kw(KW_ELSE) {
      self.reader.skip_ws();
      if !self.reader.eat_where(|t| matches!(t, Some((RantToken::Colon, _)))) {
        self.report_error(Problem::ExpectedToken(":".to_owned()), &self.reader.last_token_span());
      }
      
      self.reader.skip_ws();

      let ParsedBlock {
        is_auto_hinted: body_auto_hint,
        block: body_block,
      } = self.parse_block(BlockParseMode::NeedsStart)?;

      is_auto_hinted |= body_auto_hint;

      fallback = Some(Rc::new(body_block));
    }

    // Emit final conditional node
    let node = Expression::Conditional {
      conditions: Rc::new(conditions),
      fallback,
    };

    Ok(ParsedConditional {
      node,
      is_auto_hinted,
    })
  }
  
  /// Parses a group or list/map/tuple initializer.
  fn parse_group_or_collection_init(&mut self, start_span: &Range<usize>) -> ParseResult<ParsedGroupOrCollection> {
    self.reader.skip_ws();
    let collection_type_token_info = self.reader.take_where(|t| matches!(t, Some((RantToken::Colon | RantToken::DoubleColon, ..))));
    self.reader.skip_ws();

    if let Some((collection_type_token, _)) = collection_type_token_info {
      match collection_type_token {
        // Is it a list?
        RantToken::Colon => {
          // Exit early on empty list
          if self.reader.eat_where(|token| matches!(token, Some((RightParen, ..)))) {
            return Ok(ParsedGroupOrCollection {
              node: Expression::ListInit(Rc::new(vec![])),
              is_collection: true,
              is_auto_hinted: false,
            })
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

          // To allow trailing semicolons, remove the last element if its sequence is empty
          if let Some(seq) = sequences.last() {
            if seq.is_empty() {
              sequences.pop();
            }
          }

          Ok(ParsedGroupOrCollection {
            node: Expression::ListInit(Rc::new(sequences)),
            is_collection: true, 
            is_auto_hinted: false,
          })
        },
        // Is it a map?
        RantToken::DoubleColon => {
          let mut pairs = vec![];
        
          loop {
            let key_expr = match self.reader.next_solid() {
              // Allow blocks as dynamic keys
              Some((LeftParen, _)) => {
                Some(MapKeyExpr::Dynamic(Rc::new(self.parse_dynamic_key(false)?)))
              },
              // Allow getters as shorthands for both a key AND value, ONLY IF they target a variable
              Some((LeftAngle, _)) => {
                let start_span = self.reader.last_token_span();
                let ParsedAccessor {
                  nodes: mut accessor_nodes,
                  ..
                } = self.parse_accessor()?;
                let end_span = self.reader.last_token_span();
                let accessor_span = super_range(&start_span, &end_span);

                self.reader.skip_ws();
                
                // Ignore the separator since that's usually handled by the value sequence parser
                self.reader.eat(RantToken::Semicolon);

                if accessor_nodes.len() != 1 {
                  self.report_error(Problem::InvalidShorthandVariable, &accessor_span);
                  continue
                }

                let accessor = accessor_nodes.drain(..).next();
                match accessor {
                  Some(Expression::Get(getter)) => {
                    let mut is_valid_shorthand = false;
                    if getter.path.is_variable_target() {
                      if let Some(getter_var_name) = getter.path.var_name().as_ref().map(|v| v.as_str()) {
                        is_valid_shorthand = true;
                        let key = InternalString::from(getter_var_name);
                        let value_seq = Sequence::one(Expression::Get(getter), &self.info);
                        pairs.push((MapKeyExpr::Static(key), Rc::new(value_seq)));
                      }
                    }
                    
                    if !is_valid_shorthand {
                      self.report_error(Problem::InvalidShorthandVariable, &accessor_span);
                    }

                    continue
                  },
                  _ => {
                    self.report_error(Problem::InvalidShorthandVariable, &accessor_span);
                    None
                  }
                }
              },
              // Allow fragments as keys if they are valid identifiers
              Some((Fragment, span)) => {
                let key = self.reader.last_token_string();
                if !is_valid_ident(key.as_str()) {
                  self.report_error(Problem::InvalidIdentifier(key.to_string()), &span);
                }
                Some(MapKeyExpr::Static(key))
              },
              // Allow string literals as static keys
              Some((StringLiteral(s), _)) => {
                Some(MapKeyExpr::Static(s))
              },
              // End of map
              Some((RightParen, _)) => break,
              // Soft error on anything weird
              Some(_) => {
                self.report_unexpected_last_token_error();
                None
              },
              // Hard error on EOF
              None => {
                self.report_error(Problem::UnclosedMap, &super_range(start_span, &self.reader.last_token_span()));
                return Err(())
              }
            };
            
            self.reader.skip_ws();
            if !self.reader.eat_where(|tok| matches!(tok, Some((Equals, ..)))) {
              self.report_error(Problem::ExpectedToken("=".to_owned()), &self.reader.last_token_span());
              return Err(())
            }
            self.reader.skip_ws();
            let ParsedSequence { 
              sequence: value_expr, 
              end_type: value_expr_end, 
              .. 
            } = self.parse_sequence(SequenceParseMode::CollectionInit)?;
            
            if let Some(key_expr) = key_expr {
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
          }
          
          Ok(ParsedGroupOrCollection {
            node: Expression::MapInit(Rc::new(pairs)),
            is_collection: true,
            is_auto_hinted: false,
          })
        },
        _ => unreachable!()
      }
    } else {
      // If we end up here, it's either a tuple or expression group
      let mut sequences = vec![];

      loop {
        self.reader.skip_ws();
        
        let ParsedSequence { sequence, end_type: seq_end, is_auto_hinted, .. } = self.parse_sequence(SequenceParseMode::CollectionInit)?;
        
        match seq_end {
          SequenceEndType::CollectionInitDelim => {
            sequences.push(Rc::new(sequence));
          },
          SequenceEndType::CollectionInitEnd => {
            // If this was the only element, it's either an empty tuple or a group
            if sequences.is_empty() {
              // If the sequence we just read is empty, exit loop and generate an (empty) tuple
              if sequence.is_empty() {
                break
              }
              return Ok(ParsedGroupOrCollection {
                node: Expression::Sequence(Rc::new(sequence)),
                is_collection: false,
                is_auto_hinted,
              })
            }
            sequences.push(Rc::new(sequence));
            break
          },
          SequenceEndType::ProgramEnd => {
            // If this was the first element, we won't know whether it's a tuple yet, so give a more general error
            if sequences.is_empty() {
              self.report_error(Problem::UnclosedParens, &super_range(start_span, &self.reader.last_token_span()));
            } else {
              self.report_error(Problem::UnclosedTuple, &super_range(start_span, &self.reader.last_token_span()));
            }
            return Err(())
          },
          _ => unreachable!()
        }
      }

      // To allow trailing semicolons, remove the last element if its sequence is empty
      if let Some(seq) = sequences.last() {
        if seq.is_empty() {
          sequences.pop();
        }
      }

      Ok(ParsedGroupOrCollection {
        node: Expression::TupleInit(Rc::new(sequences)),
        is_collection: true,
        is_auto_hinted: false,
      })
    }
  }
  
  fn parse_func_params(&mut self, start_span: &Range<usize>) -> ParseResult<Vec<ParsedParameter>> {
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
      Some((Colon, _)) => {
        // Read the params
        'read_params: loop {
          self.reader.skip_ws();
          let is_auto_hinted = self.reader.eat_kw(KW_TEXT);

          match self.reader.next_solid() {
            // Regular parameter
            Some((Fragment, span)) => {              
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
              let (varity, full_param_span) = 
              if let Some((varity_token, varity_span)) = self.reader.take_where(|t| matches!(t, Some((Question | Star | Plus, _)))) 
              {
                (match varity_token {
                  // Optional parameter
                  Question => Varity::Optional,
                  // Optional variadic parameter
                  Star => Varity::VariadicStar,
                  // Required variadic parameter
                  Plus => Varity::VariadicPlus,
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

              last_varity = varity;
              is_sig_variadic |= is_param_variadic;

              // Read default value expr on optional params
              if matches!(varity, Varity::Optional) {
                let ParsedSequence {
                  sequence: default_value_seq,
                  end_type: default_value_end_type,
                  ..
                } = self.parse_sequence(SequenceParseMode::ParamDefaultValue)?;

                let should_continue = match default_value_end_type {
                  SequenceEndType::ParamDefaultValueSeparator => true,
                  SequenceEndType::ParamDefaultValueSignatureEnd => false,
                  SequenceEndType::ProgramEnd => {
                    self.report_error(Problem::UnclosedFunctionSignature, start_span);
                    return Err(())
                  }
                  _ => unreachable!(),
                };

                let opt_param = Parameter {
                  name: param_name,
                  varity: Varity::Optional,
                  default_value_expr: (!default_value_seq.is_empty()).then(|| Rc::new(default_value_seq))
                };

                // Add parameter to list
                params.push(ParsedParameter {
                  param: opt_param,
                  span: full_param_span.end .. self.reader.last_token_span().start,
                  is_auto_hinted,
                });

                // Keep reading other params if needed
                if should_continue {
                  continue 'read_params
                } else {
                  break 'read_params
                }
              }

              // Handle other varities here...

              let param = Parameter {
                name: param_name,
                varity,
                default_value_expr: None,
              };
              
              // Add parameter to list
              params.push(ParsedParameter {
                param,
                span: full_param_span,
                is_auto_hinted,
              });
                
              // Check if there are more params or if the signature is done
              match self.reader.next_solid() {
                // ';' means there are more params
                Some((Semicolon, ..)) => {
                  continue 'read_params
                },
                // ']' means end of signature
                Some((RightBracket, ..)) => {
                  break 'read_params
                },
                // Emit a hard error on anything else
                Some((_, span)) => {
                  self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
                  return Err(())
                },
                None => {
                  self.report_error(Problem::UnclosedFunctionSignature, start_span);
                  return Err(())
                },
              }
            },
            // Error on early close
            Some((RightBracket, span)) => {
              self.report_error(Problem::MissingIdentifier, &span);
              break 'read_params
            },
            // Error on anything else
            Some((.., span)) => {
              self.report_error(Problem::InvalidIdentifier(self.reader.last_token_string().to_string()), &span)
            },
            None => {
              self.report_error(Problem::UnclosedFunctionSignature, start_span);
              return Err(())
            }
          }
        }
      },
      // ']' means there are no params-- fall through to the next step
      Some((RightBracket, _)) => {},
      // Something weird is here, emit a hard error
      Some((.., span)) => {
        self.report_error(Problem::UnexpectedToken(self.reader.last_token_string().to_string()), &span);
        return Err(())
      },
      // Nothing is here, emit a hard error
      None => {
        self.report_error(Problem::UnclosedFunctionSignature, start_span);
        return Err(())
      }
    }
      
    Ok(params)
  }
    
  /// Parses a function definition, anonymous function, or function call.
  fn parse_func_access(&mut self) -> ParseResult<ParsedFunctionAccessor> {
    let start_span = self.reader.last_token_span();
    let mut is_auto_hinted = false;
    self.reader.skip_ws();
    
    // Check if we're defining a function (with [$|% ...]) or creating a lambda (with [? ...])
    if let Some((func_access_type_token, func_access_type_span)) 
    = self.reader.take_where(|t| matches!(t, Some((Dollar | Percent | Question, ..)))) {
      match func_access_type_token {
        // Function definition
        tt @ Dollar | tt @ Percent => {
          let is_const = matches!(tt, Percent);
          // Name of variable function will be stored in
          let (func_path, _func_path_span) = self.parse_access_path(false)?;

          // Warn user if non-variable function definition is marked as a constant
          if is_const && !func_path.is_variable_target() {
            self.report_warning(Problem::NestedFunctionDefMarkedConstant, &func_access_type_span);
          }
          
          self.reader.skip_ws();

          let ((body, params, end_func_sig_span, is_auto_hinted_def), captures) = self.capture_pass(|self_| {
            // Function params
            let params = self_.parse_func_params(&start_span)?;
            let end_func_sig_span = self_.reader.last_token_span();

            self_.reader.skip_ws();
            let is_auto_hinted_def = self_.reader.eat_kw(KW_TEXT);
            self_.reader.skip_ws();
            
            // Read function body
            let body = self_.parse_func_body(&params)?;
            
            Ok((body, params, end_func_sig_span, is_auto_hinted_def))
          })?;

          // Track variable
          if func_path.is_variable_target() {
            if let Some(id) = &func_path.var_name() {
              let func_def_span = super_range(&start_span, &end_func_sig_span);
              self.track_variable(id, &func_path.mode(), is_const, is_auto_hinted_def, VarRole::Function, &func_def_span);
            }
          }
          
          Ok(ParsedFunctionAccessor {
            is_auto_hinted: false,
            node: Expression::FuncDef(FunctionDef {
              body: Rc::new(body.with_name_str(format!("[{}]", func_path).as_str())),
              path: Rc::new(func_path),
              params: Rc::new(params.into_iter().map(|pp| pp.param).collect()),
              capture_vars: Rc::new(captures),
              is_const,
            })
          })
        },
        // Lambda
        Question => {
          // Lambda params
          let params = self.parse_func_params(&start_span)?;
          self.reader.skip_ws();
          // Read function body
          let (body, captures) = self.capture_pass(|self_| self_.parse_func_body(&params))?;
          
          Ok(ParsedFunctionAccessor {
            is_auto_hinted,
            node: Expression::Lambda(LambdaExpr {
              capture_vars: Rc::new(captures),
              body: Rc::new(body.with_name_str("lambda")),
              params: Rc::new(params.into_iter().map(|pp| pp.param).collect()),
            })
          })
        },
        _ => unreachable!()
      }
    } else {
      // Function calls, both piped and otherwise
      
      // List of calls in chain. This will only contain one call if it's non-piped (chain of one).
      let mut calls: Vec<FunctionCall> = vec![];
      // Target of assignment pipe
      let mut assignment_pipe = None;
      // Flag indicating whether call is piped (has multiple chained function calls)
      let mut is_piped = false;
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
        // Temporal call flag
        let mut is_temporal = false;
        // Do the user-supplied args use the pipe value?
        let mut is_pipeval_used = false;
        
        /// Reads arguments until a terminating / delimiting token is reached.
        macro_rules! parse_args {
          () => {{
            #[allow(unused_assignments)] // added because rustc whines about `spread_mode` being unused; that is a LIE
            loop {
              self.reader.skip_ws();
              let mut spread_mode = ArgumentSpreadMode::NoSpread;

              // Check for spread operators
              match self.reader.take_where(|t| matches!(t, Some((Star | DoubleStar | TemporalLabeled(_), ..)))) {
                // Parametric spread
                Some((Star, ..)) => {
                  self.reader.skip_ws();
                  spread_mode = ArgumentSpreadMode::Parametric;
                },
                // Unlabeled temporal spread
                Some((DoubleStar, ..)) => {
                  is_temporal = true;
                  self.reader.skip_ws();
                  let is_complex = self.reader.eat(RantToken::Star);
                  self.reader.skip_ws();
                  spread_mode = ArgumentSpreadMode::Temporal { label: cur_temporal_index, is_complex };
                  cur_temporal_index += 1;
                },
                // Labeled temporal spread
                Some((TemporalLabeled(label_str), ..)) => {
                  is_temporal = true;
                  self.reader.skip_ws();
                  let is_complex = self.reader.eat(RantToken::Star);
                  self.reader.skip_ws();
                  let label_index = if let Some(label_index) = temporal_index_labels.get(&label_str) {
                    *label_index
                  } else {
                    let label_index = cur_temporal_index;
                    temporal_index_labels.insert(label_str.clone(), label_index);
                    cur_temporal_index += 1;
                    label_index
                  };
                  spread_mode = ArgumentSpreadMode::Temporal { label: label_index, is_complex };
                },
                Some(_) => unreachable!(),
                None => {},
              }

              // Parse argument
              let ParsedSequence {
                sequence: arg_seq,
                end_type: arg_end,
                ..
              } = if is_piped {
                self.var_stack.push_layer();
                // Track pipe value inside arguement scope
                let pipeval_stats = VarStats::new(VarRole::PipeValue, Default::default(), true);
                self.var_stack.define(Identifier::from(PIPE_VALUE_NAME), pipeval_stats);
                let parsed_arg_expr = self.parse_sequence_inner(SequenceParseMode::FunctionArg, PREC_SEQUENCE)?;
                is_pipeval_used |= self.var_stack.get(PIPE_VALUE_NAME).unwrap().reads > 0;
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
                SequenceEndType::FunctionArgEndToPipe => {
                  is_piped = true;
                  break
                },
                SequenceEndType::FunctionArgEndToAssignPipe => {
                  let assign_pipe_start_span = self.reader.last_token_span();
                  self.reader.skip_ws();
                  let is_auto_hinted_def = self.reader.eat_kw(KW_TEXT);
                  let auto_hint_span = self.reader.last_token_span();
                  self.reader.skip_ws();

                  let (is_def, is_const_def) = if let Some((def_token, ..)) 
                  = self.reader.take_where(|t| matches!(t, Some((Dollar | Percent, ..)))) {
                    match def_token {
                      // Variable declaration
                      Dollar => (true, false),
                      // Constant declaration
                      Percent => (true, true),
                      _ => unreachable!()
                    }
                  } else {
                    (false, false)
                  };
                  self.reader.skip_ws();

                  // Error on @text when accessor isn't a definition
                  if !is_def && is_auto_hinted_def {
                    self.report_unexpected_token_error(&auto_hint_span);
                  }
                  
                  assignment_pipe = Some(Rc::new(if is_def {
                    let access_mode = self.parse_access_mode();
                    self.reader.skip_ws();
                    let ident = self.parse_ident()?;
                    let ident_span = self.reader.last_token_span();

                    if is_temporal {
                      // Don't allow constant definitions in temporal context
                      if is_const_def {
                        self.report_error(Problem::TemporalAssignPipeRedefinesConstant(ident.to_string()), &super_range(&assign_pipe_start_span, &ident_span));
                      } else {
                        self.report_warning(Problem::TemporalAssignPipeRedefinesVariable(ident.to_string()), &super_range(&assign_pipe_start_span, &ident_span));
                      }
                    }
                    
                    AssignmentPipeTarget::Def { ident, is_const: is_const_def, access_mode }
                  } else {
                    let (path, path_span) = self.parse_access_path(true)?;
                    
                    // Don't allow assignment to known anonymous values
                    if path.is_anonymous_target() {
                      self.report_error(Problem::AnonValueAssignment, &path_span);
                    }

                    self.track_variable_access(&path, true, false, &path_span, None);

                    AssignmentPipeTarget::Set(Rc::new(path))
                  }));

                  self.reader.skip_ws();

                  // We expect the function accessor to end here
                  if !self.reader.eat(RantToken::RightBracket) {
                    self.report_expected_token_error("]", &self.reader.last_token_span());
                  }

                  is_piped = true;
                  is_finished = true;
                  is_auto_hinted = false;
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
        
        /// If the pipe value wasn't used, inserts it as the first argument.
        macro_rules! fallback_pipe {
          () => {
            if calls.len() > 0 && !is_pipeval_used {
              let arg = ArgumentExpr {
                expr: Rc::new(Sequence::one(Expression::PipeValue, &self.info)),
                spread_mode: ArgumentSpreadMode::NoSpread,
              };
              func_args.insert(0, arg);
            }
          }
        }
        
        self.reader.skip_ws();

        let old_pipeval = if is_piped {
          let old = self.var_stack.remove(PIPE_VALUE_NAME);
          self.var_stack.define(PIPE_VALUE_NAME.into(), VarStats::new(VarRole::PipeValue, Default::default(), true));
          old
        } else {
          None
        };

        // Read function access path
        let (func_path, func_path_span) = self.parse_access_path(true)?;
        
        if let Some(pipeval_stats) = self.var_stack.get(PIPE_VALUE_NAME) {
          is_pipeval_used |= pipeval_stats.reads > 0;
        }
        
        if let Some((token, _)) = self.reader.next_solid() {
          match token {
            // No args, fall through
            RightBracket => {
              is_finished = true;
            },
            // Parse arguments
            Colon => parse_args!(),
            // Pipe without args
            PipeOp => {
              is_piped = true;
            }
            _ => {
              self.report_unexpected_last_token_error();
              return Err(())
            }
          }

          fallback_pipe!();
          
          // Record access to function
          self.track_variable_access(&func_path, false, false, &func_path_span, Some(&mut is_auto_hinted));
          
          // Create final node for function call
          let fcall = FunctionCall {
            target: FunctionCallTarget::Path(Rc::new(func_path)),
            arguments: Rc::new(func_args),
            is_temporal,
          };

          calls.push(fcall);
        } else {
          // Found EOF instead of end of function call, emit hard error
          self.report_error(Problem::UnclosedFunctionCall, &self.reader.last_token_span());
          return Err(())
        }

        if is_piped {
          self.var_stack.remove(PIPE_VALUE_NAME);
          if let Some(old_pipeval) = old_pipeval {
            self.var_stack.define(PIPE_VALUE_NAME.into(), old_pipeval);
          }
        }

        is_chain_temporal |= is_temporal;
      }

      // Return the finished node
      Ok(ParsedFunctionAccessor {
        is_auto_hinted,
        node: if is_piped {
          Expression::PipedCall(PipedCall {
            is_temporal: is_chain_temporal,
            steps: Rc::new(calls),
            assignment_pipe,
          })
        } else {
          Expression::FuncCall(calls.drain(..).next().unwrap())
        },
      })
    }
  }
    
  #[inline]
  fn parse_access_mode(&mut self) -> VarAccessMode {    
    if let Some((token, _span)) = self.reader.take_where(
      |t| matches!(t, Some((Slash | Caret, _)))
    ) {
      match token {
        // Accessor is explicit global
        Slash => {
          VarAccessMode::ExplicitGlobal
        },
        // Accessor is for parent scope (descope operator)
        Caret => {
          let mut descope_count = 1;
          loop {
            if !self.reader.eat_where(|t| matches!(t, Some((Caret, _)))) {
              break VarAccessMode::Descope(descope_count)
            }
            descope_count += 1;
          }
        },
        _ => unreachable!()
      }
    } else {
      VarAccessMode::Local
    }
  }
  
  /// Parses an access path.
  #[inline]
  fn parse_access_path(&mut self, allow_anonymous: bool) -> ParseResult<(AccessPath, Range<usize>)> {
    self.reader.skip_ws();
    let mut idparts = vec![];
    let start_span = self.reader.last_token_span();

    // Check for global/descope specifiers
    let access_mode = self.parse_access_mode();

    self.reader.skip_ws();

    // Parse the first part of the path
    // Check for certain index/slice forms and disallow them
    if let Ok(Some(_)) = self.try_read_signed_int() {
      let span = self.reader.last_token_span();
      self.reader.skip_ws();
      if self.reader.eat_where(|t| matches!(t, Some((DoubleDot, ..)))) {
        self.report_error(Problem::AccessPathStartsWithSlice, &super_range(&span, &self.reader.last_token_span()));
      } else {
        self.report_error(Problem::AccessPathStartsWithIndex, &span);
      }
    } else {        
      match self.reader.next() {
        // Variable name
        Some((Fragment, span)) => {
          let varname = Identifier::new(self.reader.last_token_string());
          if is_valid_ident(varname.as_str()) {
            idparts.push(AccessPathComponent::Name(varname));
          } else {
            self.report_error(Problem::InvalidIdentifier(varname.to_string()), &span);
          }
        },
        // Pipeval can be directly accessed here too
        Some((PipeValue, span)) => {
          idparts.push(AccessPathComponent::PipeValue);
          self.track_pipeval_read(&span);
        }
        // An expression can also be used to provide the variable
        Some((LeftParen, _)) if allow_anonymous => {
          let anon_value_expr = self.parse_dynamic_key(false)?;
          idparts.push(AccessPathComponent::Expression(Rc::new(anon_value_expr)));
        },
        // First path part can't be a slice
        Some((DoubleDot, span)) => {
          let _ = self.try_read_signed_int();
          self.report_error(Problem::AccessPathStartsWithSlice, &super_range(&span, &self.reader.last_token_span()));
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
      if self.reader.eat_where(|t| matches!(t, Some((Slash, ..)))) {
        // From here we expect to see either another key (fragment) or index (integer).
        // If it's anything else, return a syntax error.
        self.reader.skip_ws();
        match self.try_read_signed_int() {
          // Index or slice with static from-bound
          Ok(Some(i)) => {
            self.reader.skip_ws();
            // Look for a slice separator to see if it's a slice
            if self.reader.eat_where(|t| matches!(t, Some((DoubleDot, ..)))) {
              self.reader.skip_ws();
              match self.try_read_signed_int() {
                // Between-slice with static from- and to-bounds
                Ok(Some(j)) => {
                  idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Static(i), SliceIndex::Static(j))));
                },
                Ok(None) => {
                  match self.reader.peek() {
                    // Between-slice with static from-bound + dynamic to-bound
                    Some((LeftParen, ..)) => {
                      let to_expr = Rc::new(self.parse_dynamic_key(true)?);
                      idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Static(i), SliceIndex::Dynamic(to_expr))));
                    },
                    // From-slice with static from-bound
                    Some((Slash | RightAngle | Equals | Question | Semicolon, ..)) => {
                      idparts.push(AccessPathComponent::Slice(SliceExpr::From(SliceIndex::Static(i))));
                    },
                    // Found something weird as the to-bound, emit an error
                    Some(_) => {
                      self.reader.next();
                      let token = self.reader.last_token_string().to_string();
                      self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
                    },
                    None => {
                      self.report_error(Problem::UnclosedAccessor, &super_range(&start_span, &self.reader.last_token_span()));
                      return Err(())
                    }
                  }
                },
                Err(()) => {}
              }
            } else {
              // No slice separator, so it's an index
              idparts.push(AccessPathComponent::Index(i));
            }
          },
          Ok(None) => {
            match self.reader.next() {
              // Key
              Some((Fragment, span)) => {
                let varname = Identifier::new(self.reader.last_token_string());
                if is_valid_ident(varname.as_str()) {
                  idparts.push(AccessPathComponent::Name(varname));
                } else {
                  self.report_error(Problem::InvalidIdentifier(varname.to_string()), &span);
                }
              },
              // Pipeval
              Some((PipeValue, _span)) => {
                idparts.push(AccessPathComponent::PipeValue);
                let pipeval_span = &self.reader.last_token_span();
                self.track_pipeval_read(pipeval_span);
              }
              // Full- or to-slice
              Some((DoubleDot, _)) => {
                self.reader.skip_ws();
                match self.try_read_signed_int() {
                  // To-slice with static bound
                  Ok(Some(to)) => {
                    idparts.push(AccessPathComponent::Slice(SliceExpr::To(SliceIndex::Static(to))));
                  },
                  Ok(None) => {
                    match self.reader.peek() {
                      // To-slice with dynamic bound
                      Some((LeftParen, ..)) => {
                        let to_expr = Rc::new(self.parse_dynamic_key(true)?);
                        idparts.push(AccessPathComponent::Slice(SliceExpr::To(SliceIndex::Dynamic(to_expr))));
                      },
                      // Full-slice
                      Some((Slash | RightAngle | Equals | Question | Semicolon, ..)) => {
                        idparts.push(AccessPathComponent::Slice(SliceExpr::Full));
                      },
                      // Found something weird as the to-bound, emit an error
                      Some(_) => {
                        self.reader.next();
                        let token = self.reader.last_token_string().to_string();
                        self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
                      },
                      None => {
                        self.report_error(Problem::UnclosedAccessor, &super_range(&start_span, &self.reader.last_token_span()));
                        return Err(())
                      }
                    }
                  },
                  Err(()) => {}
                }
              },
              // Dynamic key or slice with dynamic from-bound
              Some((LeftParen, _)) => {
                let expr = Rc::new(self.parse_dynamic_key(false)?);
                self.reader.skip_ws();
                // Look for a slice separator to see if it's a slice
                if self.reader.eat_where(|t| matches!(t, Some((DoubleDot, ..)))) {
                  self.reader.skip_ws();
                  match self.try_read_signed_int() {
                    // Between-slice with a dynamic from-bound + static to-bound
                    Ok(Some(to)) => {
                      idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Dynamic(expr), SliceIndex::Static(to))));
                    },
                    Ok(None) => {
                      match self.reader.peek() {
                        // Between-slice with dynamic from- + to-bounds
                        Some((LeftParen, ..)) => {
                          let to_expr = Rc::new(self.parse_dynamic_key(true)?);
                          idparts.push(AccessPathComponent::Slice(SliceExpr::Between(SliceIndex::Dynamic(expr), SliceIndex::Dynamic(to_expr))));
                        },
                        // From-slice with dynamic bound
                        Some((Slash | RightAngle | Equals | Question | Semicolon, ..)) => {
                          idparts.push(AccessPathComponent::Slice(SliceExpr::From(SliceIndex::Dynamic(expr))));
                        },
                        // Found something weird as the to-bound, emit an error
                        Some(_) => {
                          self.reader.next();
                          let token = self.reader.last_token_string().to_string();
                          self.report_error(Problem::InvalidSliceBound(token), &self.reader.last_token_span());
                        },
                        None => {
                          self.report_error(Problem::UnclosedAccessor, &super_range(&start_span, &self.reader.last_token_span()));
                          return Err(())
                        }
                      }
                    },
                    Err(()) => {}
                  }
                } else {
                  // No slice separator, so it's an dynamic key
                  idparts.push(AccessPathComponent::Expression(expr));
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
          },
          Err(()) => {}
        }
        
      } else {
        return Ok((AccessPath::new(idparts, access_mode), start_span.start .. self.reader.last_token_span().start))
      }
    }
  }
    
  /// Parses a dynamic key.
  fn parse_dynamic_key(&mut self, expect_opening_paren: bool) -> ParseResult<Sequence> {
    if expect_opening_paren && !self.reader.eat_where(|t| matches!(t, Some((LeftParen, _)))) {
      self.report_expected_token_error("(", &self.reader.last_token_span());
      return Err(())
    }
    
    let start_span = self.reader.last_token_span();
    let ParsedSequence { sequence, end_type, .. } = self.parse_sequence(SequenceParseMode::DynamicKey)?;
    
    match end_type {
      SequenceEndType::DynamicKeyEnd => {},
      SequenceEndType::ProgramEnd => {
        // Hard error if block isn't closed
        let err_span = start_span.start .. self.source.len();
        self.report_error(Problem::UnclosedParens, &err_span);
        return Err(())
      },
      _ => unreachable!()
    }
    
    Ok(sequence)
  }

  /// Parses a function body and DOES NOT capture variables.
  fn parse_func_body(&mut self, params: &Vec<ParsedParameter>) -> ParseResult<Sequence> {
    self.reader.skip_ws();

    if !self.reader.eat_where(|t| matches!(t, Some((LeftBrace, _)))) {
      self.report_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
      return Err(())
    }

    let start_span = self.reader.last_token_span();

    // Define each parameter as a variable in the current var_stack frame so they are not accidentally captured
    for ParsedParameter {
      param,
      span,
      is_auto_hinted
    } in params {
      self.var_stack.define(param.name.clone(), VarStats {
        reads: 0,
        writes: 1,
        def_span: span.clone(),
        is_const: true,
        is_auto_hinted: *is_auto_hinted,
        has_fallible_read: false,
        role: if param.is_optional() && param.default_value_expr.is_none() {
          VarRole::FallibleOptionalArgument
        } else { 
          VarRole::Argument 
        }
      });
    }

    // parse_sequence_inner() is used here so that the new stack frame can be customized before use
    let ParsedSequence { 
      sequence, 
      end_type, 
      .. 
    } = self.parse_sequence_inner(SequenceParseMode::FunctionBodyBlock, PREC_SEQUENCE)?;

    match end_type {
      SequenceEndType::FunctionBodyEnd => {},
      SequenceEndType::ProgramEnd => {
        let err_span = start_span.start .. self.source.len();
        self.report_error(Problem::UnclosedFunctionBody, &err_span);
        return Err(())
      },
      _ => unreachable!()
    }

    Ok(sequence)
  }
  
  fn capture_pass<T>(&mut self, parse_func: impl FnOnce(&mut Self) -> ParseResult<T>) -> ParseResult<(T, Vec<Identifier>)> {
    // Since we're about to push another var_stack frame, we can use the current depth of var_stack as the index
    let capture_height = self.var_stack.depth();

    // Push a new capture frame
    self.capture_stack.push((capture_height, Default::default()));

    // Push a new variable frame
    self.var_stack.push_layer();

    // Call parse_func
    let parse_out = parse_func(self)?;

    // Run static analysis on variable/param usage
    self.analyze_top_vars();

    self.var_stack.pop_layer();

    // Pop the topmost capture frame and grab the set of captures
    let (_, mut capture_set) = self.capture_stack.pop().unwrap();

    Ok((parse_out, capture_set.drain().collect()))
  }
    
  /// Parses a standard (attribute-consuming) block.
  fn parse_block(&mut self, parse_mode: BlockParseMode) -> ParseResult<ParsedBlock> {
    let protection = match parse_mode {
      BlockParseMode::NeedsStart => {
        self.reader.skip_ws();
        let protection = if self.reader.eat(RantToken::At) {
          Some(BlockProtection::Outer)
        } else {
          None
        };

        if !self.reader.eat_where(|t| matches!(t, Some((LeftBrace, _)))) {
          self.report_error(Problem::ExpectedToken("{".to_owned()), &self.reader.last_token_span());
          return Err(())
        }

        protection
      },
      BlockParseMode::StartParsed(protection) => protection,
    };
    
    // Get position of starting brace for error reporting
    let start_pos = self.reader.last_token_pos();
    // Keeps track of inherited hinting
    let mut auto_hint = false;
    // Is the block weighted?
    let mut is_weighted = false;
    // Block content
    let mut elements = vec![];
    
    loop {
      self.reader.skip_ws();

      self.var_stack.push_layer();

      // Check for @edit
      let modifier = if self.reader.eat_kw(KW_EDIT) {
        self.reader.skip_ws();
        let input_id = if self.reader.eat(RantToken::Colon) {
          None // No input variable
        } else {
          // Get the name of the input variable
          let input_id = self.parse_ident()?;
          let input_id_span = self.reader.last_token_span();
          self.reader.skip_ws();
          if !self.reader.eat(RantToken::Colon) {
            self.report_expected_token_error(":", &self.reader.last_token_span());
          }
          self.var_stack.define(input_id.clone(), VarStats::new(VarRole::Normal, input_id_span, true));
          Some(input_id)
        };        
        self.reader.skip_ws();

        Some(OutputModifierSig {
          input_var: input_id
        })
      } else {
        None
      };

      let ParsedSequence { 
        sequence, 
        end_type, 
        is_auto_hinted, 
        extras ,
        ..
      } = self.parse_sequence_inner(SequenceParseMode::BlockElement, PREC_SEQUENCE)?;

      self.analyze_top_vars();
      self.var_stack.pop_layer();
      
      auto_hint |= is_auto_hinted;

      let element = Rc::new(BlockElement {
        main: Rc::new(sequence),
        weight: if let Some(ParsedSequenceExtras::WeightedBlockElement { weight_expr }) = extras {
          is_weighted = true;
          // Optimize constant weights
          Some(match (weight_expr.len(), weight_expr.first().map(Rc::as_ref)) {
            (1, Some(Expression::Integer(n))) => BlockWeight::Constant(*n as f64),
            (1, Some(Expression::Float(n))) => BlockWeight::Constant(*n),
            _ => BlockWeight::Dynamic(weight_expr)
          })
        } else {
          None
        },
        output_modifier: modifier,
      });
      
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
    
    Ok(ParsedBlock {
      block: Block::new(is_weighted, protection, elements),
      is_auto_hinted: auto_hint
    })
  }
  
  /// Parses an identifier.
  fn parse_ident(&mut self) -> ParseResult<Identifier> {
    if let Some((token, span)) = self.reader.next_solid() {
      match token {
        Fragment => {
          let idstr = self.reader.last_token_string();
          if !is_valid_ident(idstr.as_str()) {
            self.report_error(Problem::InvalidIdentifier(idstr.to_string()), &span);
          }
          Ok(Identifier::new(idstr))
        },
        _ => {
          self.report_unexpected_last_token_error();
          Err(())
        }
      }
    } else {
      self.report_error(Problem::MissingIdentifier, &self.reader.last_token_span());
      Err(())
    }
  }

  #[inline]
  fn track_variable(&mut self, id: &Identifier, access_kind: &VarAccessMode, is_const: bool, is_auto_hinted: bool, role: VarRole, def_span: &Range<usize>) {
    // Check if there's already a variable with this name
    let (prev_tracker, requested_depth, found_depth) = match access_kind {        
      VarAccessMode::Local => {
        (self.var_stack.get(id), 0, self.var_stack.depth_of(id))
      },
      VarAccessMode::Descope(n) => {
        let (v, d) = self.var_stack
          .get_parent_depth(id, *n)
          .map(|(v, d)| (Some(v), Some(d)))
          .unwrap_or_default();
        (v, *n, d)
      },
      VarAccessMode::ExplicitGlobal => {
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
      has_fallible_read: false,
      is_const,
      is_auto_hinted,
      role,
    };

    // Add to stack
    match access_kind {
      VarAccessMode::Local => {
        self.var_stack.define(id.clone(), v);
      },
      VarAccessMode::Descope(n) => {
        self.var_stack.define_parent(id.clone(), v, *n);
      },
      VarAccessMode::ExplicitGlobal => {
        self.var_stack.define_parent(id.clone(), v, self.var_stack.depth());
      },
    }
  }

  #[inline]
  fn track_variable_access(&mut self, path: &AccessPath, is_write: bool, has_fallback: bool, span: &Range<usize>, out_auto_hinted_read: Option<&mut bool>) {
    let mut out_auto_hinted_read_value = false;

    // Handle access stats
    if let Some(id) = &path.var_name() {
      let tracker = match path.mode() {
        VarAccessMode::Local => {
          self.var_stack.get_mut(id)
        },
        VarAccessMode::Descope(n) => {
          self.var_stack.get_parent_mut(id, n)
        },
        VarAccessMode::ExplicitGlobal => {
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
          tracker.add_read(!has_fallback);
          out_auto_hinted_read_value = tracker.is_auto_hinted;

          // Error if user is accessing a fallible optional argument without a fallback
          if tracker.has_fallible_read && tracker.role == VarRole::FallibleOptionalArgument {
            self.report_error(Problem::FallibleOptionalArgAccess(id.to_string()), span);
          }
        }
      }
    }

    if let Some(auto_hinted_read) = out_auto_hinted_read {
      *auto_hinted_read = out_auto_hinted_read_value;
    }
    
    // Handle captures
    if path.mode().is_local() {
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
        VarRole::Argument | VarRole::FallibleOptionalArgument => self.report_warning(Problem::UnusedParameter(name), &span),
        VarRole::Function => self.report_warning(Problem::UnusedFunction(name), &span),
        // Ignore any other roles
        _ => {},
      }
    }
  }
    
  /// Parses one or more accessors (getter/setter/definition).
  #[inline(always)]
  fn parse_accessor(&mut self) -> ParseResult<ParsedAccessor> {
    let mut accessors = vec![];
    let mut is_auto_hinted_accessor = false;

    macro_rules! add_accessor {
      ($rst:expr) => {{
        let rst = $rst;
        accessors.push(rst);
      }}
    }
    
    'read: loop {      
      self.reader.skip_ws();
      let is_auto_hinted_def = self.reader.eat_kw(KW_TEXT);
      let auto_hint_span = self.reader.last_token_span();
      self.reader.skip_ws();

      // Check if the accessor ends here as long as there's at least one component
      if !accessors.is_empty() && self.reader.eat_where(|t| matches!(t, Some((RightAngle, ..)))) {
        break
      }
      
      let (is_def, is_const_def) = if let Some((def_token, ..)) 
      = self.reader.take_where(|t| matches!(t, Some((Dollar | Percent, ..)))) {
        match def_token {
          // Variable declaration
          Dollar => (true, false),
          // Constant declaration
          Percent => (true, true),
          _ => unreachable!()
        }
      } else {
        (false, false)
      };

      // Error on @text when accessor isn't a definition
      if !is_def && is_auto_hinted_def {
        self.report_unexpected_token_error(&auto_hint_span);
      }

      let access_start_span = self.reader.last_token_span();

      self.reader.skip_ws();
      
      // Check if it's a definition. If not, it's a getter or setter
      if is_def {
        // Check for accessor modifiers
        let access_mode = self.parse_access_mode();
        self.reader.skip_ws();
        // Read name of variable we're defining
        let var_name = self.parse_ident()?;
        let def_span = access_start_span.start .. self.reader.last_token_span().end;
        
        if let Some((token, _token_span)) = self.reader.next_solid() {
          match token {
            // End of accessor
            RightAngle => {              
              self.track_variable(&var_name, &access_mode, is_const_def, is_auto_hinted_def, VarRole::Normal, &def_span);
              add_accessor!(Expression::Define(Definition {
                is_const: is_const_def,
                access_mode,
                name: var_name,
                value: None
              }));
              break 'read
            },
            // Accessor delimiter
            Semicolon => {
              self.track_variable(&var_name, &access_mode, is_const_def, is_auto_hinted_def, VarRole::Normal, &def_span);
              add_accessor!(Expression::Define(Definition {
                is_const: is_const_def,
                access_mode,
                name: var_name,
                value: None
              }));
              continue 'read;
            },
            // Definition and assignment
            Equals => {
              self.reader.skip_ws();
              let ParsedSequence { 
                sequence: setter_expr, 
                end_type: setter_end_type, 
                .. 
              } = self.parse_sequence(SequenceParseMode::VariableAssignment)?;

              let def_span = access_start_span.start .. self.reader.last_token_span().start;
              self.track_variable(&var_name, &access_mode, is_const_def, is_auto_hinted_def, VarRole::Normal, &def_span);
              add_accessor!(Expression::Define(Definition {
                name: var_name,
                access_mode,
                is_const: is_const_def,
                value: Some(Rc::new(setter_expr)),
              }));
              
              match setter_end_type {
                SequenceEndType::VariableAssignDelim => {
                  continue 'read
                },
                SequenceEndType::VariableAccessEnd => {
                  break 'read
                },
                SequenceEndType::ProgramEnd => {
                  self.report_error(Problem::UnclosedAccessor, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // Ran into something we don't support
            _ => {
              self.report_unexpected_last_token_error();
              return Err(())
            }
          }
        } else {
          self.report_error(Problem::UnclosedAccessor, &self.reader.last_token_span());
          return Err(())
        }
      } else {
        // Read the path to what we're accessing
        let mut is_auto_hinted_get = false;
        let (var_path, var_path_span) = self.parse_access_path(true)?;
        
        self.reader.skip_ws();
        
        if let Some((token, cur_token_span)) = self.reader.next_solid() {
          match token {
            // If we hit a '>', it's a getter
            RightAngle => {
              self.track_variable_access(&var_path, false, false, &var_path_span, Some(&mut is_auto_hinted_get));
              is_auto_hinted_accessor |= is_auto_hinted_get;
              add_accessor!(Expression::Get(Getter { path: Rc::new(var_path), fallback: None }));
              break 'read;
            },
            // If we hit a ';', it's a getter with another accessor chained after it
            Semicolon => {
              self.track_variable_access(&var_path, false, false, &var_path_span, Some(&mut is_auto_hinted_get));
              is_auto_hinted_accessor |= is_auto_hinted_get;
              add_accessor!(Expression::Get(Getter { path: Rc::new(var_path), fallback: None }));
              continue 'read;
            },
            // If we hit a `?`, it's a getter with a fallback
            Question => {
              self.reader.skip_ws();
              let ParsedSequence {
                sequence: fallback_expr,
                end_type: fallback_end_type,
                ..
              } = self.parse_sequence(SequenceParseMode::AccessorFallbackValue)?;

              self.track_variable_access(&var_path, false, true, &var_path_span, Some(&mut is_auto_hinted_get));
              is_auto_hinted_accessor |= is_auto_hinted_get;
              add_accessor!(Expression::Get(Getter { path: Rc::new(var_path), fallback: Some(Rc::new(fallback_expr)) }));

              match fallback_end_type {
                SequenceEndType::AccessorFallbackValueToDelim => continue 'read,
                SequenceEndType::AccessorFallbackValueToEnd => break 'read,
                // Error
                SequenceEndType::ProgramEnd => {
                  self.report_error(Problem::UnclosedAccessor, &cur_token_span);
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // If we hit a `=`, it's a DIRECT ASSIGNMENT setter
            Equals => {
              self.reader.skip_ws();
              let ParsedSequence {
                sequence: setter_rhs_expr,
                end_type: setter_rhs_end,
                ..
              } = self.parse_sequence(SequenceParseMode::VariableAssignment)?;
              
              let rhs_end_span = self.reader.last_token_span();
              let setter_span = super_range(&access_start_span, &rhs_end_span);

              // Don't allow setters directly on anonymous values
              if var_path.is_anonymous_target() {
                self.report_error(Problem::AnonValueAssignment, &setter_span);
              }

              self.track_variable_access(&var_path, var_path.is_variable_target(), false, &setter_span, None);
              add_accessor!(Expression::Set(Setter { path: Rc::new(var_path), value: Rc::new(setter_rhs_expr) }));

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
                  self.report_error(Problem::UnclosedAccessor, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            // If we hit a compound assignment operator... well, it's a COMPOUND ASSIGNMENT setter!
            comp_op @ (PlusEquals | MinusEquals | StarEquals | SlashEquals | DoubleStarEquals | PercentEquals | AndEquals | VertBarEquals)  => {
              self.reader.skip_ws();
              
              let ParsedSequence {
                sequence: setter_rhs_expr,
                end_type: setter_rhs_end,
                ..
              } = self.parse_sequence(SequenceParseMode::VariableAssignment)?;
              
              let rhs_end_span = self.reader.last_token_span();
              let setter_span = super_range(&access_start_span, &rhs_end_span);

              // Don't allow setters directly on anonymous values
              if var_path.is_anonymous_target() {
                self.report_error(Problem::AnonValueAssignment, &setter_span);
              }

              self.track_variable_access(&var_path, var_path.is_variable_target(), false, &setter_span, None);

              let op_lhs = Rc::new(Sequence::one(Expression::Get(Getter { path: Rc::new(var_path.clone().add_descope(1)), fallback: None }), &self.info));
              let op_rhs = Rc::new(setter_rhs_expr);
              let setter_path = Rc::new(var_path);

              add_accessor!(Expression::Set(Setter {
                path: setter_path,
                value: Rc::new(Sequence::one(match comp_op {
                  PlusEquals => Expression::Add(op_lhs, op_rhs),
                  MinusEquals => Expression::Subtract(op_lhs, op_rhs),
                  StarEquals => Expression::Multiply(op_lhs, op_rhs),
                  SlashEquals => Expression::Divide(op_lhs, op_rhs),
                  PercentEquals => Expression::Modulo(op_lhs, op_rhs),
                  DoubleStarEquals => Expression::Power(op_lhs, op_rhs),
                  AndEquals => Expression::LogicAnd(op_lhs, op_rhs),
                  VertBarEquals => Expression::LogicOr(op_lhs, op_rhs),
                  CaretEquals => Expression::LogicXor(op_lhs, op_rhs),
                  _ => unreachable!(),
                }, &self.info))
              }));

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
                  self.report_error(Problem::UnclosedAccessor, &self.reader.last_token_span());
                  return Err(())
                },
                _ => unreachable!()
              }
            },
            _ => {
              // Anything else is an error
              self.report_unexpected_last_token_error();
              return Err(())
            }
          }
        } else {
          self.report_error(Problem::UnclosedAccessor, &self.reader.last_token_span());
          return Err(())
        }
      }
    }
    
    Ok(ParsedAccessor {
      nodes: accessors,
      is_auto_hinted: is_auto_hinted_accessor
    })
  }

  /// Attempt to read a signed integer.
  ///
  /// Will generate a compiler error if there's a dangling sign.
  fn try_read_signed_int(&mut self) -> ParseResult<Option<i64>> {
    if let Some((token, span)) = self.reader.take_where(|t| matches!(t, Some((RantToken::Minus | RantToken::IntegerPositive(_), _)))) {
      match token {
        RantToken::IntegerPositive(nt) => match nt {
          PositiveIntegerToken::Value(n) => match self.try_sign_unsigned_int(n, false, &span) {
            Ok(i) => return Ok(Some(i)),
            Err(()) => return Err(())
          },
          PositiveIntegerToken::OutOfRange => {
            self.report_error(Problem::IntegerLiteralOutOfRange, &span);
            return Err(())
          }
        },
        RantToken::Minus => {
          self.reader.skip_ws();
          if let Some((RantToken::IntegerPositive(nt), span)) = self.reader.take_where(|t| matches!(t, Some((RantToken::IntegerPositive(_), _)))) {
            return match nt {
              PositiveIntegerToken::Value(n) => match self.try_sign_unsigned_int(n, true, &span) {
                Ok(i) => Ok(Some(i)),
                Err(()) => Err(())
              },
              PositiveIntegerToken::OutOfRange => {
                self.report_error(Problem::IntegerLiteralOutOfRange, &span);
                Err(())
              }
            }
          } else {
            self.report_expected_token_error("<integer>", &self.reader.last_token_span());
            return Err(())
          }
        }
        _ => unreachable!()
      }
    }
    Ok(None)
  }

  /// Tries to convert a unsigned 64-bit integer to a signed 64-bit integer with the specified sign.
  /// 
  /// Accounts for the minimum 64-bit integer value, whose absolute value is out of range for `i64`.
  #[inline]
  fn try_sign_unsigned_int(&mut self, value_unsigned: u64, negate: bool, span: &Range<usize>) -> ParseResult<i64> {
    match cast::i64(if negate { -1 } else { 1 } * cast::i128(value_unsigned)) {
      Ok(value_signed) => Ok(value_signed),
      Err(_) => {
        self.report_error(Problem::IntegerLiteralOutOfRange, span);
        Err(())
      }
    }
  }
}