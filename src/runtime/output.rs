use crate::{InternalString, RantList, RantMap, RantValue, format::{NumberFormat, OutputFormat}};
use super::format::{WhitespaceNormalizationMode};
use std::{cell::RefCell, rc::Rc};

const INITIAL_CHAIN_CAPACITY: usize = 64;
const DEFAULT_SPACE: &str = " ";

/// Writes a stream of buffers that can be passed to a parent buffer or rendered to a string.
pub struct OutputWriter {
  buffers: Vec<OutputBuffer>,
  format: Rc<OutputFormat>,
  mode: OutputPrintMode,
}

impl OutputWriter {
  #[inline]
  pub fn new(prev_output: Option<&Self>) -> Self {
    Self {
      buffers: Vec::with_capacity(INITIAL_CHAIN_CAPACITY),
      format: prev_output.map(|o| Rc::clone(&o.format)).unwrap_or_default(),
      mode: OutputPrintMode::Single,
    }
  }

  /// Gets a reference to the current output format.
  #[inline]
  pub fn format(&self) -> &OutputFormat {
    &self.format
  }

  /// Gets a mutable reference to the current output format.
  #[inline]
  pub fn format_mut(&mut self) -> &mut OutputFormat {
    Rc::make_mut(&mut self.format)
  }

  #[inline]
  fn last_buffer(&self) -> Option<&OutputBuffer> {
    self.buffers.last()
  }

  #[inline]
  fn last_buffer_mut(&mut self) -> Option<&mut OutputBuffer> {
    self.buffers.last_mut()
  }

  #[inline]
  pub fn update_number_format(&mut self) {
    let fmt = self.format.num_format.clone();
    if let Some(OutputBuffer::NumberFormatUpdate(upd)) = self.last_buffer_mut() {
      *upd = fmt;
    } else {
      self.write_buffer(OutputBuffer::NumberFormatUpdate(fmt));
    }
  }

  /// Writes a value to the output.
  #[inline]
  pub fn write_value(&mut self, value: RantValue) {
    if !matches!(value, RantValue::Empty) {
      self.write_buffer(OutputBuffer::Value(value));
    }
  }
  
  #[inline]
  fn write_buffer(&mut self, value: OutputBuffer) {
    // Set the correct mode for the output content
    match (self.buffers.len() + 1, self.mode) {
      // Decide mode for first buffer in chain
      (1, _) => {
        match &value {
          OutputBuffer::Fragment(_) => {
            self.mode = OutputPrintMode::Text;
          },
          OutputBuffer::Value(RantValue::List(_)) => {
            self.mode = OutputPrintMode::List;
          },
          OutputBuffer::Value(RantValue::Map(_)) => {
            self.mode = OutputPrintMode::Map;
          },
          _ => {},
        }
      },
      (_, OutputPrintMode::Single | OutputPrintMode::Concat) => {
        match &value {
          OutputBuffer::Fragment(_) | OutputBuffer::Whitespace(_) | OutputBuffer::Value(RantValue::String(_)) => {
            self.mode = OutputPrintMode::Text;
          },
          _ => {
            self.mode = OutputPrintMode::Concat;
          }
        }
      },
      (_, OutputPrintMode::List) => {
        if !matches!(value, OutputBuffer::Whitespace(_) | OutputBuffer::Value(RantValue::List(_))) {
          self.mode = OutputPrintMode::Text;
        }
      },
      (_, OutputPrintMode::Map) => {
        if !matches!(value, OutputBuffer::Whitespace(_) | OutputBuffer::Value(RantValue::Map(_))) {
          self.mode = OutputPrintMode::Text;
        }
      },
      _ => {}
    }

    self.buffers.push(value);
  }
  
  /// Writes a text fragment to the output.
  #[inline]
  pub fn write_frag(&mut self, value: &str) {
    self.write_buffer(OutputBuffer::Fragment(InternalString::from(value)));
  }
  
  /// Writes a whitespace string to the output.
  #[inline]
  pub fn write_ws(&mut self, value: &str) {
    let ws_str = match &self.format.ws_norm_mode {
      WhitespaceNormalizationMode::Default => DEFAULT_SPACE,
      WhitespaceNormalizationMode::IgnoreAll => return,
      WhitespaceNormalizationMode::Verbatim => value,
      WhitespaceNormalizationMode::Custom(val) => {
        let val = val.to_string();
        self.write_buffer(OutputBuffer::Whitespace(InternalString::from(&val)));
        return
      },
    };
    self.write_buffer(OutputBuffer::Whitespace(InternalString::from(ws_str)));
  }
}

impl OutputWriter {
  /// Consumes the output and returns the final value.
  #[inline]
  pub fn render_value(mut self) -> RantValue { 
    match self.buffers.len() {
      // An empty output always returns an empty value
      0 => RantValue::Empty,
      // Single buffer is always returned unchanged
      1 => {
        let buffer = self.buffers.pop().unwrap();
        match buffer {
          OutputBuffer::Fragment(s) | OutputBuffer::Whitespace(s) => RantValue::String(s.as_str().into()),
          OutputBuffer::Value(v) => v,
          _ => RantValue::Empty,
        }
      },
      _ => {
        match self.mode {
          OutputPrintMode::Single | OutputPrintMode::Text => {
            // Multiple buffers are concatenated into a single string, unless they are all empty
            let mut has_any_nonempty = false;
            let mut output = InternalString::new();
            let mut format: OutputFormat = Default::default();
            for buf in self.buffers {
              if let Some(s) = buf.render_string(&mut format) {
                has_any_nonempty = true;
                output.push_str(s.as_str());
              }
            }
            // If there is at least one non-empty, return the string; otherwise, return empty value
            if has_any_nonempty {
              RantValue::String(output.as_str().into())
            } else {
              RantValue::Empty
            }
          },
          OutputPrintMode::List => {
            let mut output = RantList::new();
            for buf in self.buffers {
              if let OutputBuffer::Value(RantValue::List(list)) = buf {
                output.extend(list.borrow().iter().cloned());
              }
            }
            RantValue::List(Rc::new(RefCell::new(output)))
          },
          OutputPrintMode::Map => {
            let mut output = RantMap::new();
            for buf in self.buffers {
              if let OutputBuffer::Value(RantValue::Map(map)) = buf {
                output.extend(map.borrow())
              }
            }
            RantValue::Map(Rc::new(RefCell::new(output)))
          },
          OutputPrintMode::Concat => {
            let mut val = RantValue::Empty;
            for buf in self.buffers {
              if let OutputBuffer::Value(bufval) = buf {
                val = val.concat(bufval);
              }
            }
            val
          }
        }
      }
    }
  }
}

impl Default for OutputWriter {
  fn default() -> Self {
    OutputWriter::new(Default::default())
  }
}

#[derive(Debug, Copy, Clone)]
enum OutputPrintMode {
  Single,
  Text,
  List,
  Map,
  Concat,
}

/// A unit of output.
#[derive(Debug)]
enum OutputBuffer {
  Fragment(InternalString),
  Whitespace(InternalString),
  Value(RantValue),
  NumberFormatUpdate(NumberFormat),
}

impl OutputBuffer {
  /// Consumes the buffer and returns its contents rendered as a single `String`.
  #[inline]
  pub(crate) fn render_string(self, format: &mut OutputFormat) -> Option<InternalString> {
    Some(match self {
      Self::Fragment(s) => s,
      Self::Whitespace(s) => s,
      Self::Value(RantValue::Empty) => return None,
      Self::Value(RantValue::Int(n)) => format.num_format.format_integer(n),
      Self::Value(RantValue::Float(n)) => format.num_format.format_float(n),
      Self::Value(v) => InternalString::from(v.to_string()),
      Self::NumberFormatUpdate(fmt) => {
        format.num_format = fmt;
        return None
      },
    })
  }
}