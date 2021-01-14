use crate::{RantValue, InternalString};
use super::format::{WhitespaceNormalizationMode, OutputFormat};
use std::rc::Rc;

const INITIAL_CHAIN_CAPACITY: usize = 64;
const DEFAULT_SPACE: &str = " ";

/// Writes a stream of buffers that can be passed to a parent buffer or rendered to a string.
pub struct OutputWriter {
  buffers: Vec<OutputBuffer>,
  frag_buffer: Option<InternalString>,
  format: Rc<OutputFormat>,
}

impl OutputWriter {
  pub fn new(prev_output: Option<&Self>) -> Self {
    Self {
      buffers: Vec::with_capacity(INITIAL_CHAIN_CAPACITY),
      frag_buffer: None,
      format: prev_output.map(|o| Rc::clone(&o.format)).unwrap_or_default(),
    }
  }

  #[inline]
  pub fn format(&self) -> &OutputFormat {
    &self.format
  }

  #[inline]
  pub fn format_mut(&mut self) -> &mut OutputFormat {
    Rc::make_mut(&mut self.format)
  }
  
  #[inline]
  pub fn write_buffer(&mut self, value: OutputBuffer) {
    self.flush_frag_buffer();
    self.buffers.push(value);
  }
  
  #[inline]
  pub fn write_frag(&mut self, value: &str) {
    // Consecutive string writes are buffered in a "frag buffer", which reduces the total number of buffer elements in the output
    if let Some(frag_buffer) = self.frag_buffer.as_mut() {
      frag_buffer.push_str(value);
    } else {
      self.frag_buffer = Some(InternalString::from(value))
    }
  }
  
  #[inline]
  pub fn write_ws(&mut self, value: &str) {
    match &self.format.ws_norm_mode {
      WhitespaceNormalizationMode::Default => {
        self.write_frag(DEFAULT_SPACE);
      },
      WhitespaceNormalizationMode::IgnoreAll => {},
      WhitespaceNormalizationMode::Verbatim => {
        self.write_frag(value);
      },
      WhitespaceNormalizationMode::Custom(val) => {
        let val = val.to_string();
        self.write_frag(&val);
      },
    }
  }
  
  #[inline]
  fn flush_frag_buffer(&mut self) {
    if let Some(frag_buffer) = self.frag_buffer.take() {
      self.buffers.push(OutputBuffer::String(frag_buffer));
    }
  }
}

impl OutputWriter {
  #[inline]
  pub fn render_value(mut self) -> RantValue {
    self.flush_frag_buffer();
    
    match self.buffers.len() {
      // An empty output always returns an empty value
      0 => RantValue::Empty,
      // Single buffer is always returned unchanged
      1 => {
        let buffer = self.buffers.pop().unwrap();
        match buffer {
          OutputBuffer::String(s) => RantValue::String(s.as_str().into()),
          OutputBuffer::Value(v) => v,
        }
      },
      // Multiple buffers are concatenated into a single string, unless they are all empty
      _ => {
        let mut has_any_nonempty = false;
        let mut output = InternalString::new();
        for buf in self.buffers {
          if !matches!(buf, OutputBuffer::Value(RantValue::Empty)) {
            has_any_nonempty = true;
            output.push_str(buf.render().as_str())
          }
        }
        // If there is at least one non-empty, return the string; otherwise, return empty value
        if has_any_nonempty {
          RantValue::String(output.as_str().into())
        } else {
          RantValue::Empty
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

/// A unit of output.
#[derive(Debug)]
pub enum OutputBuffer {
  String(InternalString),
  Value(RantValue)
}

impl<'a> OutputBuffer {
  /// Consumes the buffer and returns its contents rendered as a single `String`.
  #[inline]
  pub(crate) fn render(self) -> InternalString {
    match self {
      OutputBuffer::String(s) => s,
      OutputBuffer::Value(v) => InternalString::from(v.to_string())
    }
  }
}