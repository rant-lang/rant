use crate::{RantValue, RantString};

const INITIAL_CHAIN_CAPACITY: usize = 64;

/// Writes a stream of buffers that can be passed to a parent buffer or rendered to a string.
pub struct OutputWriter {
  buffers: Vec<OutputBuffer>,
  frag_buffer: Option<RantString>
}

impl OutputWriter {
  pub fn new() -> Self {
    Self {
      buffers: Vec::with_capacity(INITIAL_CHAIN_CAPACITY),
      frag_buffer: None
    }
  }
  
  pub fn write_buffer(&mut self, value: OutputBuffer) {
    self.flush_frag_buffer();
    self.buffers.push(value);
  }
  
  pub fn write_frag(&mut self, value: &str) {
    // Consecutive string writes are buffered in a "frag buffer", which reduces the total number of buffer elements in the output
    if let Some(frag_buffer) = self.frag_buffer.as_mut() {
      frag_buffer.push_str(value);
    } else {
      self.frag_buffer = Some(RantString::from(value))
    }
  }
  
  pub fn write_ws(&mut self, value: &str) {
    self.write_frag(" ");
    // TODO: Whitespace formatting
  }
  
  fn flush_frag_buffer(&mut self) {
    if let Some(frag_buffer) = self.frag_buffer.take() {
      self.buffers.push(OutputBuffer::String(frag_buffer));
    }
  }
}

impl OutputWriter {
  pub fn render_string(mut self) -> RantString {
    self.flush_frag_buffer();
    let mut output = RantString::new();
    for buf in self.buffers {
      output.push_str(buf.render().as_str());
    }
    output
  }

  pub fn render_value(mut self) -> RantValue {
    self.flush_frag_buffer();
    
    match self.buffers.len() {
      // An empty output always returns an empty value
      0 => RantValue::Empty,
      // Single buffer is always returned unchanged
      1 => {
        let buffer = self.buffers.pop().unwrap();
        match buffer {
          OutputBuffer::String(s) => RantValue::String(s.to_string()),
          OutputBuffer::Value(v) => v,
        }
      },
      // Multiple buffers are concatenated into a single string, unless are are empty
      _ => {
        let mut has_any_nonempty = false;
        let mut output = RantString::new();
        for buf in self.buffers {
          if !matches!(buf, OutputBuffer::Value(RantValue::Empty)) {
            has_any_nonempty = true;
            output.push_str(buf.render().as_str())
          }
        }
        // If there is at least one non-empty, return the string; otherwise, return empty value
        if has_any_nonempty {
          RantValue::String(output.to_string())
        } else {
          RantValue::Empty
        }
      }
    }
  }
}

impl Default for OutputWriter {
  fn default() -> Self {
    OutputWriter::new()
  }
}

/// A unit of output.
pub enum OutputBuffer {
  String(RantString),
  Value(RantValue)
}

impl<'a> OutputBuffer {
  /// Consumes the buffer and returns its contents rendered as a single `String`.
  pub(crate) fn render(self) -> RantString {
    match self {
      OutputBuffer::String(s) => s,
      OutputBuffer::Value(v) => RantString::from(v.to_string())
    }
  }
}