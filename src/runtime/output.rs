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
        self.buffers.push(value);
    }

    pub fn write_frag(&mut self, value: &str) {
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
            self.write_buffer(OutputBuffer::String(frag_buffer));
        }
    }
}

impl OutputWriter {
    pub fn render(mut self) -> RantString {
        self.flush_frag_buffer();
        let mut output = RantString::new();
        for buf in self.buffers {
            output.push_str(buf.render().as_str());
        }
        output
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