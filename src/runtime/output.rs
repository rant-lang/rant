use crate::{RantValue};

/// Writes a stream of buffers that can be passed to a parent buffer or rendered to a string.
pub struct OutputWriter {
    buffers: Vec<OutputBuffer>,
    frag_buffer: Option<String>
}

impl OutputWriter {
    pub fn new() -> Self {
        Self {
            buffers: Default::default(),
            frag_buffer: None
        }
    }

    pub fn write_buffer(&mut self, value: OutputBuffer) {
        todo!()
    }

    pub fn write_frag(&mut self, value: &str) {
        if let Some(frag_buffer) = self.frag_buffer.as_mut() {
            frag_buffer.push_str(value);
        } else {
            self.frag_buffer = Some(value.to_owned())
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
    pub fn render(self) -> String {
        let mut output = String::new();
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
    String(String),
    Value(RantValue)
}

impl<'a> OutputBuffer {
    /// Consumes the buffer and returns its contents rendered as a single `String`.
    pub(crate) fn render(self) -> String {
        match self {
            OutputBuffer::String(s) => s,
            OutputBuffer::Value(v) => v.as_string()
        }
    }
}