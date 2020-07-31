use crate::{RantValue};
use std::{rc::Rc};

pub struct OutputWriter {
    buffers: Vec<OutputBuffer>
}

impl OutputWriter {
    pub fn write_buffer(&mut self, value: OutputBuffer) {
        
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