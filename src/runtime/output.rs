use crate::{RantValue};
use std::{rc::Rc};

pub struct OutputWriter<'a> {
    buffers: Vec<OutputBuffer<'a>>
}

impl<'a> OutputWriter<'a> {
    pub fn write_buffer(&mut self, value: OutputBuffer) {
        
    }
}

impl<'a> OutputWriter<'a> {
    pub fn render(mut self) -> String {
        let mut output = String::new();
        for buf in self.buffers {
            output.push_str(buf.render().as_str());
        }
        output
    }
}

pub enum OutputBuffer<'a> {
    String(String),
    Value(RantValue<'a>),
    Target(Rc<RantValue<'a>>)
}

impl<'a> OutputBuffer<'a> {
    /// Consumes the buffer and returns its contents rendered as a single `String`.
    pub(crate) fn render(self) -> String {
        match self {
            OutputBuffer::String(s) => s,
            OutputBuffer::Value(v) => v.as_string(),
            OutputBuffer::Target(t) => t.as_string()
        }
    }
}