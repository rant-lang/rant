use std::{cell::RefCell, rc::Rc, ops::{DerefMut, Deref}};
use crate::{syntax::{Sequence, RST}, RantMap};
use super::output::OutputWriter;

/// Thin wrapper around call stack vector
pub struct CallStack<'a>(Vec<Rc<RefCell<StackFrame<'a>>>>);

// Allows direct immutable access to internal stack vector
impl<'a> Deref for CallStack<'a> {
    type Target = Vec<Rc<RefCell<StackFrame<'a>>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Allows direct mutable access to internal stack vector
impl<'a> DerefMut for CallStack<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for CallStack<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl CallStack<'_> {
    pub fn new() -> Self {
        Self(Default::default())
    }
}

pub struct StackFrame<'a> {
    /// Variables local to stack frame
    locals: RantMap,
    /// RST node that triggered this frame's creation
    caller: Option<&'a RST>,
    /// Node sequence being executed by the frame
    sequence: &'a Sequence,
    /// Program Counter (as index in sequence) for the current frame
    pc: usize,
    /// Has frame sequence started running?
    started: bool,
    /// Output for this stack frame
    output: Option<OutputWriter>
}

// Allows stack frame's RST sequence to be iterated and automatically increments the PC
impl<'a> Iterator for StackFrame<'a> {
    type Item = &'a RST;
    fn next(&mut self) -> Option<Self::Item> {
        if self.is_done() {
            return None
        }

        // Increment PC
        if self.started {
            self.pc += 1;
        } else {
            self.started = true;
        }

        self.sequence.get(self.pc)
    }
}

impl<'a> StackFrame<'a> {
    pub fn new(caller: Option<&'a RST>, sequence: &'a Sequence, locals: RantMap, has_output: bool) -> Self {
        Self {
            caller,
            sequence,
            locals,
            output: if has_output { Some(Default::default()) } else { None },
            started: false,
            pc: 0
        }
    }
}

impl StackFrame<'_> {
    #[inline(always)]
    fn is_done(&self) -> bool {
        self.pc >= self.sequence.len()
    }

    pub fn write_frag(&mut self, frag: &str) {
        if let Some(output) = self.output.as_mut() {
            output.write_frag(frag);
        }
    }

    pub fn write_ws(&mut self, ws: &str) {
        if let Some(output) = self.output.as_mut() {
            output.write_ws(ws);
        }
    }

    pub fn render_output(&mut self) -> Option<String> {
        self.output.take().map(|o| o.render())
    }
}