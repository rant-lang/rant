use crate::{syntax::RST, RantMap};

pub struct StackFrame<'a> {
    /// Variables local to stack frame
    locals: RantMap,
    /// RST node that triggered this frame's creation
    caller: &'a RST,
    /// Node sequence being executed by the frame
    sequence: &'a Vec<RST>,
    /// Program Counter (as index in sequence) for the current frame
    pc: usize
}