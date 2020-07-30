use crate::{Rant, RantProgram};
use stack::StackFrame;

mod resolver;
mod output;
mod stack;
mod random;

pub struct VM<'a> {
    pub(crate) engine: &'a mut Rant,
    pub(crate) program: &'a RantProgram,
    pub(crate) stack: Vec<StackFrame<'a>>
}

impl<'a> VM<'a> {
    pub fn new(engine: &'a mut Rant, program: &'a RantProgram) -> Self {
        Self {
            engine,
            program,
            stack: Default::default()
        }
    }
}

impl<'a> VM<'a> {
    pub fn run() -> String {
        todo!()
    }
}