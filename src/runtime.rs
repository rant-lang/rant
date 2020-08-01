use crate::{Rant, RantProgram, RantMap, syntax::{Sequence, RST}, RantResult, RantError, RuntimeErrorType};
use output::OutputWriter;
use std::{rc::{Weak, Rc}, cell::{Cell, RefCell}, ops::{DerefMut, Deref}};
pub use stack::*;
pub use output::*;
use resolver::Resolver;
use random::RantRng;

mod resolver;
mod output;
mod random;
mod stack;

pub const MAX_STACK_SIZE: usize = 20000;

pub struct VM<'a> {
    rng: Rc<RantRng>,
    engine: &'a mut Rant,
    program: &'a RantProgram,
    stack: RefCell<CallStack<'a>>,
    prev_frame: RefCell<Option<Rc<RefCell<StackFrame<'a>>>>>,
    resolver: Resolver
}

impl<'a> VM<'a> {
    pub fn new(seed: u64, engine: &'a mut Rant, program: &'a RantProgram) -> Self {
        let rng = Rc::new(RantRng::new(seed));
        Self {
            resolver: Resolver::new(&rng),
            rng,
            engine,
            program,
            stack: Default::default(),
            prev_frame: Default::default(),
        }
    }

    pub fn cur_frame(&self) -> Rc<RefCell<StackFrame<'a>>> {
        self.stack.borrow().last().unwrap().clone()
    }
}

/// Returns a `RantResult::Err(RantError::RuntimeError { .. })` from the current execution context with the specified error type and optional description.
macro_rules! runtime_error {
    ($err_type:ident) => {
        return Err(RantError::RuntimeError {
            error_type: RuntimeErrorType::$err_type,
            description: None
        })
    };
    ($err_type:ident, $desc:expr) => {
        return Err(RantError::RuntimeError {
            error_type: RuntimeErrorType::$err_type,
            description: Some($desc.to_string())
        })
    };
}

impl<'a> VM<'a> {
    /// Runs the program.
    pub fn run(&'a self) -> RantResult<String> {
        // Push the root RST onto the stack
        self.push_frame(None, &self.program.root, true)?;

        // Run whatever is on the top of the call stack
        while !self.is_stack_empty() {
            let frame = self.cur_frame();
            let mut frame = frame.borrow_mut();

            // Run frame's sequence elements in order
            while let Some(rst) = frame.next() {
                match rst {
                    RST::Fragment(frag) => frame.write_frag(frag),
                    RST::Whitespace(ws) => frame.write_ws(ws),
                    RST::Block(block) => {
                        
                    },
                    _ => {}
                }
            }
            // Pop frame once its sequence is finished
            self.pop_frame()?;
        }

        // Once stack is empty, program is done-- return last frame's output
        Ok(self.take_last_output().unwrap_or_default())
    }

    #[inline(always)]
    fn is_stack_empty(&self) -> bool {
        self.stack.borrow().is_empty()
    }

    fn take_last_output(&self) -> Option<String> {
        self.prev_frame.borrow_mut().as_mut()
            .map(|frame| frame.borrow_mut().render_output())
            .flatten()
    }

    fn pop_frame(&'a self) -> RantResult<Rc<RefCell<StackFrame>>> {
        if let Some(frame) = self.stack.borrow_mut().pop() {
            self.prev_frame.replace(Some(frame.clone()));
            Ok(frame)
        } else {
            runtime_error!(StackUnderflow);
        }
    }

    fn push_frame(&'a self, caller: Option<&'a RST>, callee: &'a Sequence, has_output: bool) -> RantResult<Rc<RefCell<StackFrame>>> {
        let mut stack = self.stack.borrow_mut();

        // Check if this push would overflow the stack
        if stack.len() >= MAX_STACK_SIZE {
            runtime_error!(StackOverflow);
        }

        let frame = StackFrame::new(caller, callee, Default::default(), has_output);
        stack.push(Rc::new(RefCell::new(frame)));
        Ok(stack.last().unwrap().clone())
    }
}