use crate::{Rant, RantProgram, RantMap, syntax::{Sequence, RST, PrintFlag}, RantResult, RantError, RuntimeErrorType, RantString, random::RantRng};
use output::OutputWriter;
use std::{rc::{Rc}, cell::{RefCell}, ops::{Deref}};
use resolver::Resolver;
pub use stack::*;
pub use output::*;

mod resolver;
mod output;
mod stack;

pub const MAX_STACK_SIZE: usize = 20000;

pub struct VM<'a> {
    rng: Rc<RantRng>,
    engine: &'a mut Rant,
    program: &'a RantProgram,
    stack: RefCell<CallStack>,
    prev_frame: RefCell<Option<Rc<RefCell<StackFrame>>>>,
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

    pub fn cur_frame(&self) -> Rc<RefCell<StackFrame>> {
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
    #[inline]
    pub fn run(&self) -> RantResult<String> {
        // Push the root RST onto the stack
        self.push_frame(self.program.root.clone(), true)?;

        // Run whatever is on the top of the call stack
        'from_the_top: while !self.is_stack_empty() {
            let frame = self.cur_frame();
            let mut frame = frame.borrow_mut();

            // Write last frame's output (TODO: Write as individual buffers)
            if let Some(last_output) = self.take_last_output() {
                frame.write_frag(&last_output);
            }

            // Run frame's sequence elements in order
            while let Some(rst) = &frame.seq_next() {
                match Rc::deref(rst) {
                    RST::Fragment(frag) => frame.write_frag(frag),
                    RST::Whitespace(ws) => frame.write_ws(ws),
                    RST::Block(block) => {
                        let elem = Rc::clone(&block.elements[self.rng.next_usize(block.len())]);
                        self.push_frame(elem, block.flag != PrintFlag::Sink)?;
                        continue 'from_the_top;
                    },
                    _ => {}
                }
            }

            // Pop frame once its sequence is finished
            self.pop_frame()?;
        }

        // Once stack is empty, program is done-- return last frame's output
        Ok(self.take_last_output().unwrap_or_default().to_string())
    }

    #[inline(always)]
    fn is_stack_empty(&self) -> bool {
        self.stack.borrow().is_empty()
    }

    #[inline]
    fn take_last_output(&self) -> Option<RantString> {
        self.prev_frame.borrow_mut()
            .take()
            .map(|frame| frame.borrow_mut().render_output())
            .flatten()
    }

    #[inline]
    fn pop_frame(&self) -> RantResult<Rc<RefCell<StackFrame>>> {
        if let Some(frame) = self.stack.borrow_mut().pop() {
            self.prev_frame.replace(Some(frame.clone()));
            Ok(frame)
        } else {
            runtime_error!(StackUnderflow);
        }
    }

    #[inline]
    fn push_frame(&self, callee: Rc<Sequence>, has_output: bool) -> RantResult<Rc<RefCell<StackFrame>>> {
        let mut stack = self.stack.borrow_mut();

        // Check if this push would overflow the stack
        if stack.len() >= MAX_STACK_SIZE {
            runtime_error!(StackOverflow);
        }

        let frame = StackFrame::new(callee, Default::default(), has_output);
        stack.push(Rc::new(RefCell::new(frame)));
        Ok(stack.last().unwrap().clone())
    }
}