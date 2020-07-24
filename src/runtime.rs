use crate::{RantEngine, RantProgram};

mod resolver;

pub struct VM<'engine, 'pgm> {
    engine: &'engine mut RantEngine,
    program: &'pgm RantProgram
}

impl<'engine, 'pgm> VM<'engine, 'pgm> {
    pub fn new(engine: &'engine mut RantEngine, program: &'pgm RantProgram) -> Self {
        Self {
            engine,
            program
        }
    }
}