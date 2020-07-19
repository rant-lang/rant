use crate::{RantEngine, RantProgram};

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