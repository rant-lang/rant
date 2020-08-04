use std::rc::Rc;
use crate::{random::RantRng, RantValue};

pub struct Resolver {
    rng: Rc<RantRng>
}

pub struct BlockState {
    reps: Reps,
    sep: RantValue,
}

pub enum Reps {
    Infinite,
    All,
    Finite(usize)
}

impl Resolver {
    pub fn new(rng: &Rc<RantRng>) -> Self {
        Self {
            rng: rng.clone()
        }
    }
}

impl Resolver {
    
}