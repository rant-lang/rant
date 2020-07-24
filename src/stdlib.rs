use crate::{RantResult, runtime::{VM}, FromRant, RantValue, ToRant, RantFunction};
use std::rc::Rc;

pub fn rant_r(vm: &mut VM, reps: i32) -> RantResult<()> {
    todo!()
}

// fn create_rant_func<'a, F, A>(mut func: F) -> RantFunction<'a>
// where F: FnMut(&mut VM) -> RantResult<()>,
// A: FromRant
// {
//     RantFunction::Native(Rc::new(|vm, args| {
        
//     }))
// }