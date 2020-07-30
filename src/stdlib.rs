use crate::{RantResult, runtime::VM};
use crate::{RantValue, convert::RantForeignFunc};

pub fn rep(vm: &mut VM, reps: i32) -> RantResult<()> {
    todo!()
}

fn print_func(name: &str, func: &'static dyn RantForeignFunc) {
    println!("{} = {:?}", name, func.as_rant_func())
}

pub fn print_stdlib() 
{
    print_func("rep", &rep);
}