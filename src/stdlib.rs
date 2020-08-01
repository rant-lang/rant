//* # stdlib

#![allow(unused_variables)]

use crate::{RantResult, runtime::VM};
use crate::{RantValue, AsRantForeignFunc};

pub fn rep(vm: &mut VM, reps: i32) -> RantResult<()> {
    todo!()
}

pub fn rs(vm: &mut VM, (reps, sep): (i32, RantValue)) -> RantResult<()> {
    todo!()
}

macro_rules! print_func {
    ($name:ident) => {
        println!("{} = {:?}", stringify!($name), $name.as_rant_func());
    }
}

pub fn print_stdlib() 
{
    print_func!(rep);
    print_func!(rs);
}