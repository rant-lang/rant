//! # Rant standard library

#![allow(unused_variables)]

use std::rc::Rc;
use crate::{RantResult, runtime::{self, VM}};
use crate::{RantValue, AsRantForeignFunc, RantMap, RequiredVarArgs};

fn alt(vm: &mut VM, (a, mut b): (RantValue, RequiredVarArgs<RantValue>)) -> RantResult<()> {
  if !a.is_empty() {
    vm.cur_frame_mut().write_value(a);
    Ok(())
  } else {
    for val in b.drain(..) {
      if !val.is_empty() {
        vm.cur_frame_mut().write_value(val);
        break
      }
    }
    Ok(())
  }
}

fn add(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantResult<()> {
  vm.cur_frame_mut().write_value(lhs + rhs);
  Ok(())
}

fn mul(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantResult<()> {
  vm.cur_frame_mut().write_value(lhs * rhs);
  Ok(())
}

fn mul_add(vm: &mut VM, (lhs, mhs, rhs): (RantValue, RantValue, RantValue)) -> RantResult<()> {
  vm.cur_frame_mut().write_value(lhs * mhs + rhs);
  Ok(())
}

fn sub(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantResult<()> {
  vm.cur_frame_mut().write_value(lhs - rhs);
  Ok(())
}

fn div(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantResult<()> {
  vm.cur_frame_mut().write_value(lhs / rhs);
  Ok(())
}

fn len(vm: &mut VM, val: RantValue) -> RantResult<()> {
  vm.cur_frame_mut().write_value(RantValue::Integer(val.len() as i64));
  Ok(())
}

fn get_type(vm: &mut VM, val: RantValue) -> RantResult<()> {
  vm.cur_frame_mut().write_frag(val.type_name());
  Ok(())
}

fn num(vm: &mut VM, (a, b): (i64, i64)) -> RantResult<()> {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Integer(n));
  Ok(())
}

fn pick(vm: &mut VM, list: RantValue) -> RantResult<()> {
  let index = vm.rng().next_usize(list.len());
  let item = runtime::convert_index_result(list.get_by_index(index as i64))?;
  vm.cur_frame_mut().write_value(item);
  Ok(())
}

pub(crate) fn load_stdlib(globals: &mut RantMap)
{
  macro_rules! load_func {
    ($fname:ident) => {{
      let func = $fname.as_rant_func();
      globals.raw_set(stringify!($fname), RantValue::Function(Rc::new(func)));
    }};
    ($fname:ident, $id:literal) => {{
      let func = $fname.as_rant_func();
      globals.raw_set($id, RantValue::Function(Rc::new(func)));
    }};
  }

  macro_rules! load_funcs {
    ($($fname:ident $(as $id:expr)?),+) => {
      $(load_func!($fname$(, $id)?);)+
    };
  }

  load_funcs!(
    alt,
    add, sub, mul, div, 
    mul_add as "mul-add",
    len,
    num as "n",
    pick,
    get_type as "type"
  );
}