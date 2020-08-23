//! # Rant standard library

#![allow(unused_variables)]

use std::rc::Rc;
use crate::runtime::*;
use crate::{RantValue, AsRantForeignFunc, RantMap, RequiredVarArgs, RantList, RantMapRef, IntoRuntimeResult, RuntimeError};
use std::mem;

pub(crate) type RantStdResult = Result<(), RuntimeError>;

fn alt(vm: &mut VM, (a, mut b): (RantValue, RequiredVarArgs<RantValue>)) -> RantStdResult {
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

fn seed(vm: &mut VM, _: ()) -> RantStdResult {
  // Yeah, I know, but this is pretty much guaranteed not to fail
  let signed_seed = unsafe {
    mem::transmute::<u64, i64>(vm.rng().seed())
  };
  let frame = vm.cur_frame_mut();
  frame.write_value(RantValue::Integer(signed_seed));
  Ok(())
}

fn add(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs + rhs);
  Ok(())
}

fn mul(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * rhs);
  Ok(())
}

fn mul_add(vm: &mut VM, (lhs, mhs, rhs): (RantValue, RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs * mhs + rhs);
  Ok(())
}

fn sub(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs - rhs);
  Ok(())
}

fn div(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(lhs / rhs);
  Ok(())
}

fn len(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Integer(val.len() as i64));
  Ok(())
}

fn get_type(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_frag(val.type_name());
  Ok(())
}

fn num(vm: &mut VM, (a, b): (i64, i64)) -> RantStdResult {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Integer(n));
  Ok(())
}

fn numf(vm: &mut VM, (a, b): (f64, f64)) -> RantStdResult {
  let n = vm.rng().next_f64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Float(n));
  Ok(())
}

fn pick(vm: &mut VM, list: RantValue) -> RantStdResult {
  let index = vm.rng().next_usize(list.len());
  let item = list.get_by_index(index as i64).into_runtime_result()?;
  vm.cur_frame_mut().write_value(item);
  Ok(())
}

fn join(vm: &mut VM, (sep, list): (RantValue, Vec<RantValue>)) -> RantStdResult {
  let mut is_first = true;
  let frame = vm.cur_frame_mut();
  for val in list {
    if is_first {
      is_first = false;
    } else {
      frame.write_value(sep.clone());
    }
    frame.write_value(val);
  }
  Ok(())
}

fn proto(vm: &mut VM, map: RantMapRef) -> RantStdResult {
  vm.cur_frame_mut().write_value(map.borrow().proto().map_or(RantValue::Empty, RantValue::Map));
  Ok(())
}

fn set_proto(vm: &mut VM, (map, proto): (RantMapRef, Option<RantMapRef>)) -> RantStdResult {
  map.borrow_mut().set_proto(proto);
  Ok(())
}

fn to_int(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_int());
  Ok(())
}

fn to_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_float());
  Ok(())
}

fn to_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(value.into_rant_string());
  Ok(())
}

fn get(vm: &mut VM, key: String) -> RantStdResult {
  let val = vm.get_local(key.as_str())?;
  vm.cur_frame_mut().write_value(val);
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
    // General functions
    alt, len, get_type as "type", seed,

    // Math functions
    add, sub, mul, div, mul_add as "mul-add",

    // Conversion functions
    to_int as "int", to_float as "float", to_string as "string",

    // Generator functions
    num as "n", numf as "nf",

    // Prototype functions
    proto, set_proto as "set-proto",

    // List functions
    pick, join,

    // Dynamic Variable Access functions
    get
  );
}