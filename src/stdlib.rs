//! The Rant standard library.

#![allow(unused_variables)]

use std::rc::Rc;
use crate::*;
use crate::runtime::*;
use crate::convert::*;
use crate::convert::ToRant;
use std::mem;
use lang::PrintFlag;

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
  vm.cur_frame_mut().write_value((lhs / rhs).into_runtime_result()?);
  Ok(())
}

fn rem(vm: &mut VM, (lhs, rhs): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value((lhs % rhs).into_runtime_result()?);
  Ok(())
}

fn neg(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(-val);
  Ok(())
}

fn recip(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value((RantValue::Float(1.0) / val).into_runtime_result()?);
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

fn hex(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = (&b"0123456789abcdef")[rng.next_usize(16)] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn dec(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = (&b"0123456789")[rng.next_usize(10)] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

fn maybe(vm: &mut VM, p: f64) -> RantStdResult {
  let b = vm.rng().next_bool(p);
  vm.cur_frame_mut().write_value(RantValue::Boolean(b));
  Ok(())
}

fn pick(vm: &mut VM, list: RantValue) -> RantStdResult {
  let index = vm.rng().next_usize(list.len());
  let item = list.index_get(index as i64).into_runtime_result()?;
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

fn seg(vm: &mut VM, (s, seg_size): (String, usize)) -> RantStdResult {
  if seg_size > 0 {
    let mut segs = vec![];
    let len = s.len();
    let last_seg_len = len % seg_size;
    let n = len / seg_size + (last_seg_len % 2);
    if last_seg_len > 0 {
      for i in 0..n {
        if i == n - 1 {
          segs.push(s[i * seg_size .. i * seg_size + last_seg_len].to_owned());
        } else {
          segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
        }
      }
    } else {
      for i in 0..n {
        segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
      }
    }
    vm.cur_frame_mut().write_value(segs.to_rant().into_runtime_result()?);
  }
  Ok(())
}

fn split(vm: &mut VM, (s, at): (String, Option<String>)) -> RantStdResult {
  let list = if at.as_ref().map(|s| s.is_empty()).unwrap_or(true) {
    s.chars()
      .map(|c| c.to_string())
      .collect::<Vec<String>>()
  } else {
    s
      .split(at.unwrap().as_str())
      .map(|part| part.to_owned())
      .collect::<Vec<String>>()
  }.to_rant().into_runtime_result()?;

  vm.cur_frame_mut().write_value(list);
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

fn upper(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_uppercase().as_str());
  Ok(())
}

fn lower(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_lowercase().as_str());
  Ok(())
}

fn call(vm: &mut VM, (func, args): (RantFunctionRef, Option<Vec<RantValue>>)) -> RantStdResult {
  vm.push_val(RantValue::Function(Rc::clone(&func)))?;
  let argc = args.as_ref().map(|args| args.len()).unwrap_or(0);
  if let Some(mut args) = args {
    for arg in args.drain(..).rev() {
      vm.push_val(arg)?;
    }
  }
  vm.cur_frame_mut().push_intent_front(Intent::Call { argc, flag: PrintFlag::None });
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
    alt, call, len, get_type as "type", seed,

    // Math functions
    add, sub, mul, div, mul_add as "mul-add", rem as "mod", neg, recip,

    // Conversion functions
    to_int as "int", to_float as "float", to_string as "string",

    // Generator functions
    dec, hex, maybe, num as "n", numf as "nf",

    // Prototype functions
    proto, set_proto as "set-proto",

    // List functions
    pick, join,

    // String functions
    lower, upper, seg, split,

    // Dynamic Variable Access functions
    get
  );
}