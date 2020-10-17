use std::mem;

use super::*;
use crate::lang::PrintFlag;

/// `[$alt: a (any); b+ (any)]`
///
/// Prints the first argument that isn't an `empty`.
pub(crate) fn alt(vm: &mut VM, (a, mut b): (RantValue, RequiredVarArgs<RantValue>)) -> RantStdResult {
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

pub(crate) fn call(vm: &mut VM, (func, args): (RantFunctionRef, Option<Vec<RantValue>>)) -> RantStdResult {
  vm.push_val(RantValue::Function(Rc::clone(&func)))?;
  let argc = args.as_ref().map(|args| args.len()).unwrap_or(0);
  if let Some(mut args) = args {
    for arg in args.drain(..).rev() {
      vm.push_val(arg)?;
    }
  }
  vm.cur_frame_mut().push_intent_front(Intent::Call { argc, flag: PrintFlag::None, override_print: false });
  Ok(())
}

/// `[$copy: val (any)]`
///
/// Returns a copy of a value.
pub(crate) fn copy(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(val.shallow_copy());
  Ok(())
}

/// `[$either: cond (bool); a (any); b (any)]`
///
/// Prints `a` if `cond` is true, or `b` otherwise.
pub(crate) fn either(vm: &mut VM, (cond, a, b): (bool, RantValue, RantValue)) -> RantStdResult {
  let val = if cond { a } else { b };
  vm.cur_frame_mut().write_value(val);
  Ok(())
}

/// `$[fork: seed? (string|integer)]`
///
/// Forks the RNG with the specified seed.
pub(crate) fn fork(vm: &mut VM, seed: Option<RantValue>) -> RantStdResult {
  let rng = match seed {
    Some(RantValue::Integer(i)) => vm.rng().fork_i64(i),
    Some(RantValue::String(s)) => vm.rng().fork_str(&s),
    Some(other) => runtime_error!(RuntimeErrorType::ArgumentError, "seeding fork with '{}' value is not supported", other.type_name()),
    None => vm.rng().fork_random(),
  };
  vm.push_rng(Rc::new(rng));
  Ok(())
}

pub(crate) fn get_type(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_frag(val.type_name());
  Ok(())
}

/// `$[unfork]`
///
/// Unforks the RNG down one level.
pub(crate) fn unfork(vm: &mut VM, _: ()) -> RantStdResult {
  if vm.pop_rng().is_none() {
    runtime_error!(RuntimeErrorType::InvalidOperation, "cannot unfork root seed");
  }
  Ok(())
}

/// `[$nop]`
///
/// Does absolutely nothing. Intended for use as a default/placeholder callback.
pub(crate) fn nop(vm: &mut VM, _: VarArgs<RantEmpty>) -> RantStdResult {
  Ok(())
}

/// `[$seed]`
///
/// Prints the RNG seed currently in use.
pub(crate) fn seed(vm: &mut VM, _: ()) -> RantStdResult {
  let signed_seed = unsafe {
    mem::transmute::<u64, i64>(vm.rng().seed())
  };
  let frame = vm.cur_frame_mut();
  frame.write_value(RantValue::Integer(signed_seed));
  Ok(())
}

pub(crate) fn len(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Integer(val.len() as i64));
  Ok(())
}

pub(crate) fn error(vm: &mut VM, msg: Option<String>) -> RantStdResult {
  const DEFAULT_ERROR_MESSAGE: &str = "user error";
  Err(RuntimeError {
    error_type: RuntimeErrorType::UserError,
    description: msg.unwrap_or_else(|| DEFAULT_ERROR_MESSAGE.to_owned()),
    stack_trace: None,
  })
}

pub(crate) fn require(vm: &mut VM, module_path: String) -> RantStdResult {
  // Get name of module from path
  if let Some(module_name) = 
    PathBuf::from(&module_path)
    .with_extension("")
    .file_name()
    .map(|name| name.to_str())
    .flatten()
    .map(|name| name.to_owned())
  {
    // Check if module is cached
    if let Some(RantValue::Map(module_cache_ref)) = vm.context().get_global(crate::MODULES_CACHE_KEY) {
      if let Some(RantValue::Map(cached_module)) = module_cache_ref.borrow().raw_get(&module_name) {
        vm.cur_frame_mut().push_intent_front(Intent::LoadModule { module_name });
        vm.push_val(RantValue::Map(Rc::clone(cached_module)))?;
        return Ok(())
      }
    }

    // If not cached, attempt to load it from file
    let caller_origin = Rc::clone(&vm.cur_frame().origin());
    let module_pgm = vm.context_mut().try_load_module(&module_path, caller_origin).into_runtime_result()?;
    vm.cur_frame_mut().push_intent_front(Intent::LoadModule { module_name });
    vm.push_frame(Rc::clone(&module_pgm.root), true)?;
    Ok(())
  } else {
    runtime_error!(RuntimeErrorType::ArgumentError, "module name is missing from path");
  }
}