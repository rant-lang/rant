use std::mem;

use data::DataSourceError;

use super::*;
use crate::lang::{VarAccessMode};

/// `[$alt: a (any); b+ (any)]`
///
/// Prints the first argument that isn't an `empty`.
pub(crate) fn alt(vm: &mut VM, (a, mut b): (RantValue, RequiredVarArgs<RantValue>)) -> RantStdResult {
  if !a.is_nothing() {
    vm.cur_frame_mut().write(a);
    Ok(())
  } else {
    for val in b.drain(..) {
      if !val.is_nothing() {
        vm.cur_frame_mut().write(val);
        break
      }
    }
    Ok(())
  }
}

pub(crate) fn call(vm: &mut VM, (func, args): (RantFunctionHandle, Option<Vec<RantValue>>)) -> RantStdResult {
  vm.push_val(RantValue::Function(Rc::clone(&func)))?;
  let argc = args.as_ref().map(|args| args.len()).unwrap_or(0);
  if let Some(mut args) = args {
    for arg in args.drain(..).rev() {
      vm.push_val(arg)?;
    }
  }
  vm.cur_frame_mut().push_intent(Intent::Call { argc, override_print: false });
  Ok(())
}

pub(crate) fn cat(vm: &mut VM, mut args: VarArgs<RantValue>) -> RantStdResult {
  let frame = vm.cur_frame_mut();
  for val in args.drain(..) {
    frame.write(val);
  }
  
  Ok(())
}

pub(crate) fn print(vm: &mut VM, mut args: VarArgs<RantValue>) -> RantStdResult {
  if args.len() < 2 {
    let frame = vm.cur_frame_mut();
    for val in args.drain(..) {
      frame.write(val);
    }
  } else if let Some(frame) = vm.parent_frame_mut(1) {
    for val in args.drain(..) {
      frame.write(val);
    }
  }
  
  Ok(())
}

/// `[$copy: val (any)]`
///
/// Returns a copy of a value.
pub(crate) fn copy(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(val.shallow_copy());
  Ok(())
}

/// `[$either: cond (bool); a (any); b (any)]`
///
/// Prints `a` if `cond` is true, or `b` otherwise.
pub(crate) fn either(vm: &mut VM, (cond, a, b): (bool, RantValue, RantValue)) -> RantStdResult {
  let val = if cond { a } else { b };
  vm.cur_frame_mut().write(val);
  Ok(())
}

/// `$[fork: seed? (string|int)]`
///
/// Forks the RNG with the specified seed.
pub(crate) fn fork(vm: &mut VM, seed: Option<RantValue>) -> RantStdResult {
  let rng = match seed {
    Some(RantValue::Int(i)) => vm.rng().fork_i64(i),
    Some(RantValue::String(s)) => vm.rng().fork_str(s.as_str()),
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

/// Does nothing and takes any number of arguments. Use this as a no-op or non-printing temporal pipe.
pub(crate) fn tap(vm: &mut VM, _: VarArgs<RantNothing>) -> RantStdResult {
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
  frame.write(RantValue::Int(signed_seed));
  Ok(())
}

pub(crate) fn len(vm: &mut VM, val: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(val.len().try_into_rant().into_runtime_result()?);
  Ok(())
}

pub(crate) fn error(vm: &mut VM, msg: Option<String>) -> RantStdResult {
  const DEFAULT_ERROR_MESSAGE: &str = "user error";
  Err(RuntimeError {
    error_type: RuntimeErrorType::UserError,
    description: msg,
    stack_trace: None,
  })
}

pub(crate) fn range(vm: &mut VM, (a, b, step): (i64, Option<i64>, Option<u64>)) -> RantStdResult {
  let step = step.unwrap_or(1);
  
  let range = if let Some(b) = b {
    RantRange::new(a, b, step)
  } else {
    RantRange::new(0, a, step)
  };

  vm.cur_frame_mut().write(range);
  Ok(())
}

pub(crate) fn irange(vm: &mut VM, (a, b, step): (i64, Option<i64>, Option<u64>)) -> RantStdResult {
  let step = step.unwrap_or(1);
  
  let range = if let Some(b) = b {
    RantRange::new(a, b + if a <= b { 1 } else { -1 }, step)
  } else {
    RantRange::new(0, a + if a >= 0 { 1 } else { -1 }, step)
  };

  vm.cur_frame_mut().write(range);
  Ok(())
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
    // Check if module is cached; if so, don't do anything
    if let Some(cached_module) = vm.context().get_cached_module(&module_name) {
      vm.def_var_value(module_name.as_str(), VarAccessMode::Descope(1), cached_module.clone(), true)?;
      return Ok(())
    }

    // If not cached, attempt to resolve it and load the module
    let dependant = Rc::clone(vm.cur_frame().origin());
    let module_resolver = Rc::clone(&vm.context().module_resolver);
    match module_resolver.try_resolve(vm.context_mut(), module_path.as_str(), Some(&dependant.as_ref())) {
      Ok(module_program) => {
        vm.cur_frame_mut().push_intent(Intent::ImportLastAsModule { module_name, descope: 1 });
        vm.push_frame_flavored(Rc::clone(&module_program.root), StackFrameFlavor::FunctionBody)?;
        Ok(())
      },
      Err(err) => runtime_error!(RuntimeErrorType::ModuleError(err)),
    }
  } else {
    runtime_error!(RuntimeErrorType::ArgumentError, "missing module name from path: {}", module_path);
  }
}

pub(crate) fn try_(vm: &mut VM, (context, handler): (RantValue, Option<RantFunctionHandle>)) -> RantStdResult {
  vm.push_unwind_state(handler);
  vm.cur_frame_mut().push_intent(Intent::DropStaleUnwinds);
  match context {
    RantValue::Function(func) => {
      vm.call_func(func, vec![], false)?;
    },
    other => runtime_error!(RuntimeErrorType::ArgumentError, "try: cannot protect '{}' value; only functions and blocks can be protected", other.get_type())
  }
  Ok(())
}

pub(crate) fn ds_request(vm: &mut VM, (dsid, args): (InternalString, VarArgs<RantValue>)) -> RantStdResult {
  match vm.context().data_source(dsid.as_str()) {
    Some(ds) => {
      let result = ds.request_data(args.into_vec()).into_runtime_result()?;
      vm.cur_frame_mut().write(result);
    },
    None => {
      runtime_error!(RuntimeErrorType::DataSourceError(DataSourceError::User(format!("data source '{}' not found", &dsid))))
    }
  }
  Ok(())
}

pub(crate) fn ds_query_sources(vm: &mut VM, _: ()) -> RantStdResult {
  let sources = vm.context().iter_data_sources().map(|(id, _)| id.into_rant()).collect::<RantList>();
  vm.cur_frame_mut().write(sources);
  Ok(())
}