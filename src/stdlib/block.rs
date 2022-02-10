use super::*;
use crate::runtime::resolver::Reps;

pub fn if_(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_if(condition);
  Ok(())
}

pub fn elseif(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else_if(condition);
  Ok(())
}

pub fn else_(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else();
  Ok(())
}

pub fn rep(vm: &mut VM, reps: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().reps = match reps {
    RantValue::Int(n) => Reps::Repeat(n.max(0) as usize),
    RantValue::String(s) => match s.as_str() {
      "once" => Reps::Once,
      "all" => Reps::All,
      "forever" => Reps::Forever,
      _ => return Err(RuntimeError {
        error_type: RuntimeErrorType::ArgumentError,
        description: Some(format!("unknown repetition mode: '{}'", s)),
        stack_trace: None,
      })
    },
    _ => return Err(RuntimeError {
      error_type: RuntimeErrorType::ArgumentError,
      description: Some(format!("value of type '{}' cannot be used as repetition value", reps.type_name())),
      stack_trace: None,
    })
  };
  Ok(())
}

pub fn sep(vm: &mut VM, separator: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().separator = separator;
  Ok(())
}

pub fn mut_(vm: &mut VM, mutator_func: Option<RantFunctionHandle>) -> RantStdResult {
  vm.resolver_mut().attrs_mut().mutator = mutator_func;
  Ok(())
}

pub fn step_index(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_index());
  vm.cur_frame_mut().write(n as i64);
  Ok(())
}

pub fn step(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step());
  vm.cur_frame_mut().write(n as i64);
  Ok(())
}

pub fn step_count(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_count());
  vm.cur_frame_mut().write(n as i64);
  Ok(())
}

pub fn mksel(vm: &mut VM, mode: SelectorMode) -> RantStdResult {
  let sel = RantSelector::new(mode);
  vm.cur_frame_mut().write(sel);
  Ok(())
}

pub fn sel(vm: &mut VM, selector: Option<RantValue>) -> RantStdResult {
  match selector {
    Some(RantValue::Selector(handle)) => {
      vm.resolver_mut().attrs_mut().selector = Some(handle.clone());
    },
    Some(val @ RantValue::String(_)) => {
      let mode = SelectorMode::try_from_rant(val).into_runtime_result()?;
      let selector = RantSelector::new(mode).into_handle();
      vm.resolver_mut().attrs_mut().selector = Some(selector);
    },
    Some(val) => {
      return Err(RuntimeError {
        error_type: RuntimeErrorType::ValueError(ValueError::InvalidConversion {
          from: val.type_name(),
          to: "selector",
          message: None,
        }),
        description: Some("value is not a selector".to_owned()),
        stack_trace: None,
      })
    },
    None => {
      let selector = vm.resolver().attrs().selector
        .clone()
        .map(|s| s.into_rant())
        .unwrap_or(RantValue::Nothing);
      vm.cur_frame_mut().write(selector);
    },
  }
  Ok(())
}

pub fn sel_skip(vm: &mut VM, (selector, n): (RantSelectorHandle, Option<usize>)) -> RantStdResult {
  let mut sel = selector.borrow_mut();
  let count = sel.count();
  let n = n.unwrap_or(1);
  for _ in 0..n {
    sel.select(count, vm.rng()).into_runtime_result()?;
  }
  Ok(())
}

pub fn sel_freeze(vm: &mut VM, (selector, frozen): (RantSelectorHandle, Option<bool>)) -> RantStdResult {
  let mut sel = selector.borrow_mut();
  sel.set_frozen(frozen.unwrap_or(true));
  Ok(())
}

pub fn sel_frozen(vm: &mut VM, (selector, frozen): (RantSelectorHandle, bool)) -> RantStdResult {
  let sel = selector.borrow();
  vm.cur_frame_mut().write(sel.is_frozen());
  Ok(())
}

pub fn reset_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().reset_attrs();
  Ok(())
}