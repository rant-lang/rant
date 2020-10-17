use super::*;
use crate::resolver::{SelectorMode, Reps, Selector};

pub(crate) fn resolve(vm: &mut VM, value: RantValue) -> RantStdResult {
  if let RantValue::Block(block) = value {
    vm.push_block(block.as_ref(), block.flag)?;
    Ok(())
  } else {
    Err(RuntimeError {
      error_type: RuntimeErrorType::ValueError(
        ValueError::InvalidConversion {
          from: value.type_name(),
          to: "block",
          message: None,
        }
      ),
      description: "value must be a block".to_owned(),
      stack_trace: None,
    })
  }
}

pub(crate) fn rep(vm: &mut VM, reps: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().reps = match reps {
    RantValue::Integer(n) => Reps::Repeat(n.max(0) as usize),
    RantValue::String(s) => match s.as_str() {
      "once" => Reps::Once,
      "all" => Reps::All,
      "forever" => Reps::RepeatForever,
      _ => return Err(RuntimeError {
        error_type: RuntimeErrorType::ArgumentError,
        description: format!("unknown repetition mode: '{}'", s),
        stack_trace: None,
      })
    },
    _ => return Err(RuntimeError {
      error_type: RuntimeErrorType::ArgumentError,
      description: format!("value of type '{}' cannot be used as repetition value", reps.type_name()),
      stack_trace: None,
    })
  };
  Ok(())
}

pub(crate) fn sep(vm: &mut VM, separator: RantValue) -> RantStdResult {
  vm.resolver_mut().attrs_mut().separator = separator;
  Ok(())
}

pub(crate) fn step_index(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_index());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

pub(crate) fn step(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

pub(crate) fn step_count(vm: &mut VM, _: ()) -> RantStdResult {
  let n = vm.resolver().active_block().map_or(0, |block| block.step_count());
  vm.cur_frame_mut().write_value(RantValue::Integer(n as i64));
  Ok(())
}

pub(crate) fn mksel(vm: &mut VM, mode: SelectorMode) -> RantStdResult {
  let selector = Rc::new(RefCell::new(Selector::new(mode)));
  let special = RantSpecial::Selector(selector);
  vm.cur_frame_mut().write_value(RantValue::Special(special));
  Ok(())
}

pub(crate) fn sel(vm: &mut VM, selector: Option<RantValue>) -> RantStdResult {
  vm.resolver_mut().attrs_mut().selector = match selector {
    Some(RantValue::Special(RantSpecial::Selector(selector))) => {
      Some(Rc::clone(&selector))
    },
    Some(val) => {
      return Err(RuntimeError {
        error_type: RuntimeErrorType::ValueError(ValueError::InvalidConversion {
          from: val.type_name(),
          to: "selector",
          message: None,
        }),
        description: "value is not a selector".to_owned(),
        stack_trace: None,
      })
    },
    None => None,
  };
  Ok(())
}

pub(crate) fn push_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().push_attrs();
  Ok(())
}

pub(crate) fn pop_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().pop_attrs();
  Ok(())
}

pub(crate) fn count_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().count_attrs();
  Ok(())
}

pub(crate) fn reset_attrs(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().reset_attrs();
  Ok(())
}