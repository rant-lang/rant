use super::*;

pub(crate) fn if_(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_if(condition);
  Ok(())
}

pub(crate) fn else_if(vm: &mut VM, condition: bool) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else_if(condition);
  Ok(())
}

pub(crate) fn else_(vm: &mut VM, _: ()) -> RantStdResult {
  vm.resolver_mut().attrs_mut().make_else();
  Ok(())
}

pub(crate) fn break_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.interrupt_repeater(val, false)?;
  Ok(())
}

pub(crate) fn continue_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.interrupt_repeater(val, true)?;
  Ok(())
}

pub(crate) fn return_(vm: &mut VM, val: Option<RantValue>) -> RantStdResult {
  vm.func_return(val)?;
  Ok(())
}