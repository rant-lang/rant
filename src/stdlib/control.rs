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