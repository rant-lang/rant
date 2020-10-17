use super::*;

pub(crate) fn eq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a == b));
  Ok(())
}

pub(crate) fn neq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a != b));
  Ok(())
}

pub(crate) fn lt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a < b));
  Ok(())
}

pub(crate) fn gt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a > b));
  Ok(())
}

pub(crate) fn le(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a <= b));
  Ok(())
}

pub(crate) fn ge(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::Boolean(a >= b));
  Ok(())
}