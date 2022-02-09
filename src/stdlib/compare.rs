use super::*;

pub fn eq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a == b);
  Ok(())
}

pub fn neq(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a != b);
  Ok(())
}

pub fn lt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a < b);
  Ok(())
}

pub fn gt(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a > b);
  Ok(())
}

pub fn le(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a <= b);
  Ok(())
}

pub fn ge(vm: &mut VM, (a, b): (RantValue, RantValue)) -> RantStdResult {
  vm.cur_frame_mut().write(a >= b);
  Ok(())
}