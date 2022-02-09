use super::*;

pub fn proto(vm: &mut VM, map: RantMapHandle) -> RantStdResult {
  vm.cur_frame_mut().write(map.borrow().proto().map_or(RantValue::Nothing, RantValue::Map));
  Ok(())
}

pub fn set_proto(vm: &mut VM, (map, proto): (RantMapHandle, Option<RantMapHandle>)) -> RantStdResult {
  map.borrow_mut().set_proto(proto);
  Ok(())
}