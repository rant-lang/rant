use super::*;

pub(crate) fn proto(vm: &mut VM, map: RantMapRef) -> RantStdResult {
  vm.cur_frame_mut().write_value(map.borrow().proto().map_or(RantValue::Empty, RantValue::Map));
  Ok(())
}

pub(crate) fn set_proto(vm: &mut VM, (map, proto): (RantMapRef, Option<RantMapRef>)) -> RantStdResult {
  map.borrow_mut().set_proto(proto);
  Ok(())
}