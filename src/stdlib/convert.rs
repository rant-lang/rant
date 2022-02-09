use super::*;

pub fn to_int(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.into_int_value());
  Ok(())
}

pub fn to_float(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.into_float_value());
  Ok(())
}

pub fn to_string(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.into_string_value());
  Ok(())
}

pub fn to_bool(vm: &mut VM, value: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(value.into_bool_value());
  Ok(())
}

pub fn to_list(vm: &mut VM, collection: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(collection.into_list_value());
  Ok(())
}

pub fn to_tuple(vm: &mut VM, collection: RantValue) -> RantStdResult {
  vm.cur_frame_mut().write(collection.into_tuple_value());
  Ok(())
}