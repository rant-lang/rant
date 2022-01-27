use super::*;

pub(crate) fn split(vm: &mut VM, (s, at): (String, Option<String>)) -> RantStdResult {
  let list = if at.as_ref().map(|s| s.is_empty()).unwrap_or(true) {
    s.chars()
      .map(|c| c.to_string())
      .collect::<Vec<String>>()
  } else {
    s
      .split(at.unwrap().as_str())
      .map(|part| part.to_owned())
      .collect::<Vec<String>>()
  }.try_into_rant().into_runtime_result()?;

  vm.cur_frame_mut().write_value(list);
  Ok(())
}

pub(crate) fn lines(vm: &mut VM, s: String) -> RantStdResult {
  let lines: Vec<RantValue> = s.lines().map(|line| RantValue::String(line.into())).collect();
  vm.cur_frame_mut().write_value(RantValue::List(RantList::from(lines).into_handle()));
  Ok(())
}

pub(crate) fn indent(vm:  &mut VM, (text, indent): (String, String)) -> RantStdResult {
  let frame = vm.cur_frame_mut();
  let mut first = true;
  for line in text.lines() {
    if first {
      first = false;
    } else {
      frame.write_frag("\n");
    }
    frame.write_frag(indent.as_str());
    frame.write_frag(line);
  }
  Ok(())
}

pub(crate) fn upper(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_uppercase().as_str());
  Ok(())
}

pub(crate) fn lower(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_lowercase().as_str());
  Ok(())
}