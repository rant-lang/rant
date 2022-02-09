use super::*;

pub fn split(vm: &mut VM, (s, at): (String, Option<String>)) -> RantStdResult {
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

  vm.cur_frame_mut().write(list);
  Ok(())
}

pub fn lines(vm: &mut VM, s: String) -> RantStdResult {
  let lines: Vec<RantValue> = s.lines().map(|line| RantValue::String(line.into())).collect();
  vm.cur_frame_mut().write(lines);
  Ok(())
}

pub fn indent(vm:  &mut VM, (text, indent): (String, String)) -> RantStdResult {
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

pub fn upper(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_uppercase().as_str());
  Ok(())
}

pub fn lower(vm: &mut VM, s: String) -> RantStdResult {
  vm.cur_frame_mut().write_frag(s.to_lowercase().as_str());
  Ok(())
}

pub fn string_replace(vm: &mut VM, (input, query, replacement): (RantString, RantString, RantString)) -> RantStdResult {
  vm.cur_frame_mut().write_frag(input.as_str().replace(query.as_str(), replacement.as_str()).as_str());
  Ok(())
}