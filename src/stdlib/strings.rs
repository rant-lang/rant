use super::*;

pub(crate) fn seg(vm: &mut VM, (s, seg_size): (String, usize)) -> RantStdResult {
  if seg_size > 0 {
    let mut segs = vec![];
    let len = s.len();
    let last_seg_len = len % seg_size;
    let n = len / seg_size + (last_seg_len % 2);
    if last_seg_len > 0 {
      for i in 0..n {
        if i == n - 1 {
          segs.push(s[i * seg_size .. i * seg_size + last_seg_len].to_owned());
        } else {
          segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
        }
      }
    } else {
      for i in 0..n {
        segs.push(s[i * seg_size .. (i + 1) * seg_size].to_owned());
      }
    }
    vm.cur_frame_mut().write_value(segs.to_rant().into_runtime_result()?);
  }
  Ok(())
}

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
  }.to_rant().into_runtime_result()?;

  vm.cur_frame_mut().write_value(list);
  Ok(())
}

pub(crate) fn lines(vm: &mut VM, s: String) -> RantStdResult {
  let lines = s.lines().map(|line| RantValue::String(line.into())).collect();
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(lines))));
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