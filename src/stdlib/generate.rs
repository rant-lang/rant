use super::*;

pub(crate) fn rand(vm: &mut VM, (a, b): (i64, i64)) -> RantStdResult {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Int(n));
  Ok(())
}

pub(crate) fn randf(vm: &mut VM, (a, b): (f64, f64)) -> RantStdResult {
  let n = vm.rng().next_f64(a, b);
  vm.cur_frame_mut().write_value(RantValue::Float(n));
  Ok(())
}

pub(crate) fn rand_list(vm: &mut VM, (a, b, n): (i64, i64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Int(rng.next_i64(a, b)));
  }
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  Ok(())
}

pub(crate) fn randf_list(vm: &mut VM, (a, b, n): (f64, f64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Float(rng.next_f64(a, b)));
  }
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  Ok(())
}

pub(crate) fn alpha(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyz";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

pub(crate) fn digh(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"0123456789abcdef";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

pub(crate) fn dig(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"0123456789";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

pub(crate) fn dignz(vm: &mut VM, count: Option<usize>) -> RantStdResult {
  const CHARS: &[u8] = b"123456789";
  let count = count.unwrap_or(1);
  let mut s = String::with_capacity(count);
  let rng = vm.rng();
  for _ in 0..count {
    let ch = CHARS[rng.next_usize(CHARS.len())] as char;
    s.push(ch);
  }
  vm.cur_frame_mut().write_frag(s.as_str());
  Ok(())
}

pub(crate) fn shred(vm: &mut VM, (value, n, variance): (RantValue, i64, Option<f64>)) -> RantStdResult {
  if n <= 0 {
    return Err(RuntimeError {
      error_type: RuntimeErrorType::ArgumentError,
      description: Some("shred count must be greater than zero".to_owned()),
      stack_trace: None,
    })
  }

  let rng = vm.rng();
  
  match value {
    RantValue::Int(m) => {
      let mut shreds = vec![];
      let variance = variance.unwrap_or_default().abs() as i64;
      let quotient = m / n;
      let remainder = m % n;

      let (head_chunk, tail_chunk) = (quotient + remainder, quotient);

      // Populate chunks
      for i in 0..n {
        shreds.push(if i == 0 { head_chunk } else { tail_chunk });
      }

      // Redistribute chunk size randomly
      for i in 0..n {
        let shift = rng.next_i64(0, variance + 1);
        let cell = shreds.get_mut(i as usize).unwrap();
        *cell -= shift;
        let cell = shreds.get_mut(((i + 1) % n) as usize).unwrap();
        *cell += shift;
      }

      vm.cur_frame_mut().write_value(shreds.into_rant().into_runtime_result()?);
    },
    RantValue::Float(m) => {
      let mut shreds = vec![];
      let variance = variance.unwrap_or_default().abs() as f64;
      let nf = n as f64;
      let quotient = m / nf;
      let remainder = m % nf;

      let (head_chunk, tail_chunk) = (quotient + remainder, quotient);

      // Populate chunks
      for i in 0..n {
        shreds.push(if i == 0 { head_chunk } else { tail_chunk });
      }

      // Redistribute chunk size randomly
      for i in 0..n {
        let shift = rng.next_f64(0.0, variance);
        let cell = shreds.get_mut(i as usize).unwrap();
        *cell -= shift;
        let cell = shreds.get_mut(((i + 1) % n) as usize).unwrap();
        *cell += shift;
      }

      vm.cur_frame_mut().write_value(shreds.into_rant().into_runtime_result()?);
    },
    other => {
      return Err(RuntimeError {
        error_type: RuntimeErrorType::ArgumentError,
        description: Some(format!("cannot shred '{}' value", other.type_name())),
        stack_trace: None,
      })
    }
  }

  Ok(())
}

pub(crate) fn maybe(vm: &mut VM, p: Option<f64>) -> RantStdResult {
  let b = vm.rng().next_bool(p.unwrap_or(0.5));
  vm.cur_frame_mut().write_value(RantValue::Boolean(b));
  Ok(())
}