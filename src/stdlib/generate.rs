use super::*;

pub fn rand(vm: &mut VM, (a, b): (i64, i64)) -> RantStdResult {
  let n = vm.rng().next_i64(a, b);
  vm.cur_frame_mut().write(n);
  Ok(())
}

pub fn randf(vm: &mut VM, (a, b): (f64, f64)) -> RantStdResult {
  let n = vm.rng().next_f64(a, b);
  vm.cur_frame_mut().write(n);
  Ok(())
}

pub fn rand_list(vm: &mut VM, (a, b, n): (i64, i64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Int(rng.next_i64(a, b)));
  }
  vm.cur_frame_mut().write(list);
  Ok(())
}

pub fn randf_list(vm: &mut VM, (a, b, n): (f64, f64, usize)) -> RantStdResult {
  let mut list = RantList::new();
  let rng = vm.rng();
  for _ in 0..n {
    list.push(RantValue::Float(rng.next_f64(a, b)));
  }
  vm.cur_frame_mut().write(list);
  Ok(())
}

pub fn alpha(vm: &mut VM, count: Option<usize>) -> RantStdResult {
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

pub fn digh(vm: &mut VM, count: Option<usize>) -> RantStdResult {
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

pub fn dig(vm: &mut VM, count: Option<usize>) -> RantStdResult {
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

pub fn dignz(vm: &mut VM, count: Option<usize>) -> RantStdResult {
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

pub fn rand_list_sum(vm: &mut VM, (value, n, variance): (RantValue, i64, Option<f64>)) -> RantStdResult {
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

      vm.cur_frame_mut().write(shreds);
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

      vm.cur_frame_mut().write(shreds);
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

pub fn maybe(vm: &mut VM, p: Option<f64>) -> RantStdResult {
  let b = vm.rng().next_bool(p.unwrap_or(0.5));
  vm.cur_frame_mut().write(b);
  Ok(())
}

pub fn pick(vm: &mut VM, list: RantValue) -> RantStdResult {
  let n = list.len();
  if n > 0 {
    let index = vm.rng().next_usize(n);
    let item = list.index_get(index as i64).into_runtime_result()?;
    vm.cur_frame_mut().write(item);
  }
  Ok(())
}

pub fn pick_sparse(vm: &mut VM, mut items: RequiredVarArgs<RantValue>) -> RantStdResult {
  let len_sum = items
    .iter()
    .map(|v| v.len())
    .fold(0usize, |acc, x| acc.saturating_add(x));

  let mut rem_sum = vm.rng().next_usize(len_sum);

  for item in items.drain(..) {
    let item_len = item.len();
    if rem_sum < item_len {
      vm.cur_frame_mut().write(if item.is_indexable() { 
        item.index_get(rem_sum.try_into().unwrap_or(i64::MAX)).into_runtime_result()?
      } else {
        item 
      });
      break
    }
    rem_sum = rem_sum.saturating_sub(item_len);
  }

  Ok(())
}