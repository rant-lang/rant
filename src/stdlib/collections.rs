use std::cmp::Ordering;

use super::*;
use crate::{lang::PrintFlag};

pub(crate) fn squish(vm: &mut VM, (list, target_size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut();

  if target_size == 0 {
    runtime_error!(RuntimeErrorType::ArgumentError, "cannot squish to a target size of 0");
  }

  if list.len() <= target_size || list.len() < 2 {
    return Ok(())
  }

  let rng = vm.rng();
  while list.len() > target_size {
    let n = list.len();
    let left_index = rng.next_usize(n - 1);
    let right_index = left_index + 1;
    let left_val = list.get(left_index).unwrap().clone();
    let right_val = list.remove(right_index);
    list[left_index] = left_val + right_val;
  }

  Ok(())
}

pub(crate) fn squished(vm: &mut VM, (list, target_size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut().clone();

  if target_size == 0 {
    runtime_error!(RuntimeErrorType::ArgumentError, "cannot squish to a target size of 0");
  }

  let rng = vm.rng();
  while list.len() > target_size {
    let n = list.len();
    let left_index = rng.next_usize(n - 1);
    let right_index = (left_index + 1) % n;
    let left_val = list.get(left_index).unwrap().clone();
    let right_val = list.remove(right_index);
    list[left_index] = left_val + right_val;
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));

  Ok(())
}

pub(crate) fn filter(vm: &mut VM, (list, predicate): (RantListRef, RantFunctionRef)) -> RantStdResult {
  let list_ref = list.borrow();
  if list_ref.is_empty() {
    vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_ref.clone()))));
    return Ok(())
  }

  fn _iterate_filter(vm: &mut VM, src: RantListRef, mut dest: RantList, index: usize, predicate: RantFunctionRef) -> RuntimeResult<()> {
    let src_ref = src.borrow();

    // Check predicate result from last iteration
    if index > 0 {
      match vm.pop_val()? {
        RantValue::Boolean(passed) => {
          if passed {
            dest.push(src_ref.get(index - 1).cloned().unwrap_or_default());
          }
        },
        other => runtime_error!(RuntimeErrorType::TypeError, "filter callback expected to return 'bool' value, but returned '{}' instead", other.type_name())
      }
    }

    // Check if filtering finished
    if index >= src_ref.len() {
      vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(dest))));
      return Ok(())
    }

    let src_clone = Rc::clone(&src);
    let predicate_arg = src_ref.get(index).cloned().unwrap_or_default();
    let predicate_clone = Rc::clone(&predicate);

    // Prepare next iteration
    vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
      _iterate_filter(vm, src_clone, dest, index + 1, predicate)?;
      Ok(())
    })));

    // Prepare predicate call for current iteration
    vm.push_val(RantValue::Function(predicate_clone))?;
    vm.push_val(predicate_arg)?;
    vm.cur_frame_mut().push_intent_front(Intent::Call {
      argc: 1,
      flag: PrintFlag::None,
      override_print: true,
    });

    Ok(())
  }

  let list_clone = Rc::clone(&list);
  vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
    _iterate_filter(vm, list_clone, RantList::new(), 0, predicate)
  })));

  Ok(())
}

pub(crate) fn map(vm: &mut VM, (list, map_func): (RantListRef, RantFunctionRef)) -> RantStdResult {
  let list_ref = list.borrow();
  if list_ref.is_empty() {
    vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_ref.clone()))));
    return Ok(())
  }

  fn _iterate_map(vm: &mut VM, src: RantListRef, mut dest: RantList, index: usize, map_func: RantFunctionRef) -> RuntimeResult<()> {
    let src_ref = src.borrow();

    // Add result from last iteration to destination list
    if index > 0 {
      dest.push(vm.pop_val()?);
    }

    // Check if mapping finished
    if index >= src_ref.len() {
      vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(dest))));
      return Ok(())
    }

    let src_clone = Rc::clone(&src);
    let map_func_arg = src_ref.get(index).cloned().unwrap_or_default();
    let map_func_clone = Rc::clone(&map_func);

    // Prepare next iteration
    vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
      _iterate_map(vm, src_clone, dest, index + 1, map_func)?;
      Ok(())
    })));

    // Prepare predicate call for current iteration
    vm.push_val(RantValue::Function(map_func_clone))?;
    vm.push_val(map_func_arg)?;
    vm.cur_frame_mut().push_intent_front(Intent::Call {
      argc: 1,
      flag: PrintFlag::None,
      override_print: true,
    });

    Ok(())
  }

  let list_clone = Rc::clone(&list);
  vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
    _iterate_map(vm, list_clone, RantList::new(), 0, map_func)
  })));

  Ok(())
}

pub(crate) fn zip(vm: &mut VM, (list_a, list_b, zip_func): (RantListRef, RantListRef, RantFunctionRef)) -> RantStdResult {
  let (list_a_ref, list_b_ref) = (list_a.borrow(), list_b.borrow());
  let max_len = list_a_ref.len().max(list_b_ref.len());

  fn _iterate_zip(vm: &mut VM, src_a: RantListRef, src_b: RantListRef, mut dest: RantList, index: usize, max_len: usize, zip_func: RantFunctionRef) -> RuntimeResult<()> {
    let (src_a_ref, src_b_ref) = (src_a.borrow(), src_b.borrow());

    // Add result from last iteration to destination list
    if index > 0 {
      dest.push(vm.pop_val()?);
    }

    // Check whether zipping finished
    if index >= max_len {
      vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(dest))));
      return Ok(())
    }

    let (src_a_clone, src_b_clone) = (Rc::clone(&src_a), Rc::clone(&src_b));
    let zip_func_clone = Rc::clone(&zip_func);

    // Prepare next iteration
    vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
      _iterate_zip(vm, src_a_clone, src_b_clone, dest, index + 1, max_len, zip_func_clone)
    })));

    // Prepare predicate call for current iteration
    vm.push_val(RantValue::Function(zip_func))?;
    vm.push_val(src_b_ref.get(index).cloned().unwrap_or_default())?;
    vm.push_val(src_a_ref.get(index).cloned().unwrap_or_default())?;
    vm.cur_frame_mut().push_intent_front(Intent::Call {
      argc: 2,
      flag: PrintFlag::Hint,
      override_print: true,
    });

    Ok(())
  }

  let (list_a_clone, list_b_clone) = (Rc::clone(&list_a), Rc::clone(&list_b));
  vm.cur_frame_mut().push_intent_front(Intent::RuntimeCall(Box::new(move |vm| {
    _iterate_zip(vm, list_a_clone, list_b_clone, RantList::new(), 0, max_len, zip_func)
  })));

  Ok(())
}

pub(crate) fn pick(vm: &mut VM, list: RantValue) -> RantStdResult {
  let index = vm.rng().next_usize(list.len());
  let item = list.index_get(index as i64).into_runtime_result()?;
  vm.cur_frame_mut().write_value(item);
  Ok(())
}

pub(crate) fn join(vm: &mut VM, (sep, list): (RantValue, Vec<RantValue>)) -> RantStdResult {
  let mut is_first = true;
  let frame = vm.cur_frame_mut();
  for val in list {
    if is_first {
      is_first = false;
    } else {
      frame.write_value(sep.clone());
    }
    frame.write_value(val);
  }
  Ok(())
}

#[allow(clippy::needless_range_loop)]
pub(crate) fn oxford_join(vm: &mut VM, (comma, conj, comma_conj, list): (RantValue, RantValue, RantValue, Vec<RantValue>)) -> RantStdResult {
  let frame = vm.cur_frame_mut();
  let n = list.len();
  for i in 0..n {
    match (i, n, i == n - 1) {
      (0, ..) | (_, 1, _) => {},
      (1, 2, _) => frame.write_value(conj.clone()),
      (.., false) => frame.write_value(comma.clone()),
      (.., true) => frame.write_value(comma_conj.clone()),
    }
    frame.write_value(list[i].clone());
  }
  Ok(())
}

pub(crate) fn sum(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(())
  }

  let mut iter = list.iter().cloned();
  let mut sum = iter.next().unwrap();
  
  for val in iter {
    sum = sum + val;
  }

  vm.cur_frame_mut().write_value(sum);

  Ok(())
}

pub(crate) fn min(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(());
  }

  let mut iter = list.iter();
  let mut min = iter.next().unwrap();

  for val in iter {
    if val < min {
      min = val;
    }
  }

  vm.cur_frame_mut().write_value(min.clone());

  Ok(())
}

pub(crate) fn max(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let list = list.borrow();
  if list.is_empty() {
    return Ok(());
  }

  let mut iter = list.iter();
  let mut max = iter.next().unwrap();

  for val in iter {
    if val > max {
      max = val;
    }
  }

  vm.cur_frame_mut().write_value(max.clone());

  Ok(())
}

pub(crate) fn shuffled(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow().clone();
  if list.is_empty() {
    return Ok(());
  }

  let n = list.len();
  let rng = vm.rng();

  for i in 0..n {
    let swap_index = rng.next_usize(n);
    list.swap(i, swap_index);
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));
  
  Ok(())
}

pub(crate) fn clear(vm: &mut VM, collection: RantValue) -> RantStdResult {
  match collection {
    RantValue::List(list) => list.borrow_mut().clear(),
    RantValue::Map(map) => map.borrow_mut().clear(),
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "value passed to [clear] must be a collection type");
    }
  }
  Ok(())
}

pub(crate) fn keys(vm: &mut VM, map: RantMapRef) -> RantStdResult {
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(map.borrow().raw_keys()))));
  Ok(())
}

pub(crate) fn assoc(vm: &mut VM, (keys, values): (RantListRef, RantListRef)) -> RantStdResult {
  let keys = keys.borrow();
  let values = values.borrow();
  if keys.len() != values.len() {
    runtime_error!(RuntimeErrorType::ArgumentError, "assoc: key and value counts don't match");
  }

  let mut map = RantMap::new();
  for (key, val) in keys.iter().zip(values.iter()) {
    map.raw_set(key.to_string().as_ref(), val.clone());
  }

  vm.cur_frame_mut().write_value(RantValue::Map(Rc::new(RefCell::new(map))));

  Ok(())
}

pub(crate) fn translate(vm: &mut VM, (list, map): (RantListRef, RantMapRef)) -> RantStdResult {
  let list = list.borrow();
  let map = map.borrow();

  let translated: RantList = list
    .iter()
    .map(|val| map.raw_get(val.to_string().as_ref()).cloned().unwrap_or_else(|| val.clone()))
    .collect();

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(translated))));

  Ok(())
}

pub(crate) fn list_push(vm: &mut VM, (list, value): (RantListRef, RantValue)) -> RantStdResult {
  list.borrow_mut().push(value);
  Ok(())
}

pub(crate) fn list_pop(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let value = list.borrow_mut().pop().unwrap_or(RantValue::Empty);
  vm.cur_frame_mut().write_value(value);
  Ok(())
}

pub(crate) fn sift(vm: &mut VM, (list, size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow_mut();
  if list.len() <= size {
    return Ok(())
  }

  let rng = vm.rng();
  while list.len() > size {
    let remove_index = rng.next_usize(list.len());
    list.remove(remove_index);
  }

  Ok(())
}

pub(crate) fn sifted(vm: &mut VM, (list, size): (RantListRef, usize)) -> RantStdResult {
  let mut list = list.borrow().clone();

  let rng = vm.rng();
  while list.len() > size {
    let remove_index = rng.next_usize(list.len());
    list.remove(remove_index);
  }

  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list))));

  Ok(())
}

pub(crate) fn insert(vm: &mut VM, (collection, value, pos): (RantValue, RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Insert into list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize > list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.insert(index, value);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot insert into list by '{}' index", non_index.type_name());
    },
    // Insert into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-set function
      map.raw_set(key.as_str(), value);
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot insert into a non-collection");
    }
  }
  Ok(())
}

pub(crate) fn index_of(vm: &mut VM, (list, value): (RantListRef, RantValue)) -> RantStdResult {
  let index = list
    .borrow()
    .iter()
    .position(|v| v == &value)
    .map(|i| RantValue::Integer(i as i64))
    .unwrap_or(RantValue::Empty);

    vm.cur_frame_mut().write_value(index);
    Ok(())
}

pub(crate) fn last_index_of(vm: &mut VM, (list, value): (RantListRef, RantValue)) -> RantStdResult {
  let index = list
    .borrow()
    .iter()
    .rposition(|v| v == &value)
    .map(|i| RantValue::Integer(i as i64))
    .unwrap_or(RantValue::Empty);

    vm.cur_frame_mut().write_value(index);
    Ok(())
}

pub(crate) fn remove(vm: &mut VM, (collection, pos): (RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Remove from list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize >= list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.remove(index);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot remove from list by '{}' index", non_index.type_name());
    },
    // Remove from into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-remove function
      map.raw_remove(key.as_str());
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot remove from a non-collection");
    }
  }
  Ok(())
}

pub(crate) fn take(vm: &mut VM, (collection, pos): (RantValue, RantValue)) -> RantStdResult {
  match (collection, pos) {
    // Take from list by index
    (RantValue::List(list), RantValue::Integer(index)) => {
      let mut list = list.borrow_mut();
      // Bounds check
      if index < 0 || index as usize >= list.len() {
        runtime_error!(RuntimeErrorType::IndexError(IndexError::OutOfRange), "index is out of range of list size");
      }
      let index = index as usize;
      list.remove(index);
    },
    // Error on non-index list access
    (RantValue::List(_), non_index) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot take from list by '{}' index", non_index.type_name());
    },
    // Remove from into map by key
    (RantValue::Map(map), key_val) => {
      let mut map = map.borrow_mut();
      let key = key_val.to_string();
      // TODO: Replace with prototype key-remove function
      if let Some(val) = map.raw_take(key.as_str()) {
        vm.cur_frame_mut().write_value(val);
      } else {
        runtime_error!(RuntimeErrorType::KeyError(KeyError::KeyNotFound(key.to_owned())), "tried to take non-existent key: '{}'", key);
      }
    },
    _ => {
      runtime_error!(RuntimeErrorType::ArgumentError, "cannot take from a non-collection");
    }
  }
  Ok(())
}

pub(crate) fn sort(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow_mut();
  list.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
  Ok(())
}

pub(crate) fn sorted(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list_copy = list.borrow().clone();
  list_copy.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));
  vm.cur_frame_mut().write_value(RantValue::List(Rc::new(RefCell::new(list_copy))));
  Ok(())
}

pub(crate) fn shuffle(vm: &mut VM, list: RantListRef) -> RantStdResult {
  let mut list = list.borrow_mut();
  if list.is_empty() {
    return Ok(())
  }

  let n = list.len();
  let rng = vm.rng();
  for i in 0..n {
    list.swap(i, rng.next_usize(n));
  }
  Ok(())
}

pub(crate) fn has(vm: &mut VM, (value, key): (RantValue, RantValue)) -> RantStdResult {
  let result = match (value, key) {
    (RantValue::Map(map), RantValue::String(key)) => {
      map.borrow().raw_has_key(key.as_str())
    },
    (RantValue::List(list), element) => {
      list.borrow().contains(&element)
    },
    (value, key) => {
      runtime_error!(RuntimeErrorType::ArgumentError, "unable to check if value of type '{}' contains element of type '{}'", value.type_name(), key.type_name())
    }
  };

  vm.cur_frame_mut().write_value(RantValue::Boolean(result));
  Ok(())
}