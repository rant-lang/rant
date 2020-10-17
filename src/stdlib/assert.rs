use super::*;

pub(crate) fn _assert(vm: &mut VM, (condition, message): (bool, Option<String>)) -> RantStdResult {
  if !condition {
    runtime_error!(RuntimeErrorType::AssertError, "{}", message.as_deref().unwrap_or("assertion failed: condition was false"));
  }
  Ok(())
}

pub(crate) fn _assert_eq(vm: &mut VM, (expected, actual, message): (RantValue, RantValue, Option<String>)) -> RantStdResult {
  if expected != actual {
    runtime_error!(RuntimeErrorType::AssertError, "{}", message.unwrap_or_else(|| format!("expected: {:?}; actual: {:?}", expected, actual)));
  }
  Ok(())
}

pub(crate) fn _assert_neq(vm: &mut VM, (unexpected, actual, message): (RantValue, RantValue, Option<String>)) -> RantStdResult {
  if unexpected == actual {
    runtime_error!(RuntimeErrorType::AssertError, "{}", message.unwrap_or_else(|| format!("unexpected value: {:?}", unexpected)));
  }
  Ok(())
}