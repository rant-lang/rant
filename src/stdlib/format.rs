use super::*;
use crate::format::*;

/// `[$whitespace-fmt: mode? (string); custom-value? (any)]`
///
/// Gets or sets the whitespace normalization mode for the current scope.
pub(crate) fn whitespace_fmt(vm: &mut VM, (mode, custom): (Option<String>, Option<RantValue>)) -> RantStdResult {
  if let Some(mode) = mode.as_deref() {
    let mode = match mode {
      "default" =>    WhitespaceNormalizationMode::Default,
      "ignore-all" => WhitespaceNormalizationMode::IgnoreAll,
      "verbatim" =>   WhitespaceNormalizationMode::Verbatim,
      "custom" =>     WhitespaceNormalizationMode::Custom(custom.unwrap_or(RantValue::Empty)),
      bad_mode => runtime_error!(RuntimeErrorType::ArgumentError, "invalid whitespace normalization mode: '{}'", bad_mode),
    };
    vm.parent_frame_mut(1).unwrap().use_output_mut(move |output| output.format_mut().ws_norm_mode = mode);
  } else {
    let mode = vm.cur_frame().use_output(|output| output.format().ws_norm_mode.clone()).unwrap_or_default();
    let frame = vm.cur_frame_mut();
    match mode {
      WhitespaceNormalizationMode::Custom(custom_val) => {
        frame.write_value(custom_val);
      },
      other => frame.write_frag(match other {
        WhitespaceNormalizationMode::Default =>   "default",
        WhitespaceNormalizationMode::IgnoreAll => "ignore-all",
        WhitespaceNormalizationMode::Verbatim =>  "verbatim",
        WhitespaceNormalizationMode::Custom(_) => unreachable!(),
      })
    }
  }
  Ok(())
}

pub(crate) fn num_fmt_system(vm: &mut VM, system_name: Option<String>) -> RantStdResult {
  if let Some(system_name) = system_name {
    let system_name_invariant = system_name.to_ascii_lowercase();
    if let Some(s) = NumeralSystem::from_name(&system_name_invariant) {
      vm.parent_frame_mut(1).unwrap().use_output_mut(|o| o.format_mut().num_format.system = s);
    } else {
      runtime_error!(RuntimeErrorType::ArgumentError, "invalid numeral system: '{}'", system_name);
    }
  } else {
    let cur_system_name = vm.cur_frame().output().map_or(Default::default(), |o| o.format().num_format.system).name();
    vm.cur_frame_mut().write_frag(cur_system_name);
  }
  
  Ok(())
}

pub(crate) fn num_fmt_padding(vm: &mut VM, padding: Option<u16>) -> RantStdResult {
  if let Some(padding) = padding {
    vm.parent_frame_mut(1).unwrap().use_output_mut(|o| o.format_mut().num_format.padding = padding);
  } else {
    let cur_padding = vm.cur_frame().output().map_or(Default::default(), |o| o.format().num_format.padding);
    vm.cur_frame_mut().write_value(RantValue::Int(cur_padding as i64));
  }
  
  Ok(())
}