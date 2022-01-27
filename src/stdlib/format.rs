use super::*;
use crate::format::*;

/// `[$ws-fmt: mode? (string); custom-value? (any)]`
///
/// Gets or sets the whitespace normalization mode for the current scope.
pub(crate) fn ws_fmt(vm: &mut VM, (mode, custom): (Option<String>, Option<RantValue>)) -> RantStdResult {
  if let Some(mode) = mode.as_deref() {
    let mode = match mode {
      "default" =>    WhitespaceNormalizationMode::Default,
      "ignore-all" => WhitespaceNormalizationMode::IgnoreAll,
      "verbatim" =>   WhitespaceNormalizationMode::Verbatim,
      "custom" =>     WhitespaceNormalizationMode::Custom(custom.unwrap_or(RantValue::Empty)),
      bad_mode => runtime_error!(RuntimeErrorType::ArgumentError, "invalid whitespace normalization mode: '{}'", bad_mode),
    };
    vm.parent_frame_mut(1).unwrap().output_mut().format_mut().ws_norm_mode = mode;
  } else {
    let mode = vm.cur_frame().output().format().ws_norm_mode.clone();
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

pub(crate) fn num_fmt(vm: &mut VM, (options, depth): (Option<RantMapHandle>, Option<usize>)) -> RantStdResult {
  const KEY_SYSTEM: &str = "system";
  const KEY_ALT: &str = "alt";
  const KEY_PRECISION: &str = "precision";
  const KEY_PADDING: &str = "padding";
  const KEY_UPPER: &str = "upper";
  const KEY_ENDIAN: &str = "endian";
  const KEY_SIGN: &str = "sign";
  const KEY_INFINITY: &str = "infinity";
  const KEY_GROUP_SEP: &str = "group-sep";
  const KEY_DECIMAL_SEP: &str = "decimal-sep";

  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(options) = options {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let options = options.borrow();
      if options.is_empty() { return Ok(()) }

      let format = &mut frame.output_mut().format_mut().num_format;

      for (k, v) in options.raw_pairs_internal() {
        let v = v.clone();
        let mut key_invariant = InternalString::from(k);
        key_invariant.make_ascii_lowercase();

        match key_invariant.as_str() {
          KEY_SYSTEM => {
            let system = NumeralSystem::try_from_rant(v).into_runtime_result()?;
            format.system = system;
          },
          KEY_ALT => {
            let alt = bool::try_from_rant(v).into_runtime_result()?;
            format.alternate = alt;
          },
          KEY_PRECISION => {
            let precision_encoded = i16::try_from_rant(v).into_runtime_result()?;
            let precision = (precision_encoded >= 0).then(|| precision_encoded as u16);
            format.precision = precision;
          },
          KEY_PADDING => {
            let padding = u16::try_from_rant(v).into_runtime_result()?;
            format.padding = padding;
          },
          KEY_UPPER => {
            let upper = bool::try_from_rant(v).into_runtime_result()?;
            format.uppercase = upper;
          },
          KEY_ENDIAN => {
            let endian = Endianness::try_from_rant(v).into_runtime_result()?;
            format.endianness = endian;
          },
          KEY_SIGN => {
            let sign = SignStyle::try_from_rant(v).into_runtime_result()?;
            format.sign = sign;
          },
          KEY_INFINITY => {
            let infinity = InfinityStyle::try_from_rant(v).into_runtime_result()?;
            format.infinity = infinity;
          },
          KEY_GROUP_SEP => {
            let group_sep_encoded = InternalString::try_from_rant(v).into_runtime_result()?;
            let group_sep = (!group_sep_encoded.is_empty()).then(|| group_sep_encoded);
            format.group_sep = group_sep;
          },
          KEY_DECIMAL_SEP => {
            let decimal_sep_encoded = InternalString::try_from_rant(v).into_runtime_result()?;
            let decimal_sep = (!decimal_sep_encoded.is_empty()).then(|| decimal_sep_encoded);
            format.decimal_sep = decimal_sep;
          },
          _ => {}
        }
      }

      frame.output_mut().update_number_format();
    }
  } else {
    let fmt = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.clone(),
      None => Default::default(),
    };

    let mut fmt_map = RantMap::new();
    
    fmt_map.raw_set(KEY_SYSTEM, fmt.system.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_ALT, fmt.alternate.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_PRECISION, fmt.precision.map(|p| p as i64).unwrap_or(-1).try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_PADDING, fmt.padding.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_UPPER, fmt.uppercase.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_ENDIAN, fmt.endianness.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_SIGN, fmt.sign.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_INFINITY, fmt.infinity.try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_GROUP_SEP, fmt.group_sep.unwrap_or_default().try_into_rant().into_runtime_result()?);
    fmt_map.raw_set(KEY_DECIMAL_SEP, fmt.decimal_sep.unwrap_or_default().try_into_rant().into_runtime_result()?);

    vm.cur_frame_mut().write_value(fmt_map.try_into_rant().into_runtime_result()?);
  }

  Ok(())
}

pub(crate) fn num_fmt_system(vm: &mut VM, (system, depth): (Option<NumeralSystem>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(system) = system {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.system = system;
      output.update_number_format();
    }
  } else {
    let cur_system = match vm.parent_frame_mut(actual_depth) {
      Some(frame) => frame.output().format().num_format.system,
      None => Default::default(),
    }.try_into_rant().into_runtime_result()?;
      
    vm.cur_frame_mut().write_value(cur_system);
  }
  
  Ok(())
}

pub(crate) fn num_fmt_alt(vm: &mut VM, (alt, depth): (Option<bool>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(alt) = alt {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.alternate = alt;
      output.update_number_format();
    }
  } else {
    let cur_alternate = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.alternate,
      None => false
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_alternate);
  }

  Ok(())
}

pub(crate) fn num_fmt_padding(vm: &mut VM, (padding, depth): (Option<u16>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(padding) = padding {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.padding = padding;
      output.update_number_format();
    }
  } else {
    let cur_padding = match vm.parent_frame(actual_depth) {
        Some(frame) => frame.output().format().num_format.padding,
        None => 0,
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_padding);
  }
  
  Ok(())
}

pub(crate) fn num_fmt_precision(vm: &mut VM, (precision, depth): (Option<i16>, Option<usize>)) -> RantStdResult {
  const DEFAULT_PRECISION: i64 = -1;
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(precision) = precision {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.precision = (precision >= 0).then(|| precision as u16);
      output.update_number_format();
    }
  } else {
    let cur_precision = match vm.parent_frame(actual_depth) {
        Some(frame) => frame.output().format().num_format.precision.map(|p| p as i64).unwrap_or(-DEFAULT_PRECISION),
        None => DEFAULT_PRECISION,
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_precision);
  }
  
  Ok(())
}

pub(crate) fn num_fmt_upper(vm: &mut VM, (upper, depth): (Option<bool>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(upper) = upper {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.uppercase = upper;
      output.update_number_format();
    }
  } else {
    let cur_upper = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.uppercase,
      None => false
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_upper);
  }

  Ok(())
}

pub(crate) fn num_fmt_endian(vm: &mut VM, (endianness, depth): (Option<Endianness>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(endianness) = endianness {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.endianness = endianness;
      output.update_number_format();
    }
  } else {
    let cur_endianness = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.endianness,
      None => Default::default()
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_endianness);
  }

  Ok(())
}

pub(crate) fn num_fmt_sign(vm: &mut VM, (sign_style, depth): (Option<SignStyle>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(sign_style) = sign_style {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.sign = sign_style;
      output.update_number_format();
    }
  } else {
    let cur_sign_style = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.sign,
      None => Default::default()
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_sign_style);
  }

  Ok(())
}

pub(crate) fn num_fmt_infinity(vm: &mut VM, (infinity_style, depth): (Option<InfinityStyle>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(infinity_style) = infinity_style {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.infinity = infinity_style;
      output.update_number_format();
    }
  } else {
    let cur_infinity_style = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.infinity,
      None => Default::default()
    }.try_into_rant().into_runtime_result()?;
    vm.cur_frame_mut().write_value(cur_infinity_style);
  }

  Ok(())
}

pub(crate) fn num_fmt_group_sep(vm: &mut VM, (group_sep, depth): (Option<InternalString>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(group_sep) = group_sep {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.group_sep = (!group_sep.is_empty()).then(|| group_sep);
      output.update_number_format();
    }
  } else {
    let cur_group_sep = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.group_sep.clone().unwrap_or_default(),
      None => Default::default()
    }.try_into_rant().into_runtime_result()?;

    vm.cur_frame_mut().write_value(cur_group_sep);
  }

  Ok(())
}

pub(crate) fn num_fmt_decimal_sep(vm: &mut VM, (decimal_sep, depth): (Option<InternalString>, Option<usize>)) -> RantStdResult {
  let actual_depth = depth.unwrap_or(0).saturating_add(1);

  if let Some(decimal_sep) = decimal_sep {
    if let Some(frame) = vm.parent_frame_mut(actual_depth) {
      let output = frame.output_mut();
      output.format_mut().num_format.decimal_sep = (!decimal_sep.is_empty()).then(|| decimal_sep);
      output.update_number_format();
    }
  } else {
    let cur_decimal_sep = match vm.parent_frame(actual_depth) {
      Some(frame) => frame.output().format().num_format.decimal_sep.clone().unwrap_or_default(),
      None => Default::default()
    }.try_into_rant().into_runtime_result()?;

    vm.cur_frame_mut().write_value(cur_decimal_sep);
  }

  Ok(())
}