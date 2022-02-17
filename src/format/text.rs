use crate::{TryFromRant, RantValue, ValueError, RantString, InternalString};

#[derive(Debug, Clone, Default)]
pub struct CasingFormat {
  locale: CasingLocale,
  style: CasingStyle,
}

impl CasingFormat {
  
}

/// Provides locale disambiguation for casing operations.
#[derive(Debug, Copy, Clone)]
pub enum CasingLocale {
  /// Follow the default casing rules defined by Unicode.
  Invariant,
  /// Follow casing rules specific to the [Common Turkic Alphabet](https://en.wikipedia.org/wiki/Common_Turkic_Alphabet).
  Turkic,
}

impl CasingLocale {
  #[inline]
  pub fn convert_upper(&self, input: &RantString) -> RantString {
    transform_str(input.as_str(), match self {
      CasingLocale::Invariant => convert_char_upper_invariant,
      CasingLocale::Turkic => convert_char_upper_turkic,
    }).into()
  }

  #[inline]
  pub fn convert_lower(&self, input: &RantString) -> RantString {
    transform_str(input.as_str(), match self {
      CasingLocale::Invariant => convert_char_lower_invariant,
      CasingLocale::Turkic => convert_char_lower_turkic,
    }).into()
  }
}

impl TryFromRant for CasingLocale {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    Ok(match val {
      RantValue::String(s) => match s.as_str() {
        "invariant" => Self::Invariant,
        "turkic" => Self::Turkic,
        other => return Err(ValueError::InvalidConversion {
          from: "string",
          to: "casing locale",
          message: Some(format!("unrecognized casing locale: '{other}'")),
        })
      },
      other => return Err(ValueError::InvalidConversion {
        from: other.type_name(),
        to: "casing locale",
        message: None,
      })
    })
  }
}

impl Default for CasingLocale {
  fn default() -> Self {
    Self::Invariant
  }
}

#[derive(Debug, Copy, Clone)]
pub enum CasingStyle {
  /// Don't change the casing at all.
  Normal,
  /// Convert to uppercase.
  Upper,
  /// Convert to lowercase.
  Lower,
  /// Capitalize the next letter.
  FirstLetter,
  /// Capitalize the first letter of every sentence.
  Sentence,
}

impl Default for CasingStyle {
  fn default() -> Self {
    Self::Normal
  }
}

impl TryFromRant for CasingStyle {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    Ok(match val {
      RantValue::String(s) => match s.as_str() {
        "normal" => Self::Normal,
        "upper" => Self::Upper,
        "lower" => Self::Lower,
        "first-letter" => Self::FirstLetter,
        "sentence" => Self::Sentence,
        other => return Err(ValueError::InvalidConversion {
          from: "string",
          to: "casing style",
          message: Some(format!("unrecognized casing style: '{other}'")),
        })
      },
      other => return Err(ValueError::InvalidConversion {
        from: other.type_name(),
        to: "casing style",
        message: None,
      })
    })
  }
}

fn transform_str<F>(input: &str, transformer: F) -> InternalString
  where F: Fn(char, &mut InternalString) 
{
  let mut output = InternalString::new();
  for c in input.chars() {
    transformer(c, &mut output);
  }
  output
}

#[inline]
fn convert_char_upper_invariant(input: char, buffer: &mut InternalString) {
  match input {
    'ß' => buffer.push('ẞ'),
    other => buffer.push_str(&other.to_uppercase().to_string())
  }
}

#[inline]
fn convert_char_lower_invariant(input: char, buffer: &mut InternalString) {
  match input {
    'ß' => buffer.push('ẞ'),
    other => buffer.push_str(&other.to_uppercase().to_string())
  }
}

#[inline]
fn convert_char_upper_turkic(input: char, buffer: &mut InternalString) {
  match input {
    'i' => buffer.push('İ'),
    other => convert_char_upper_invariant(other, buffer)
  }
}

#[inline]
fn convert_char_lower_turkic(input: char, buffer: &mut InternalString) {
  match input {
    'I' => buffer.push('ı'),
    other => convert_char_lower_invariant(other, buffer)
  }
}