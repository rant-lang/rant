use std::cmp::Ordering;

use crate::{TryFromRant, InternalString, TryIntoRant, RantValue, ValueError};

const DEFAULT_SIGN_POSITIVE: &str = "+";
const DEFAULT_SIGN_NEGATIVE: &str = "-";
const DEFAULT_NAN: &str = "NaN";
const DEFAULT_INFINITY_KW: &str = "infinity";
const DEFAULT_NEG_INFINITY_KW: &str = "-infinity";
const DEFAULT_INFINITY_SYMBOL: &str = "\u{221e}";
const DEFAULT_NEG_INFINITY_SYMBOL: &str = "-\u{221e}";

const DIGITS_DEFAULT: &[char] = &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
const DIGITS_ARABIC_E: &[char] = &['\u{0660}', '\u{0661}', '\u{0662}', '\u{0663}', '\u{0664}', '\u{0665}', '\u{0666}', '\u{0667}', '\u{0668}', '\u{0669}'];
const DIGITS_PERSIAN: &[char] = &['\u{06F0}', '\u{06F1}', '\u{06F2}', '\u{06F3}', '\u{06F4}', '\u{06F5}', '\u{06F6}', '\u{06F7}', '\u{06F8}', '\u{06F9}'];

const PREFIX_HEXADECIMAL: &str = "0x";
const PREFIX_OCTAL: &str = "0o";
const PREFIX_BINARY: &str = "0b";

const ROMAN_ZERO: &str = "n";
const ROMAN_THOUSAND: &str = "m";
const ROMAN_ONES: &[&str] = &["", "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix"];
const ROMAN_TENS: &[&str] = &["", "x", "xx", "xxx", "xl", "l", "lx", "lxx", "lxxx", "lc"];
const ROMAN_HUNDREDS: &[&str] = &["", "c", "cc", "ccc", "cd", "c", "cd", "dcc", "dccc", "cm"];

const BABYLONIAN_ZERO: &str = "\u{2423}";
const BABYLONIAN_ONES: &[&str] = &["", "\u{12415}", "\u{12416}", "\u{12417}", "\u{12418}", "\u{12419}", "\u{1241a}", "\u{1241b}", "\u{1241c}", "\u{1241d}"];
const BABYLONIAN_TENS: &[&str] = &["", "\u{1230b}", "\u{1230b}\u{1230b}", "\u{1230d}", "\u{1240f}", "\u{12410}"];
const BABYLONIAN_TENS_ALT: &[&str] = &["", "\u{1230b}", "\u{1230b}\u{1230b}", "\u{1230d}", "\u{12469}", "\u{1246a}"];

/// Specifies a format for converting a number to a string.
#[derive(Debug, Clone, Default)]
pub struct NumberFormat {
  /// The numeral system to use.
  pub system: NumeralSystem,
  /// Enables the alternative display mode for the selected system, if available.
  ///
  /// For systems such as `Binary`, `Hex`, and `Octal`, this will append their associated prefix (e.g. `0x` for hex).
  pub alternate: bool,
  /// Enables uppercase number formatting on certain systems, such as `Hex` and `Roman`.
  pub uppercase: bool,
  /// Byte ordering for binary, octal, and hex formats.
  pub endianness: Endianness,
  /// Sets how infinite floating-point values are formatted.
  pub infinity: InfinityStyle,
  /// The sign style to use.
  pub sign: SignStyle,
  /// The maximum number of leading zeros to add to formatted numbers.
  pub padding: u16,
  /// The decimal precision of formatted numbers.
  pub precision: Option<u16>,
  /// The digit group separator.
  pub group_sep: Option<InternalString>,
  /// The decimal separator to use.
  pub decimal_sep: Option<InternalString>,
}

/// Defines sign display modes for formatted numbers.
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum SignStyle {
  /// Show a minus (-) for negative numbers and nothing for zero or positive numbers.
  NegativeOnly,
  /// Show a minus (-) for negative numbers and a plus (+) for zero or positive numbers.
  Explicit,
  /// Show a minus (-) for negative numbers, nothing for zero, and a plus (+) for positive numbers.
  ExplicitNonZero,
}

impl SignStyle {
  /// Alias for the `NegativeOnly` variant used by Rant.
  pub const ALIAS_NEGATIVE_ONLY: &'static str = "negative-only";
  /// Alias for the `Explicit` variant used by Rant.
  pub const ALIAS_EXPLICIT: &'static str = "explicit";
  /// Alias for the `ExplicitNonZero` variant used by Rant.
  pub const ALIAS_EXPLICIT_NON_ZERO: &'static str = "explicit-non-zero";
}

impl Default for SignStyle {
  fn default() -> Self {
    Self::NegativeOnly
  }
}

impl TryFromRant for SignStyle {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    let mut s = val.to_string();
    s.make_ascii_lowercase();
    Ok(match s.as_str() {
      Self::ALIAS_NEGATIVE_ONLY | "default" => Self::NegativeOnly,
      Self::ALIAS_EXPLICIT => Self::Explicit,
      Self::ALIAS_EXPLICIT_NON_ZERO => Self::ExplicitNonZero,
      _ => return Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "sign style",
        message: None,
      })
    })
  }

  fn is_optional_param_type() -> bool {
    false
  }
}

impl TryIntoRant for SignStyle {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    match self {
      Self::Explicit => Self::ALIAS_EXPLICIT,
      Self::ExplicitNonZero => Self::ALIAS_EXPLICIT_NON_ZERO,
      Self::NegativeOnly => Self::ALIAS_NEGATIVE_ONLY,
    }.try_into_rant()
  }
}

/// Defines infinity handling modes for formatted floating-point numbers.
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum InfinityStyle {
  /// Show `infinity` for positive infinity and `-infinity` for negative infinity.
  Keyword,
  /// Show `∞` for positive infinity and `-∞` for negative infinity.
  Symbol,
}

impl InfinityStyle {
  /// Alias for the `Keyword` variant used by Rant.
  pub const ALIAS_KEYWORD: &'static str = "keyword";
  /// Alias for the `Symbol` variant used by Rant.
  pub const ALIAS_SYMBOL: &'static str = "symbol";
}

impl Default for InfinityStyle {
  fn default() -> Self {
    Self::Keyword
  }
}

impl TryFromRant for InfinityStyle {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    let mut s = val.to_string();
    s.make_ascii_lowercase();
    Ok(match s.as_str() {
      Self::ALIAS_KEYWORD | "default" => Self::Keyword,
      Self::ALIAS_SYMBOL => Self::Symbol,
      _ => return Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "infinity style",
        message: None,
      })
    })
  }

  fn is_optional_param_type() -> bool {
    false
  }
}

impl TryIntoRant for InfinityStyle {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    match self {
      Self::Keyword => Self::ALIAS_KEYWORD,
      Self::Symbol => Self::ALIAS_SYMBOL,
    }.try_into_rant()
  }
}

/// Defines numeral systems for formatted numbers.
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum NumeralSystem {
  /// Westerm Arabic numerals in base 10
  WestArabic,
  /// Eastern Arabic numerals in base 10
  EastArabic,
  /// Persian numerals in base 10
  Persian,
  /// Roman numerals (truncates decimals)
  Roman,
  /// Babylonian cuneiform numerals (base 60, truncates decimals)
  Babylonian,
  /// Hexadecimal (base 16)
  Hex,
  /// Octal (base 8)
  Octal,
  /// Binary (base 2)
  Binary,
  /// Latin alphabetical numerals (a, b, c, ...)
  Alpha,
}

impl NumeralSystem {
  /// Alias for the `WestArabic` variant used by Rant.
  pub const ALIAS_WEST_ARABIC: &'static str = "west-arabic";
  /// Alias for the `EastArabic` variant used by Rant.
  pub const ALIAS_EAST_ARABIC: &'static str = "east-arabic";
  /// Alias for the `Persian` variant used by Rant.
  pub const ALIAS_PERSIAN: &'static str = "persian";
  /// Alias for the `Roman` variant used by Rant.
  pub const ALIAS_ROMAN: &'static str = "roman";
  /// Alias for the `Babylonian` variant used by Rant.
  pub const ALIAS_BABYLONIAN: &'static str = "babylonian";
  /// Alias for the `Hex` variant used by Rant.
  pub const ALIAS_HEX: &'static str = "hex";
  /// Alias for the `Octal` variant used by Rant.
  pub const ALIAS_OCTAL: &'static str = "octal";
  /// Alias for the `Binary` variant used by Rant.
  pub const ALIAS_BINARY: &'static str = "binary";
  /// Alias for the `Alpha` variant used by Rant.
  pub const ALIAS_ALPHA: &'static str = "alpha";
}

impl Default for NumeralSystem {
  fn default() -> Self {
    Self::WestArabic
  }
}

impl TryFromRant for NumeralSystem {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    let mut s = val.to_string();
    s.make_ascii_lowercase();
    Ok(match s.as_str() {
      Self::ALIAS_WEST_ARABIC | "default" => Self::WestArabic,
      Self::ALIAS_EAST_ARABIC => Self::EastArabic,
      Self::ALIAS_PERSIAN => Self::Persian,
      Self::ALIAS_ROMAN => Self::Roman,
      Self::ALIAS_BABYLONIAN => Self::Babylonian,
      Self::ALIAS_HEX => Self::Hex,
      Self::ALIAS_OCTAL => Self::Octal,
      Self::ALIAS_BINARY => Self::Binary,
      Self::ALIAS_ALPHA => Self::Alpha,
      _ => return Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "numeral system",
        message: None,
      })
    })
  }

  fn is_optional_param_type() -> bool {
    false
  }
}

impl TryIntoRant for NumeralSystem {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    match self {
      Self::WestArabic => Self::ALIAS_WEST_ARABIC,
      Self::EastArabic => Self::ALIAS_EAST_ARABIC,
      Self::Persian => Self::ALIAS_PERSIAN,
      Self::Roman => Self::ALIAS_ROMAN,
      Self::Babylonian => Self::ALIAS_BABYLONIAN,
      Self::Hex => Self::ALIAS_HEX,
      Self::Octal => Self::ALIAS_OCTAL,
      Self::Binary => Self::ALIAS_BINARY,
      Self::Alpha => Self::ALIAS_ALPHA,
    }.try_into_rant()
  }
}

impl NumeralSystem {
  #[inline]
  fn get_decimal_digit(&self, digit_index: usize) -> Option<char> {
    if digit_index > 9 { return None }
    Some(match self {
      Self::WestArabic => DIGITS_DEFAULT[digit_index],
      Self::EastArabic => DIGITS_ARABIC_E[digit_index],
      Self::Persian => DIGITS_PERSIAN[digit_index],
      _ => return None
    })
  }

  #[inline]
  fn transliterate_decimal_digit(&self, digit_char: char) -> Option<char> {
    let offset: usize = match digit_char {
      '0' => 0,
      '1' => 1,
      '2' => 2,
      '3' => 3,
      '4' => 4,
      '5' => 5,
      '6' => 6,
      '7' => 7,
      '8' => 8,
      '9' => 9,
      _ => return None
    };

    Some(match self {
      Self::WestArabic => DIGITS_DEFAULT[offset],
      Self::EastArabic => DIGITS_ARABIC_E[offset],
      Self::Persian => DIGITS_PERSIAN[offset],
      _ => return None
    })
  }
}

/// Defines byte ordering types for power-of-two radices.
#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum Endianness {
  /// Big-endian byte ordering (MSB comes last)
  Big,
  /// Little-endian byte ordering (LSB comes last)
  Little,
}

impl Default for Endianness {
  fn default() -> Self {
    Self::Big
  }
}

impl TryFromRant for Endianness {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    let mut s = val.to_string();
    s.make_ascii_lowercase();
    Ok(match s.as_str() {
      "little" => Self::Little,
      "big" => Self::Big,
      _ => return Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "endianness",
        message: None,
      }),
    })
  }

  fn is_optional_param_type() -> bool {
    false
  }
}

impl TryIntoRant for Endianness {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    match self {
      Self::Big => "big",
      Self::Little => "little",
    }.try_into_rant()
  }
}

impl NumberFormat {
  pub fn format_float<N: Into<f64>>(&self, n: N) -> InternalString {
    let n: f64 = n.into();

    macro_rules! handle_specials {
      ($expression:expr) => {{
        if n.is_nan() {
          return DEFAULT_NAN.into()
        } else if n.is_infinite() {
          let is_positive = n.is_sign_positive();
          return match self.infinity {
            InfinityStyle::Keyword => if is_positive { DEFAULT_INFINITY_KW } else { DEFAULT_NEG_INFINITY_KW }.into(),
            InfinityStyle::Symbol => if is_positive { DEFAULT_INFINITY_SYMBOL } else { DEFAULT_NEG_INFINITY_SYMBOL }.into(),
          }
        }

        $expression
      }}
    }

    use NumeralSystem::*;
    match self.system {
      WestArabic | EastArabic | Persian => handle_specials!(self.format_decimal_float(n)),
      Roman => handle_specials!(self.format_roman_float(n)),
      Babylonian => handle_specials!(self.format_babylonian_integer(n as i64)),
      Hex => self.format_bitwise_float(n, 16),
      Octal => handle_specials!(self.format_bitwise_float(n, 8)),
      Binary => self.format_bitwise_float(n, 2),
      Alpha => self.format_alpha_integer(n as i64),
    }
  }

  pub fn format_integer<N: Into<i64>>(&self, n: N) -> InternalString {
    let n: i64 = n.into();
    use NumeralSystem::*;
    match self.system {
      WestArabic | EastArabic | Persian => self.format_decimal_integer(n),
      Roman => self.format_roman_integer(n),
      Babylonian => self.format_babylonian_integer(n),
      Hex => self.format_bitwise_integer(n, 16),
      Octal => self.format_bitwise_integer(n, 8),
      Binary => self.format_bitwise_integer(n, 2),
      Alpha => self.format_alpha_integer(n),
    }
  }

  #[inline]
  fn get_float_sign(&self, n: f64) -> &'static str {
    if n.abs() < f64::EPSILON {
      match self.sign {
        SignStyle::ExplicitNonZero => DEFAULT_SIGN_POSITIVE,
        _ => "",
      }
    } else if n.is_sign_negative() {
      DEFAULT_SIGN_NEGATIVE
    } else {
      match self.sign {
        SignStyle::Explicit | SignStyle::ExplicitNonZero => DEFAULT_SIGN_POSITIVE,
        _ => "",
      }
    }
  }

  #[inline]
  fn get_integer_sign(&self, n: i64) -> &'static str {
    match n.cmp(&0) {
      Ordering::Less => DEFAULT_SIGN_NEGATIVE,
      Ordering::Equal => match self.sign {
        SignStyle::ExplicitNonZero => DEFAULT_SIGN_POSITIVE,
        _ => "",
      },
      Ordering::Greater => match self.sign {
        SignStyle::Explicit | SignStyle::ExplicitNonZero => DEFAULT_SIGN_POSITIVE,
        _ => "",
      },
    }
  }

  #[inline]
  fn get_needed_padding<N: Into<usize>>(&self, num_significant_digits: N) -> usize {
    (self.padding as usize).saturating_sub(num_significant_digits.into())
  }

  #[inline]
  fn get_radix_prefix(&self, radix: usize) -> &'static str {
    match radix {
      2 => PREFIX_BINARY,
      8 => PREFIX_OCTAL,
      16 => PREFIX_HEXADECIMAL,
      _ => "",
    }
  }

  fn format_alpha_integer(&self, input: i64) -> InternalString {
    let mut buf = InternalString::new();
    let mut n = input;
    let sign = n.signum();

    if n == 0 { return " ".into() }

    let mut digit_stack: Vec<char> = vec![];

    buf.push_str(self.get_integer_sign(input));

    while n != 0 {
      let alpha_index = (n - sign).abs() % 26;
      let digit = (b'a' + alpha_index as u8) as char;
      digit_stack.push(digit);
      n = (n - (alpha_index - 1) * sign) / 26;
    }

    for d in digit_stack.drain(..).rev() {
      buf.push(d);
    }

    if self.uppercase {
      buf.make_ascii_uppercase();
    }

    buf
  }
  
  fn format_bitwise_float(&self, input: f64, radix: usize) -> InternalString {
    let mut digit_stack: Vec<char> = vec![];
    let sign_padding_index = if input.is_sign_negative() { radix - 1 } else { 0 };
    let mut n = u64::from_be_bytes(match self.endianness {
      Endianness::Big => input.to_be_bytes(),
      Endianness::Little => input.to_le_bytes(),
    });

    let r = radix as u64;

    loop {
      let digit = DIGITS_DEFAULT[(n % r) as usize];
      digit_stack.push(if self.uppercase {
        digit.to_ascii_uppercase()
      } else {
        digit
      });

      n /= r;

      if n == 0 {
        break
      }
    }

    let mut buf = InternalString::new();
    let needed_padding = self.get_needed_padding(digit_stack.len());

    // Add prefix
    if self.alternate {
      buf.push_str(self.get_radix_prefix(radix));
    }

    // Add big-endian padding
    if self.endianness == Endianness::Big {
      for _ in 0..needed_padding {
        buf.push(DIGITS_DEFAULT[sign_padding_index]);
      }
    }

    // Add digits
    for digit in digit_stack.drain(..).rev() {
      buf.push(digit);
    }

    // Add little-endian padding
    if self.endianness == Endianness::Little {
      for _ in 0..needed_padding {
        buf.push(DIGITS_DEFAULT[sign_padding_index]);
      }
    }

    buf
  }

  fn format_bitwise_integer(&self, input: i64, radix: usize) -> InternalString {
    let mut digit_stack: Vec<char> = vec![];
    let sign_padding_index = if input < 0 { radix - 1 } else { 0 };
    let mut n = u64::from_be_bytes(match self.endianness {
      Endianness::Big => input.to_be_bytes(),
      Endianness::Little => input.to_le_bytes(),
    });

    let r = radix as u64;

    loop {
      let digit = DIGITS_DEFAULT[(n % r) as usize];
      digit_stack.push(if self.uppercase {
        digit.to_ascii_uppercase()
      } else {
        digit
      });

      n /= r;

      if n == 0 {
        break
      }
    }

    let mut buf = InternalString::new();
    let needed_padding = self.get_needed_padding(digit_stack.len());

    // Add prefix
    if self.alternate {
      buf.push_str(self.get_radix_prefix(radix));
    }

    // Add big-endian padding
    if self.endianness == Endianness::Big {
      for _ in 0..needed_padding {
        buf.push(DIGITS_DEFAULT[sign_padding_index]);
      }
    }

    // Add digits
    for digit in digit_stack.drain(..).rev() {
      buf.push(digit);
    }

    // Add little-endian padding
    if self.endianness == Endianness::Little {
      for _ in 0..needed_padding {
        buf.push(DIGITS_DEFAULT[sign_padding_index]);
      }
    }

    buf
  }

  fn format_babylonian_integer(&self, input: i64) -> InternalString {
    let mut bab_digit_stack = vec![];
    let mut buf = InternalString::new();
    let mut num_powers = 0;
    
    buf.push_str(self.get_integer_sign(input));

    let mut n = input;

    loop {
      let bab_digit = (n % 60).abs() as usize;

      // Add spaces between digits
      if num_powers > 0 {
        bab_digit_stack.push(" ");
      }

      if bab_digit > 0 {
        let ones_index = bab_digit % 10;
        let tens_index = bab_digit / 10;
        bab_digit_stack.push(BABYLONIAN_ONES[ones_index]);
        bab_digit_stack.push(if self.alternate { BABYLONIAN_TENS_ALT[tens_index] } else { BABYLONIAN_TENS[tens_index] });
      } else {
        bab_digit_stack.push(BABYLONIAN_ZERO);
      }

      num_powers += 1;
      n /= 60;
      if n == 0 { break }
    }

    for bab in bab_digit_stack.drain(..).rev() {
      buf.push_str(bab);
    }

    buf
  }

  fn format_roman_float(&self, input: f64) -> InternalString {
    let mut buf = InternalString::new();
    buf.push_str(self.get_float_sign(input));
    let n = input.abs().trunc();

    if n >= 1.0 {
      let ones = (n % 10.0) as usize;
      let tens = ((n % 100.0) / 10.0) as usize;
      let hundreds = ((n % 1000.0) / 100.0) as usize;
      let thousands = (n / 1000.0) as usize;
      for _ in 0..thousands {
        buf.push_str(ROMAN_THOUSAND);
      }
      buf.push_str(ROMAN_HUNDREDS[hundreds]);
      buf.push_str(ROMAN_TENS[tens]);
      buf.push_str(ROMAN_ONES[ones]);  
    } else {
      buf.push_str(ROMAN_ZERO);
    }

    if self.uppercase {
      buf.make_ascii_uppercase();
    }
    buf
  }

  fn format_roman_integer(&self, input: i64) -> InternalString {
    let mut buf = InternalString::new();
    buf.push_str(self.get_integer_sign(input));
    let n = input;

    if n > 0 {
      let ones = (n % 10).abs() as usize;
      let tens = ((n % 100).abs() / 10) as usize;
      let hundreds = ((n % 1000).abs() / 100) as usize;
      let thousands = (n / 1000).abs() as usize;
      for _ in 0..thousands {
        buf.push_str(ROMAN_THOUSAND);
      }
      buf.push_str(ROMAN_HUNDREDS[hundreds]);
      buf.push_str(ROMAN_TENS[tens]);
      buf.push_str(ROMAN_ONES[ones]);  
    } else {
      buf.push_str(ROMAN_ZERO);
    }

    if self.uppercase {
      buf.make_ascii_uppercase();
    }
    buf
  }

  fn format_decimal_float(&self, input: f64) -> InternalString {
    let mut buf = InternalString::new();

    // Add sign
    buf.push_str(self.get_float_sign(input));

    let raw_abs = if let Some(precision) = self.precision {
      format!("{:.1$}", input.abs(), precision as usize)
    } else {
      input.abs().to_string()
    };

    let raw_parts: Vec<&str> = raw_abs.as_str().split('.').collect();

    // raw_parts will always have 1..=2 components

    let raw_int = raw_parts[0];
    let num_int_digits = raw_int.len();
    let padding = self.get_needed_padding(num_int_digits);

    // Add padding
    for _ in 0..padding {
      buf.push(self.system.get_decimal_digit(0).unwrap());
    }

    // Add integral digits
    for (i, d) in raw_int.chars().enumerate() {
      let place = num_int_digits - i - 1;

      // Add group separator
      if let Some(group_sep) = &self.group_sep {
        if place > 3 && place % 3 == 0 {
          buf.push_str(group_sep);
        }
      }

      // Add digit
      buf.push(self.system.transliterate_decimal_digit(d).unwrap());
    }

    // Add fractional digits

    if raw_parts.len() == 2 {
      // Add decimal point
      buf.push_str(self.decimal_sep.as_deref().unwrap_or("."));

      let raw_frac = raw_parts[1];

      // Add fractional digits
      for d in raw_frac.chars() {
        buf.push(self.system.transliterate_decimal_digit(d).unwrap());
      }
    }

    buf
  }

  fn format_decimal_integer(&self, input: i64) -> InternalString {
    let mut buf = InternalString::new();
    let mut digit_stack: Vec<char> = vec![];
    let mut num_digits: u16 = 0;

    // Add sign
    buf.push_str(self.get_integer_sign(input));

    let mut n = input;

    // Add digits
    loop {
      let digit = (n % 10).abs() as usize;

      digit_stack.push(self.system.get_decimal_digit(digit).unwrap());
      num_digits += 1;

      n /= 10;
      if n == 0 { break }
    }

    // Add padding
    if self.padding > num_digits {
      let needed_padding = self.padding - num_digits;
      for _ in 0..needed_padding {
        buf.push(self.system.get_decimal_digit(0).unwrap());
      }
    }

    // Add digits
    for (p, d) in digit_stack.drain(..).enumerate().rev() {
      buf.push(d);

      // Add group separator
      if let Some(group_sep) = self.group_sep.as_deref() {
        if p > 0 && p % 3 == 0 {
          buf.push_str(group_sep);
        } 
      }
    }

    buf
  }
}