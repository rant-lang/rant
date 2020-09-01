use crate::RantValue;

#[derive(Debug, Clone)]
pub struct OutputFormat {
  pub ws_norm_mode: WhitespaceNormalizationMode,
}

impl Default for OutputFormat {
  fn default() -> Self {
    Self {
      ws_norm_mode: Default::default(),
    }
  }
}

#[derive(Debug, Clone)]
pub enum WhitespaceNormalizationMode {
  /// Normalizes all whitespace tokens to a single ASCII space character (0x20).
  Default,
  /// Strips all (non-literal) whitespace.
  IgnoreAll,
  /// Prints all non-breaking whitespace verbatim.
  Verbatim,
  /// Normalizes all whitespace to a custom value.
  Custom(RantValue)
}

impl Default for WhitespaceNormalizationMode {
  fn default() -> Self {
    Self::Default
  }
}