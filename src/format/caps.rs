const DEFAULT_LOWERCASE: &str = "abcdefghijklmnopqrstuvwxyz";
const DEFAULT_UPPERCASE: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

/// Provides locale disambiguation for casing operations.
#[derive(Debug, Clone)]
pub enum CapsLocale {
  /// Capitalize according to the default behavior defined by Unicode.
  Invariant,
  /// Capitalize according to Turkish casing rules.
  Turkish
}

pub enum CapsStyle {
  Normal,
  Upper,
  Lower,
  Title,
  FirstLetter,
  Sentence,
}