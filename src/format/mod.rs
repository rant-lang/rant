mod num;
mod case;
mod ws;

pub use self::num::*;
pub use self::case::*;
pub use self::ws::*;

#[derive(Debug, Clone, Default)]
pub struct OutputFormat {
  pub whitespace_format: WhitespaceNormalizationMode,
  pub number_format: NumberFormat,
  pub casing_format: CasingFormat,
}