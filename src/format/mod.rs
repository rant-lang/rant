mod num;
mod ws;

pub use self::num::*;
pub use self::ws::*;

#[derive(Debug, Clone)]
pub struct OutputFormat {
  pub ws_norm_mode: WhitespaceNormalizationMode,
  pub num_format: NumberFormat,
}

impl Default for OutputFormat {
  fn default() -> Self {
    Self {
      ws_norm_mode: Default::default(),
      num_format: Default::default(),
    }
  }
}