mod num;
mod caps;
mod ws;

pub use self::num::*;
pub use self::caps::*;
pub use self::ws::*;

#[derive(Debug, Clone, Default)]
pub struct OutputFormat {
  pub ws_norm_mode: WhitespaceNormalizationMode,
  pub num_format: NumberFormat,
}