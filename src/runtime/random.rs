use rand::Rng;
use rand_xoshiro::rand_core::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;

/// Rant's random number generator, which is a thin wrapper around a xoshiro256++ PRNG.
pub struct RantRng {
    seed: u64,
    rng: Xoshiro256PlusPlus,
}

impl RantRng {
    pub fn new(seed: u64) -> Self {
        Self {
            seed,
            rng: Xoshiro256PlusPlus::seed_from_u64(seed)
        }
    }
}