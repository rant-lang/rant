use rand::Rng;
use rand_xoshiro::rand_core::SeedableRng;
use rand_xoshiro::Xoshiro256PlusPlus;
use std::{cell::RefCell, hash::Hasher};
use fnv::FnvHasher;
use crate::util::*;

/// Rant's random number generator, which is a thin wrapper around a xoshiro256++ PRNG.
#[derive(Debug)]
pub struct RantRng {
  seed: u64,
  rng: RefCell<Xoshiro256PlusPlus>,
}

impl RantRng {
  /// Creates a new RNG with the supplied seed.
  pub fn new(seed: u64) -> Self {
    Self {
      seed,
      rng: RefCell::new(Xoshiro256PlusPlus::seed_from_u64(seed))
    }
  }
  
  /// Creates a new RNG by hashing the parent seed with the supplied integer to produce a new seed.
  /// Uses the Fowler-Noll-Vo hash function.
  pub fn branch(&self, seed: u64) -> Self {
    let mut hasher = FnvHasher::default();
    hasher.write_u64(self.seed);
    hasher.write_u64(seed);
    RantRng::new(hasher.finish())
  }
  
  /// Creates a new RNG by hashing the parent seed with the supplied string to produce a new seed.
  /// Uses the Fowler-Noll-Vo hash function.
  pub fn branch_str(&self, seed: &str) -> Self {
    let mut hasher = FnvHasher::default();
    hasher.write_u64(self.seed);
    hasher.write(seed.as_bytes());
    RantRng::new(hasher.finish())
  }
}

impl RantRng {
  pub fn seed(&self) -> u64 {
    self.seed
  }
  
  /// Generates a pseudorandom `i64` between two inclusive values. The range may be specified in either order.
  #[inline]
  pub fn next_i64(&self, a: i64, b: i64) -> i64 {
    if a == b { return a }
    let (min, max) = minmax(a, b);
    self.rng.borrow_mut().gen_range(min, max + 1)
  }
  
  /// Generates a pseudorandom `f64` between two inclusive values. The range may be specified in either order.
  #[inline]
  pub fn next_f64(&self, a: f64, b: f64) -> f64 {
    if a.eq(&b) { return a }
    let (min, max) = minmax(a, b);
    self.rng.borrow_mut().gen_range(min, max)
  }
  
  #[inline]
  pub fn next_usize(&self, max: usize) -> usize {
    self.rng.borrow_mut().gen_range(0usize, max)
  }
  
  /// Generates a pseudorandom `f64` between 0 and 1.
  #[inline]
  pub fn next_normal_f64(&self) -> f64 {
    self.rng.borrow_mut().gen()
  }
  
  /// Generates a `bool` with `p` probability of being `true`.
  #[inline]
  pub fn next_bool(&self, p: f64) -> bool {
    self.rng.borrow_mut().gen_bool(saturate(p))
  }
}