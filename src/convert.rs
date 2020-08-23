//! # rant::convert
//! Provides conversions between RantValues and native types.

#![allow(unused_mut)]
#![allow(unused_parens)]
#![allow(unused_variables)]

use crate::value::*;
use crate::{lang::{Varity, Parameter, Identifier}, RantString, RantMapRef, stdlib::RantStdResult};
use crate::runtime::*;
use cast::*;
use cast::Error as CastError;
use std::{rc::Rc, ops::{DerefMut, Deref}};

/// Enables conversion from a native type to a `RantValue`.
pub trait ToRant {
  /// Convert to a `RantValue`.
  fn to_rant(self) -> Result<RantValue, ValueError>;
}

/// Enables conversion from a `RantValue` to a native type.
pub trait FromRant: Sized {
  /// Convert from a `RantValue`.
  fn from_rant(val: RantValue) -> Result<Self, ValueError>;
  /// Returns true if the type can be used to represent an optional Rant parameter.
  fn is_rant_optional() -> bool;
}

trait ToCastResult<T> {
  fn to_cast_result(self) -> Result<T, CastError>;
}

impl<T> ToCastResult<T> for Result<T, CastError> {
  fn to_cast_result(self) -> Result<T, CastError> {
    self
  }
}

impl ToCastResult<i64> for i64 {
  fn to_cast_result(self) -> Result<i64, CastError> {
    Ok(self)
  }
}

fn rant_cast_error(from: &'static str, to: &'static str, err: CastError) -> ValueError {
  ValueError::InvalidConversion {
    from,
    to,
    message: Some(match err {
      CastError::Overflow => "integer overflow",
      CastError::Underflow => "integer underflow",
      CastError::Infinite => "infinity",
      CastError::NaN => "not a number"
    }.to_owned())
  }
}

macro_rules! rant_int_conversions {
  ($int_type: ident) => {
    impl ToRant for $int_type {
      fn to_rant(self) -> ValueResult<RantValue> {
        match i64(self).to_cast_result() {
          Ok(i) => Ok(RantValue::Integer(i)),
          Err(err) => Err(rant_cast_error(
            stringify!($int_type), 
            stringify!(RantValue::Integer), 
            err
          ))
        }
      }
    }
    
    impl FromRant for $int_type {
      fn from_rant(val: RantValue) -> ValueResult<Self> {
        macro_rules! cast_int {
          (i64, $val:expr) => {
            Ok($val)
          };
          (isize, $val:expr) => {
            Ok(isize($val))
          };
          ($to:ident, $val:expr) => {
            $to($val)
          };
        }
        if let RantValue::Integer(i) = val {
          let result: Result<$int_type, CastError> = cast_int!($int_type, i);
          return match result {
            Ok(i) => Ok(i),
            Err(err) => Err(rant_cast_error(
              val.type_name(),
              stringify!($int_type),
              err
            ))
          }
        }
        
        // Other conversion failure
        let src_type = val.type_name();
        let dest_type = stringify!{$int_type};
        
        Err(ValueError::InvalidConversion {
          from: src_type,
          to: dest_type,
          message: None
        })
      }

      fn is_rant_optional() -> bool { false }
    }
  };
  ($int_type: ident, $($int_type2: ident), *) => {
    rant_int_conversions! { $int_type }
    rant_int_conversions! { $($int_type2), + }
  };
}

rant_int_conversions! { u8, i8, u16, i16, u32, i32, u64, i64, isize, usize }

impl FromRant for RantValue {
  #[inline(always)]
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    Ok(val)
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl FromRant for f32 {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Integer(i) => Ok(f32(i)),
      RantValue::Float(f) => match f32(f) {
        Ok(f) => Ok(f),
        Err(err) => Err(rant_cast_error(val.type_name(), "f32", err))
      },
      _ => Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "f32",
        message: Some(format!("Rant value type '{}' cannot be converted to f32", val.type_name()))
      })
    }
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl FromRant for f64 {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Integer(i) => Ok(f64(i)),
      RantValue::Float(f) => Ok(f),
      _ => Err(ValueError::InvalidConversion {
        from: val.type_name(),
        to: "f64",
        message: Some(format!("Rant value type '{}' cannot be converted to f64", val.type_name()))
      })
    }
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl ToRant for String {
  fn to_rant(self) -> ValueResult<RantValue> {
    Ok(RantValue::String(self))
  }
}

impl ToRant for &'static str {
  fn to_rant(self) -> ValueResult<RantValue> {
    Ok(RantValue::String(self.to_owned()))
  }
}

impl FromRant for String {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    Ok(val.to_string())
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl FromRant for RantMapRef {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    if let RantValue::Map(map_ref) = val {
      Ok(map_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: "map", message: None })
    }
  }
  fn is_rant_optional() -> bool {
    false
  }
}

impl<T: FromRant> FromRant for Option<T> {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Empty => Ok(None),
      other => Ok(Some(T::from_rant(other)?))
    }
  }
  fn is_rant_optional() -> bool {
    true
  }
}

impl<T: ToRant> ToRant for Option<T> {
  fn to_rant(self) -> ValueResult<RantValue> {
    match self {
      None => Ok(RantValue::Empty),
      Some(val) => Ok(val.to_rant()?)
    }
  }
}

impl<T: FromRant> FromRant for Vec<T> {
  fn from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::List(vec) => Ok(vec.borrow().iter().cloned().map(T::from_rant).collect::<ValueResult<Vec<T>>>()?),
      other => Err(ValueError::InvalidConversion {
        from: other.type_name(),
        to: stringify!(Vec<T>),
        message: Some("only lists can be turned into vectors".to_owned())
      })
    }
  }

  fn is_rant_optional() -> bool {
    false
  }
}

#[inline(always)]
fn as_varity<T: FromRant>() -> Varity {
  if T::is_rant_optional() {
    Varity::Optional
  } else {
    Varity::Required
  }
}

#[inline(always)]
fn inc(counter: &mut usize) -> usize {
  let prev = *counter;
  *counter += 1;
  prev
}

/// Converts from argument list to tuple of `impl FromRant` values
pub trait FromRantArgs: Sized {
  fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self>;
  fn as_rant_params() -> Vec<Parameter>;
}

impl<T: FromRant> FromRantArgs for T {
  fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self> {
    let mut args = args.into_iter();
    Ok(T::from_rant(args.next().unwrap_or(RantValue::Empty))?)
  }

  fn as_rant_params() -> Vec<Parameter> {
    let varity = if T::is_rant_optional() {
      Varity::Optional
    } else {
      Varity::Required
    };

    let param = Parameter {
      name: Identifier::new(RantString::from("arg0")),
      varity
    };

    vec![param]
  }
}

/// Semantic wrapper around a Vec<T> for use in optional variadic argument lists.
pub(crate) struct VarArgs<T: FromRant>(Vec<T>);

impl<T: FromRant> VarArgs<T> {
  pub fn new(args: Vec<T>) -> Self {
    Self(args)
  }
}

impl<T: FromRant> Deref for VarArgs<T> {
  type Target = Vec<T>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: FromRant> DerefMut for VarArgs<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

/// Semantic wrapper around a Vec<T> for use in required variadic argument lists.
pub(crate) struct RequiredVarArgs<T: FromRant>(Vec<T>);

impl<T: FromRant> RequiredVarArgs<T> {
  pub fn new(args: Vec<T>) -> Self {
    Self(args)
  }
}

impl<T: FromRant> Deref for RequiredVarArgs<T> {
  type Target = Vec<T>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: FromRant> DerefMut for RequiredVarArgs<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

macro_rules! impl_from_rant_args {
  ($($generic_types:ident),*) => {
    // Non-variadic implementation
    impl<$($generic_types: FromRant,)*> FromRantArgs for ($($generic_types,)*) {
      fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.into_iter();
        Ok(($($generic_types::from_rant(args.next().unwrap_or(RantValue::Empty))?,)*))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(RantString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
        },)*]
      }
    }
    
    // Variadic* implementation
    impl<$($generic_types: FromRant,)* VarArgItem: FromRant> FromRantArgs for ($($generic_types,)* VarArgs<VarArgItem>) {
      fn from_rant_args(mut args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.drain(..);
        Ok(
          ($($generic_types::from_rant(args.next().unwrap_or(RantValue::Empty))?,)*
          VarArgs::new(args
            .map(VarArgItem::from_rant)
            .collect::<ValueResult<Vec<VarArgItem>>>()?
          )
        ))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(RantString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
        },)*
        Parameter {
          name: Identifier::new(RantString::from(format!("arg{}", inc(&mut i)))),
          varity: Varity::VariadicStar,
        }]
      }
    }

    // Variadic+ implementation
    impl<$($generic_types: FromRant,)* VarArgItem: FromRant> FromRantArgs for ($($generic_types,)* RequiredVarArgs<VarArgItem>) {
      fn from_rant_args(mut args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.drain(..);
        Ok(
          ($($generic_types::from_rant(args.next().unwrap_or(RantValue::Empty))?,)*
          RequiredVarArgs::new(args
            .map(VarArgItem::from_rant)
            .collect::<ValueResult<Vec<VarArgItem>>>()?
          )
        ))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(RantString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
        },)*
        Parameter {
          name: Identifier::new(RantString::from(format!("arg{}", inc(&mut i)))),
          varity: Varity::VariadicPlus,
        }]
      }
    }
  }
}

impl_from_rant_args!();
impl_from_rant_args!(A);
impl_from_rant_args!(A, B);
impl_from_rant_args!(A, B, C);
impl_from_rant_args!(A, B, C, D);
impl_from_rant_args!(A, B, C, D, E);
impl_from_rant_args!(A, B, C, D, E, F);
impl_from_rant_args!(A, B, C, D, E, F, G);
impl_from_rant_args!(A, B, C, D, E, F, G, H);
impl_from_rant_args!(A, B, C, D, E, F, G, H, I);
impl_from_rant_args!(A, B, C, D, E, F, G, H, I, J);
impl_from_rant_args!(A, B, C, D, E, F, G, H, I, J, K);
//impl_from_rant_args!(A, B, C, D, E, F, G, H, I, J, K, L);

pub trait AsRantForeignFunc<Params: FromRantArgs> {
  fn as_rant_func(&'static self) -> RantFunction;
}

impl<Params: FromRantArgs, Function: Fn(&mut VM, Params) -> RantStdResult> AsRantForeignFunc<Params> for Function {
  fn as_rant_func(&'static self) -> RantFunction {
    let body = RantFunctionInterface::Foreign(Rc::new(move |vm, args| {
      self(vm, Params::from_rant_args(args).into_runtime_result()?)
    }));

    let params = Rc::new(Params::as_rant_params());

    RantFunction {
      body,
      captured_vars: None,
      min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
      vararg_start_index: params.iter()
      .enumerate()
      .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
      .unwrap_or_else(|| params.len()),
      params,
    }
  }
}