//! # rant::convert
//! Provides conversions between `RantValue`s and native types.

#![allow(unused_mut)]
#![allow(unused_parens)]
#![allow(unused_variables)]

use crate::*;
use crate::runtime::*;
use crate::{lang::{Varity, Parameter, Identifier}, stdlib::RantStdResult};
use cast::*;
use cast::Error as CastError;
use std::{rc::Rc, ops::{DerefMut, Deref}, convert::TryInto};

/// Enables fallible conversion from a native type to a `RantValue`.
pub trait TryIntoRant: Sized {
  /// Convert to a `RantValue`.
  fn try_into_rant(self) -> Result<RantValue, ValueError>;
}

/// Enables fallible conversion from a `RantValue` to a native type.
pub trait TryFromRant: Sized {
  /// Convert from a `RantValue`.
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError>;
  /// Returns true if the type can be used to represent an optional Rant parameter.
  fn is_rant_optional() -> bool;
}

trait IntoCastResult<T> {
  fn into_cast_result(self) -> Result<T, CastError>;
}

impl<T> IntoCastResult<T> for Result<T, CastError> {
  fn into_cast_result(self) -> Result<T, CastError> {
    self
  }
}

impl IntoCastResult<i64> for i64 {
  fn into_cast_result(self) -> Result<i64, CastError> {
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
      CastError::NaN => "NaN"
    }.to_owned())
  }
}

macro_rules! rant_int_conversions {
  ($int_type: ident) => {
    impl TryIntoRant for $int_type {
      fn try_into_rant(self) -> ValueResult<RantValue> {
        match i64(self).into_cast_result() {
          Ok(i) => Ok(RantValue::Int(i)),
          Err(err) => Err(rant_cast_error(
            stringify!($int_type), 
            stringify!(RantValue::Int), 
            err
          ))
        }
      }
    }
    
    impl TryFromRant for $int_type {
      fn try_from_rant(val: RantValue) -> ValueResult<Self> {
        macro_rules! cast_int {
          (i64, $val:expr) => {
            Ok($val)
          };
          (isize, $val:expr) => {
            {
              let val = $val;
              val.try_into().map_err(|_| if val < 0 {
                CastError::Underflow
              } else {
                CastError::Overflow
              })
            }
          };
          ($to:ident, $val:expr) => {
            $to($val)
          };
        }
        macro_rules! cast_float_to_int {
          (i64, $val:expr) => {
            Ok($val as i64)
          };
          ($to:ident, $val:expr) => {
            $to($val)
          };
        }
        match val {
          RantValue::Int(i) => {
            let result: Result<$int_type, CastError> = cast_int!($int_type, i);
            match result {
              Ok(i) => Ok(i),
              Err(err) => Err(rant_cast_error(
                val.type_name(),
                stringify!($int_type),
                err
              ))
            }
          },
          RantValue::Float(f) => {
            let result: Result<$int_type, CastError> = cast_float_to_int!($int_type, f);
            match result {
              Ok(i) => Ok(i),
              Err(err) => Err(rant_cast_error(
                val.type_name(),
                stringify!($int_type),
                err
              ))
            }
          },
          _ => {
            // Other conversion failure
            let src_type = val.type_name();
            let dest_type = stringify!{$int_type};
            
            Err(ValueError::InvalidConversion {
              from: src_type,
              to: dest_type,
              message: None
            })
          }
        }
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

impl TryFromRant for RantValue {
  #[inline(always)]
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    Ok(val)
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for RantEmpty {
  fn try_from_rant(_: RantValue) -> Result<Self, ValueError> {
    Ok(RantEmpty)
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for bool {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    Ok(val.to_bool())
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl TryIntoRant for bool {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::Boolean(self))
  }
}

impl TryFromRant for f32 {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Int(i) => Ok(f32(i)),
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

impl TryFromRant for f64 {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Int(i) => Ok(f64(i)),
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

impl TryIntoRant for f32 {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::Float(self as f64))
  }
}

impl TryIntoRant for f64 {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::Float(self))
  }
}


impl TryIntoRant for String {
  fn try_into_rant(self) -> ValueResult<RantValue> {
    Ok(RantValue::String(self.into()))
  }
}

impl TryIntoRant for RantMap {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::Map(self.into_handle()))
  }
}

impl TryIntoRant for RantList {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::List(self.into_handle()))
  }
}

impl TryIntoRant for RantTuple {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::Tuple(self.into_handle()))
  }
}

impl TryIntoRant for InternalString {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    RantString::from(self.as_str()).try_into_rant()
  }
}

impl TryFromRant for InternalString {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    Ok(InternalString::from(val.to_string()))
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl TryIntoRant for &'static str {
  fn try_into_rant(self) -> ValueResult<RantValue> {
    Ok(RantValue::String(self.into()))
  }
}

impl TryIntoRant for Vec<RantValue> {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(RantValue::List(RantList::from(self).into_handle()))
  }
}

impl<T: TryIntoRant> TryIntoRant for Vec<T> {
  fn try_into_rant(mut self) -> Result<RantValue, ValueError> {
    let list = self.drain(..).map(|v| v.try_into_rant()).collect::<Result<RantList, ValueError>>()?;
    Ok(RantValue::List(RantList::from(list).into_handle()))
  }
}

impl TryFromRant for String {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    Ok(val.to_string())
  }

  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for RantTupleHandle {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    if let RantValue::Tuple(tuple_ref) = val {
      Ok(tuple_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::Tuple.name(), message: None })
    }
  }
  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for RantListHandle {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    if let RantValue::List(list_ref) = val {
      Ok(list_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::List.name(), message: None })
    }
  }
  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for RantMapHandle {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    if let RantValue::Map(map_ref) = val {
      Ok(map_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::Map.name(), message: None })
    }
  }
  fn is_rant_optional() -> bool {
    false
  }
}

impl TryFromRant for RantFunctionHandle {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    if let RantValue::Function(func_ref) = val {
      Ok(func_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::Function.name(), message: None })
    }
  }
  fn is_rant_optional() -> bool {
    false
  }
}

impl<T: TryFromRant> TryFromRant for Option<T> {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Empty => Ok(None),
      other => Ok(Some(T::try_from_rant(other)?))
    }
  }
  fn is_rant_optional() -> bool {
    true
  }
}

impl<T: TryIntoRant> TryIntoRant for Option<T> {
  fn try_into_rant(self) -> ValueResult<RantValue> {
    match self {
      None => Ok(RantValue::Empty),
      Some(val) => Ok(val.try_into_rant()?)
    }
  }
}

impl<T: TryFromRant> TryFromRant for Vec<T> {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::List(vec) => Ok(vec.borrow().iter().cloned().map(T::try_from_rant).collect::<ValueResult<Vec<T>>>()?),
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
fn as_varity<T: TryFromRant>() -> Varity {
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

/// Converts from argument list to tuple of `impl TryFromRant` values
pub trait FromRantArgs: Sized {
  fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self>;
  fn as_rant_params() -> Vec<Parameter>;
}

impl<T: TryFromRant> FromRantArgs for T {
  fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self> {
    let mut args = args.into_iter();
    T::try_from_rant(args.next().unwrap_or(RantValue::Empty))
  }

  fn as_rant_params() -> Vec<Parameter> {
    let varity = if T::is_rant_optional() {
      Varity::Optional
    } else {
      Varity::Required
    };

    let param = Parameter {
      name: Identifier::new(InternalString::from("arg0")),
      varity,
      default_value_expr: None,
    };

    vec![param]
  }
}

/// Semantic wrapper around a Vec<T> for use in optional variadic argument lists.
pub(crate) struct VarArgs<T: TryFromRant>(Vec<T>);

impl<T: TryFromRant> VarArgs<T> {
  pub fn new(args: Vec<T>) -> Self {
    Self(args)
  }
}

impl<T: TryFromRant> Deref for VarArgs<T> {
  type Target = Vec<T>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: TryFromRant> DerefMut for VarArgs<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

impl<T: TryFromRant> VarArgs<T> {
  #[inline]
  pub fn into_vec(self) -> Vec<T> {
    self.0
  }
}

/// Semantic wrapper around a Vec<T> for use in required variadic argument lists.
pub(crate) struct RequiredVarArgs<T: TryFromRant>(Vec<T>);

impl<T: TryFromRant> RequiredVarArgs<T> {
  pub fn new(args: Vec<T>) -> Self {
    Self(args)
  }
}

impl<T: TryFromRant> Deref for RequiredVarArgs<T> {
  type Target = Vec<T>;
  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl<T: TryFromRant> DerefMut for RequiredVarArgs<T> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.0
  }
}

macro_rules! impl_from_rant_args {
  ($($generic_types:ident),*) => {
    // Non-variadic implementation
    impl<$($generic_types: TryFromRant,)*> FromRantArgs for ($($generic_types,)*) {
      fn from_rant_args(args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.into_iter();
        Ok(($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Empty))?,)*))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(InternalString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
          default_value_expr: None,
        },)*]
      }
    }
    
    // Variadic* implementation
    impl<$($generic_types: TryFromRant,)* VarArgItem: TryFromRant> FromRantArgs for ($($generic_types,)* VarArgs<VarArgItem>) {
      fn from_rant_args(mut args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.drain(..);
        Ok(
          ($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Empty))?,)*
          VarArgs::new(args
            .map(VarArgItem::try_from_rant)
            .collect::<ValueResult<Vec<VarArgItem>>>()?
          )
        ))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(InternalString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
          default_value_expr: None,
        },)*
        Parameter {
          name: Identifier::new(InternalString::from(format!("arg{}", inc(&mut i)))),
          varity: Varity::VariadicStar,
          default_value_expr: None,
        }]
      }
    }

    // Variadic+ implementation
    impl<$($generic_types: TryFromRant,)* VarArgItem: TryFromRant> FromRantArgs for ($($generic_types,)* RequiredVarArgs<VarArgItem>) {
      fn from_rant_args(mut args: Vec<RantValue>) -> ValueResult<Self> {
        let mut args = args.drain(..);
        Ok(
          ($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Empty))?,)*
          RequiredVarArgs::new(args
            .map(VarArgItem::try_from_rant)
            .collect::<ValueResult<Vec<VarArgItem>>>()?
          )
        ))
      }

      fn as_rant_params() -> Vec<Parameter> {
        let mut i: usize = 0;
        vec![$(Parameter { 
          name: Identifier::new(InternalString::from(format!("arg{}", inc(&mut i)))),
          varity: as_varity::<$generic_types>(),
          default_value_expr: None,
        },)*
        Parameter {
          name: Identifier::new(InternalString::from(format!("arg{}", inc(&mut i)))),
          varity: Varity::VariadicPlus,
          default_value_expr: None,
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

/// Trait for converting something to a Rant function.
pub trait AsRantFunction<Params: FromRantArgs> {
  /// Performs the conversion.
  fn as_rant_func(&'static self) -> RantFunction;
}

impl<Params: FromRantArgs, Function: Fn(&mut VM, Params) -> RantStdResult> AsRantFunction<Params> for Function {
  fn as_rant_func(&'static self) -> RantFunction {
    let body = RantFunctionInterface::Foreign(Rc::new(move |vm, args| {
      self(vm, Params::from_rant_args(args).into_runtime_result()?)
    }));

    let params = Rc::new(Params::as_rant_params());

    RantFunction {
      body,
      captured_vars: vec![],
      min_arg_count: params.iter().take_while(|p| p.is_required()).count(),
      vararg_start_index: params.iter()
      .enumerate()
      .find_map(|(i, p)| if p.varity.is_variadic() { Some(i) } else { None })
      .unwrap_or_else(|| params.len()),
      params,
      flavor: None,
    }
  }
}