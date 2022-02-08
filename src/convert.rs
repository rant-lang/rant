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

/// Enables infallible conversion into a `RantValue`.
pub trait IntoRant: Sized {
  /// Converts to a `RantValue`.
  fn into_rant(self) -> RantValue;
}

/// Enables fallible conversion into a `RantValue`.
pub trait TryIntoRant: Sized {
  /// Attempts to convert to a `RantValue`.
  fn try_into_rant(self) -> Result<RantValue, ValueError>;
}

pub trait FromRant: Sized {
  /// Converts from a `RantValue`.
  fn from_rant(val: RantValue) -> Self;

  /// Returns `true` if the type can be used to represent an optional Rant parameter in native functions; otherwise, `false`.
  fn is_optional_param_type() -> bool {
    false
  }
}

/// Enables fallible conversion from a `RantValue`.
pub trait TryFromRant: Sized {
  /// Convert from a `RantValue`.
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError>;
  
  /// Returns `true` if the type can be used to represent an optional Rant parameter in native functions; otherwise, `false`.
  fn is_optional_param_type() -> bool {
    false
  }
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

macro_rules! rant_fallible_int_conversions {
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
    }
  };
  ($int_type: ident, $($int_type2: ident), *) => {
    rant_fallible_int_conversions! { $int_type }
    rant_fallible_int_conversions! { $($int_type2), + }
  };
}

/// Implements `FromRant` and `TryFromRant` for a type.
macro_rules! converts_from_rant {
  ($param:ident -> $t:ty $b:block) => {
    impl FromRant for $t {
      fn from_rant($param: RantValue) -> $t {
        $b
      }
    }

    impl TryFromRant for $t {
      fn try_from_rant(val: RantValue) -> Result<$t, ValueError> {
        Ok(<$t as FromRant>::from_rant(val))
      }
    }
  }
}

/// Implements `IntoRant` and `TryIntoRant` for a type.
macro_rules! converts_into_rant {
  ($param:ident: $t:ty $b:block) => {
    impl IntoRant for $t {
      fn into_rant(self) -> RantValue {
        let $param = self;
        $b
      }
    }

    impl TryIntoRant for $t {
      fn try_into_rant(self) -> Result<RantValue, ValueError> {
        Ok(IntoRant::into_rant(self))
      }
    }
  }
}

rant_fallible_int_conversions! { u8, i8, u16, i16, u32, i32, u64, i64, isize, usize }

converts_from_rant!(v -> RantNothing { Self });
converts_from_rant!(v -> RantValue { v });
converts_from_rant!(v -> bool { v.to_bool() });
converts_from_rant!(v -> InternalString { v.to_string().into() });
converts_from_rant!(v -> RantString { v.to_string().into() });
converts_from_rant!(v -> String { v.to_string() });

converts_into_rant!(v: RantValue { v });
converts_into_rant!(v: RantNothing { RantValue::Nothing });
converts_into_rant!(v: bool { RantValue::Boolean(v) });
converts_into_rant!(v: f32 { RantValue::Float(v as f64) });
converts_into_rant!(v: f64 { RantValue::Float(v) });
converts_into_rant!(v: String { RantValue::String(v.into()) });
converts_into_rant!(v: RantString { RantValue::String(v) });
converts_into_rant!(v: InternalString { RantString::from(v.as_str()).into_rant() });
converts_into_rant!(v: RantMap { RantValue::Map(v.into_handle()) });
converts_into_rant!(v: RantMapHandle { RantValue::Map(v) });
converts_into_rant!(v: RantList { RantValue::List(v.into_handle()) });
converts_into_rant!(v: RantListHandle { RantValue::List(v) });
converts_into_rant!(v: RantTuple { RantValue::Tuple(v.into_handle()) });
converts_into_rant!(v: RantTupleHandle { RantValue::Tuple(v) });
converts_into_rant!(v: RantSpecial { RantValue::Special(v) });
converts_into_rant!(v: RantRange { RantValue::Range(v) });

impl<'a> IntoRant for &'a str {
  fn into_rant(self) -> RantValue {
    RantValue::String(self.into())
  }
}

impl<'a> TryIntoRant for &'a str {
  fn try_into_rant(self) -> Result<RantValue, ValueError> {
    Ok(self.into_rant())
  }
}

impl IntoRant for isize {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for i64 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self)
  }
}

impl IntoRant for i32 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for u32 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for i16 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for u16 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for i8 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
  }
}

impl IntoRant for u8 {
  fn into_rant(self) -> RantValue {
    RantValue::Int(self as i64)
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
}

impl<T: IntoRant> IntoRant for Vec<T> {
  fn into_rant(mut self) -> RantValue {
    let list = self.drain(..).map(|v| v.into_rant()).collect::<RantList>();
    RantValue::List(list.into_handle())
  }
}

impl<T: TryIntoRant> TryIntoRant for Vec<T> {
  fn try_into_rant(mut self) -> Result<RantValue, ValueError> {
    let list = self.drain(..).map(|v| v.try_into_rant()).collect::<Result<RantList, ValueError>>()?;
    Ok(RantValue::List(RantList::from(list).into_handle()))
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
}

impl TryFromRant for RantListHandle {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    if let RantValue::List(list_ref) = val {
      Ok(list_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::List.name(), message: None })
    }
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
}

impl TryFromRant for RantFunctionHandle {
  fn try_from_rant(val: RantValue) -> Result<Self, ValueError> {
    if let RantValue::Function(func_ref) = val {
      Ok(func_ref)
    } else {
      Err(ValueError::InvalidConversion { from: val.type_name(), to: RantValueType::Function.name(), message: None })
    }
  }
}

impl<T: TryFromRant> TryFromRant for Option<T> {
  fn try_from_rant(val: RantValue) -> ValueResult<Self> {
    match val {
      RantValue::Nothing => Ok(None),
      other => Ok(Some(T::try_from_rant(other)?))
    }
  }
  fn is_optional_param_type() -> bool {
    true
  }
}

impl<T: TryIntoRant> TryIntoRant for Option<T> {
  fn try_into_rant(self) -> ValueResult<RantValue> {
    match self {
      Some(val) => Ok(val.try_into_rant()?),
      None => Ok(RantValue::Nothing),
    }
  }
}

impl<T: IntoRant> IntoRant for Option<T> {
  fn into_rant(self) -> RantValue {
    match self {
      Some(val) => val.into_rant(),
      None => RantValue::Nothing,
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
}

#[inline(always)]
fn as_varity<T: TryFromRant>() -> Varity {
  if T::is_optional_param_type() {
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
    T::try_from_rant(args.next().unwrap_or(RantValue::Nothing))
  }

  fn as_rant_params() -> Vec<Parameter> {
    let varity = if T::is_optional_param_type() {
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

/// Semantic wrapper around a `Vec<T>`.
/// 
/// Use this type to add an optional variadic (`*`) parameter to native functions.
pub struct VarArgs<T: TryFromRant>(Vec<T>);

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

/// Semantic wrapper around a `Vec<T>`.
/// 
/// Use this type to add a required variadic (`+`) parameter to native functions.
pub struct RequiredVarArgs<T: TryFromRant>(Vec<T>);

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
        Ok(($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Nothing))?,)*))
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
          ($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Nothing))?,)*
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
          ($($generic_types::try_from_rant(args.next().unwrap_or(RantValue::Nothing))?,)*
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
pub trait IntoRantFunction<Params: FromRantArgs> {
  /// Performs the conversion.
  fn into_rant_func(self) -> RantFunction;
}

impl<Params: FromRantArgs, Function: Fn(&mut VM, Params) -> RantStdResult> IntoRantFunction<Params> for &'static Function {
  fn into_rant_func(self) -> RantFunction {
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