//! # rant::convert
//! Provides conversions between RantValues and native types.

#![allow(unused_mut)]
#![allow(unused_parens)]
#![allow(unused_variables)]

use crate::value::*;
use crate::{runtime::VM, RantError, RantResult};
use cast::*;
use cast::Error as CastError;
use std::{rc::Rc};

/// Enables conversion from a native type to a `RantValue`.
pub trait ToRant {
    /// Convert to a `RantValue`.
    fn to_rant(self) -> RantResult<RantValue>;
}

/// Enables conversion from a `RantValue` to a native type.
pub trait FromRant: Sized {
    /// Convert from a `RantValue`.
    fn from_rant(val: RantValue) -> RantResult<Self>;
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

fn rant_cast_error(from: &'static str, to: &'static str, err: CastError) -> RantError {
    RantError::ValueConversionError {
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
            fn to_rant(self) -> RantResult<RantValue> {
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
            fn from_rant(val: RantValue) -> RantResult<Self> {
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

                Err(RantError::ValueConversionError {
                    from: src_type,
                    to: dest_type,
                    message: None
                })
            }
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
    fn from_rant(val: RantValue) -> RantResult<Self> {
        Ok(val)
    }
}

impl FromRant for f32 {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        match val {
            RantValue::Integer(i) => Ok(f32(i)),
            RantValue::Float(f) => match f32(f) {
                Ok(f) => Ok(f),
                Err(err) => Err(rant_cast_error(val.type_name(), "f32", err))
            },
            _ => Err(RantError::ValueConversionError {
                from: val.type_name(),
                to: "f32",
                message: Some(format!("Rant value type '{}' cannot be converted to f32", val.type_name()))
            })
        }
    }
}

impl FromRant for f64 {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        match val {
            RantValue::Integer(i) => Ok(f64(i)),
            RantValue::Float(f) => Ok(f),
            _ => Err(RantError::ValueConversionError {
                from: val.type_name(),
                to: "f64",
                message: Some(format!("Rant value type '{}' cannot be converted to f64", val.type_name()))
            })
        }
    }
}

impl ToRant for String {
    fn to_rant(self) -> RantResult<RantValue> {
        Ok(RantValue::String(self))
    }
}

impl ToRant for &'static str {
    fn to_rant(self) -> RantResult<RantValue> {
        Ok(RantValue::String(self.to_owned()))
    }
}

impl FromRant for String {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        Ok(val.as_string())
    }
}

impl<T: FromRant> FromRant for Option<T> {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        match val {
            RantValue::None => Ok(None),
            other => Ok(Some(T::from_rant(other)?))
        }
    }
}

impl<T: ToRant> ToRant for Option<T> {
    fn to_rant(self) -> RantResult<RantValue> {
        match self {
            None => Ok(RantValue::None),
            Some(val) => Ok(val.to_rant()?)
        }
    }
}

impl<T: FromRant> FromRant for Vec<T> {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        match val {
            RantValue::List(vec) => Ok(vec.iter().cloned().map(T::from_rant).collect::<RantResult<Vec<T>>>()?),
            other => Err(RantError::ValueConversionError {
                from: other.type_name(),
                to: stringify!(Vec<T>),
                message: Some("only lists can be turned into vectors".to_owned())
            })
        }
    }
}

/// Converts from argument list to tuple of `impl FromRant` values
pub trait FromRantArgs: Sized {
    fn from_rant_args(args: Vec<RantValue>) -> RantResult<Self>;
}

impl<T: FromRant> FromRantArgs for T {
    fn from_rant_args(args: Vec<RantValue>) -> RantResult<Self> {
        let mut args = args.into_iter();
        Ok(T::from_rant(args.next().unwrap_or(RantValue::None))?)
    }
}

macro_rules! impl_from_rant_args {
    ($($generic_types:ident),*) => {
        // Non-variadic implementation
        impl<$($generic_types: FromRant,)*> FromRantArgs for ($($generic_types,)*) {
            fn from_rant_args(args: Vec<RantValue>) -> RantResult<Self> {
                let mut args = args.into_iter();
                Ok(($($generic_types::from_rant(args.next().unwrap_or(RantValue::None))?,)*))
            }
        }

        // Variadic implementation
        impl<$($generic_types: FromRant,)* Variadic: FromRant> FromRantArgs for ($($generic_types,)* VarArgs<Variadic>) {
            fn from_rant_args(mut args: Vec<RantValue>) -> RantResult<Self> {
                let mut args = args.drain(..);
                Ok(
                    ($($generic_types::from_rant(args.next().unwrap_or(RantValue::None))?,)*
                    VarArgs::new(args
                        .map(Variadic::from_rant)
                        .collect::<RantResult<Vec<Variadic>>>()?
                    )
                ))
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
impl_from_rant_args!(A, B, C, D, E, F, G, H, I, J, K, L);

pub trait AsRantForeignFunc<Params: FromRantArgs> {
    fn as_rant_func(&'static self) -> RantFunction;
}

impl<Params: FromRantArgs, Function: Fn(&mut VM, Params) -> RantResult<()>> AsRantForeignFunc<Params> for Function {
    fn as_rant_func(&'static self) -> RantFunction {
        RantFunction::Foreign(Rc::new(move |vm, args| {
            self(vm, Params::from_rant_args(args)?)
        }))
    }
}