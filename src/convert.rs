use crate::value::*;
use crate::{runtime::VM, RantError, RantResult};
use std::rc::Rc;

/// Enables conversion from a native type to a `RantValue`.
pub trait ToRant {
    fn to_rant(self) -> RantValue;
}

/// Enables conversion from a `RantValue` to a native type.
pub trait FromRant: Sized {
    fn from_rant(val: RantValue) -> RantResult<Self>;
}

macro_rules! rant_int_conversions {
    ($int_type: ty) => {
        impl ToRant for $int_type {
            fn to_rant(self) -> RantValue {
                RantValue::Integer(self as i64)
            }
        }
        impl FromRant for $int_type {
            fn from_rant(val: RantValue) -> RantResult<Self> {
                if let RantValue::Integer(i) = val {
                    return Ok(i as Self)
                }

                let src_type = val.type_name();
                let dest_type = stringify!{$int_type};
                Err(RantError::ValueConversionError {
                    from: src_type,
                    to: dest_type,
                    message: Some(format!("Rant type '{}' cannot be converted to native type '{}'.", src_type, dest_type))
                })
            }
        }
    };
    ($int_type: ty, $($int_type2: ty), +) => {
        rant_int_conversions! { $int_type }
        rant_int_conversions! { $($int_type2), + }
    };
}

rant_int_conversions! { u8, i8, u16, i16, u32, i32, u64, i64, isize, usize }

impl FromRant for RantValue {
    fn from_rant(val: RantValue) -> RantResult<Self> {
        Ok(val)
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