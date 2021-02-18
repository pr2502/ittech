//! Data type definitions.


use crate::error::OutOfRangeError;
pub(crate) use bitflags::bitflags;
use std::convert::TryFrom;
use std::fmt::{self, Debug};


macro_rules! ranged_u8_newtype {
    ( $name: ident, $low: literal ..= $high: literal ) => {
        #[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
        pub struct $name(u8);

        impl $name {
            pub(crate) fn as_u8(self) -> u8 {
                self.0
            }
        }

        impl TryFrom<u8> for $name {
            type Error = OutOfRangeError<$low, $high>;

            fn try_from(raw: u8) -> Result<Self, Self::Error> {
                if ($low..=$high).contains(&raw) {
                    Ok($name(raw))
                } else {
                    Err(OutOfRangeError(raw))
                }
            }
        }

        impl From<$name> for u8 {
            fn from(ranged: $name) -> u8 {
                ranged.as_u8()
            }
        }

        impl Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                Debug::fmt(&self.as_u8(), f)
            }
        }
    };
}

// TODO We're already using range-limited opaque numbers for the IDs, ideally we'd be also
//      versioning them (like <https://crates.io/crates/slotmap>) to make sure we always catch
//      nonsensical values both during parsing and during serialization.
ranged_u8_newtype!(InstrumentId, 0..=98);
ranged_u8_newtype!(PatternId, 0..=199);
ranged_u8_newtype!(SampleId, 0..=98);


pub trait Get<I> {
    type Output;
    fn get(&self, index: I) -> Option<&Self::Output>;
}

impl<I, C> Get<&I> for C
where
    C: Get<I>,
    I: Copy,
{
    type Output = <C as Get<I>>::Output;
    fn get(&self, index: &I) -> Option<&Self::Output> {
        self.get(*index)
    }
}

macro_rules! impl_index_from_get {
    ( $for: ty, $idx: ty ) => {
        impl ::std::ops::Index<$idx> for $for {
            type Output = <$for as $crate::data::Get<$idx>>::Output;
            fn index(&self, index: $idx) -> &Self::Output {
                self.get(index)
                    .unwrap_or_else(|| panic!("{} index {:?} out of range", ::std::any::type_name::<$idx>(), &index))
            }
        }

        impl ::std::ops::Index<&$idx> for $for {
            type Output = <$for as $crate::data::Get<$idx>>::Output;
            fn index(&self, index: &$idx) -> &Self::Output {
                self.get(index)
                    .unwrap_or_else(|| panic!("{} index {:?} out of range", ::std::any::type_name::<$idx>(), &index))
            }
        }
    };
}


mod channel;
mod envelope;
mod instrument;
mod module;
mod pattern;
mod sample;
mod util;

pub use channel::*;
pub use envelope::*;
pub use instrument::*;
pub use module::*;
pub use pattern::*;
pub use sample::*;
pub use util::*;
