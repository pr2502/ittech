//! Data type definitions.


macro_rules! ranged_u8_newtype {
    ( $name: ident, $range: expr ) => {
        #[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
        pub struct $name(u8);

        impl $name {
            pub(crate) fn as_u8(self) -> u8 {
                self.0
            }
        }

        impl ::std::convert::TryFrom<u8> for $name {
            type Error = ::std::num::TryFromIntError;

            fn try_from(raw: u8) -> Result<Self, Self::Error> {
                fn _typecheck(_: impl ::std::ops::RangeBounds<u8>) {}
                _typecheck($range);

                if $range.contains(&raw) {
                    Ok($name(raw))
                } else {
                    // There is no public constructor for `TryFromIntError` so we obtain it through a
                    // definitely-out-of-range cast ... :/
                    Err(u8::try_from(u16::MAX).unwrap_err())
                }
            }
        }

        impl From<$name> for u8 {
            fn from(ranged: $name) -> u8 {
                ranged.as_u8()
            }
        }

        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::fmt::Debug::fmt(&self.as_u8(), f)
            }
        }
    };
}


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
pub use channel::*;

mod envelope;
pub use envelope::*;

mod instrument;
pub use instrument::*;

mod module;
pub use module::*;

mod pattern;
pub use pattern::*;

mod sample;
pub use sample::*;

mod util;
pub use util::*;
