use super::RangedU8;
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug};
use std::iter::FromIterator;
use std::num::TryFromIntError;
use std::ops::{BitOr, BitOrAssign};


#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Channel(RangedU8<0, 63>);

impl Channel {
    pub const MAX: u8 = 63;

    pub fn as_usize(self) -> usize {
        self.0.as_u8() as usize
    }

    pub(crate) fn from_u8_truncate(raw: u8) -> Channel {
        Channel((raw & 63).try_into().unwrap())
    }
}

impl TryFrom<u8> for Channel {
    type Error = TryFromIntError;
    fn try_from(raw: u8) -> Result<Self, Self::Error> {
        Ok(Channel(raw.try_into()?))
    }
}

impl From<Channel> for u8 {
    fn from(chan: Channel) -> u8 {
        chan.0.as_u8()
    }
}

impl Debug for Channel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.as_u8().fmt(f)
    }
}


/// Mask representing active channels in a particular pattern or module.
#[derive(Clone, Copy)]
pub struct ActiveChannels(u64);

impl ActiveChannels {
    pub const fn all() -> ActiveChannels {
        ActiveChannels(u64::MAX)
    }

    pub const fn empty() -> ActiveChannels {
        ActiveChannels(0)
    }

    pub fn iter(self) -> impl Iterator<Item=Channel> {
        (0..=Channel::MAX)
            .filter(move |chan| (self.0 & (1u64 << chan)) != 0)
            .map(|chan| chan.try_into().unwrap())
    }

    pub fn count(self) -> usize {
        self.0.count_ones() as usize
    }
}


impl BitOr<ActiveChannels> for ActiveChannels {
    type Output = ActiveChannels;
    fn bitor(self, rhs: ActiveChannels) -> Self::Output {
        ActiveChannels(self.0 | rhs.0)
    }
}

impl BitOrAssign for ActiveChannels {
    fn bitor_assign(&mut self, rhs: ActiveChannels) {
        *self = *self | rhs;
    }
}

impl FromIterator<Channel> for ActiveChannels {
    fn from_iter<I: IntoIterator<Item=Channel>>(iter: I) -> ActiveChannels {
        ActiveChannels(
            iter.into_iter()
                .map(|chan| 1u64 << chan.as_usize())
                .fold(0u64, u64::bitor)
        )
    }
}

impl Debug for ActiveChannels {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_list()
            .entries(self.iter())
            .finish()
    }
}
