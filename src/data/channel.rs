use super::*;
use std::borrow::Borrow;
use std::convert::TryInto;
use std::fmt::{self, Debug};
use std::iter::FromIterator;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};


#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct Channel(RangedU8<0, 63>);

impl Channel {
    /// Create a channel identifier with the given number
    ///
    /// Accepted range is 1..=64
    pub fn new(number: u8) -> Channel {
        assert!((1..=64).contains(&number), "channel number is out of range");
        Channel::from_u8_index(number - 1)
    }

    /// Returns 0 based channel index (0..=63), as opposed to channel number (1..=64)
    pub fn as_usize(self) -> usize {
        self.0.as_u8() as usize
    }

    /// Creates channel from channel index (0..=63), as opposed to channel number (1..=64)
    pub(crate) fn from_u8_index(raw: u8) -> Channel {
        Channel(raw.try_into().expect("channel index out of range"))
    }
}

impl Debug for Channel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ch{:02}", self.0.as_u8() + 1)
    }
}


/// Mask representing active channels in a particular pattern or module.
#[derive(Clone, Copy, PartialEq)]
pub struct ActiveChannels(u64);

impl ActiveChannels {
    pub const fn all() -> ActiveChannels {
        ActiveChannels(u64::MAX)
    }

    pub const fn empty() -> ActiveChannels {
        ActiveChannels(0)
    }

    pub fn new<C: Borrow<Channel>>(iter: impl IntoIterator<Item=C>) -> ActiveChannels {
        iter.into_iter()
            .map(|c| *c.borrow())
            .collect()
    }

    pub fn iter(self) -> impl Iterator<Item=Channel> {
        (0..=63)
            .filter(move |chan| (self.0 & (1u64 << chan)) != 0)
            .map(Channel::from_u8_index)
    }

    pub const fn count(self) -> usize {
        self.0.count_ones() as usize
    }
}

impl BitAnd<ActiveChannels> for ActiveChannels {
    type Output = ActiveChannels;
    fn bitand(self, rhs: ActiveChannels) -> Self::Output {
        ActiveChannels(self.0 & rhs.0)
    }
}

impl BitAndAssign for ActiveChannels {
    fn bitand_assign(&mut self, rhs: ActiveChannels) {
        *self = *self & rhs;
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
