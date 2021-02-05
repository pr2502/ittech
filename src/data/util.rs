use std::convert::TryFrom;
use std::fmt::{self, Write};
use std::num::TryFromIntError;


#[derive(Clone, Copy)]
pub struct Name {
    pub bytes: [u8; 26],
}

#[derive(Clone, Copy)]
pub struct DOSFilename {
    pub bytes: [u8; 13],
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
pub struct RangedU8<const LOW: u8, const HIGH: u8>(u8);


impl<const LOW: u8, const HIGH: u8> RangedU8<LOW, HIGH> {
    pub fn as_u8(self) -> u8 {
        self.0
    }
}

impl<const LOW: u8, const HIGH: u8> TryFrom<u8> for RangedU8<LOW, HIGH> {
    type Error = TryFromIntError;
    fn try_from(raw: u8) -> Result<Self, Self::Error> {
        if (LOW..=HIGH).contains(&raw) {
            Ok(RangedU8(raw))
        } else {
            // There is no public constructor for `TryFromIntError` so we obtain it through a
            // definitely-out-of-range cast ... :/
            Err(u8::try_from(u16::MAX).unwrap_err())
        }
    }
}

impl<const LOW: u8, const HIGH: u8> From<RangedU8<LOW, HIGH>> for u8 {
    fn from(ranged: RangedU8<LOW, HIGH>) -> u8 {
        ranged.as_u8()
    }
}

fn null_terminated(bytes: &[u8]) -> &[u8] {
    let null_pos = bytes.iter()
        .position(|&b| b == 0)
        .unwrap_or(bytes.len());
    &bytes[..null_pos]
}

fn debug_bytestring(bytes: &[u8], f: &mut fmt::Formatter) -> fmt::Result {
    f.write_char('"')?;
    for &byte in null_terminated(bytes) {
        if byte.is_ascii_graphic() || byte == b' ' {
            f.write_char(byte as char)?;
        } else {
            write!(f, "<0x{:02X}>", byte)?;
        }
    }
    f.write_char('"')
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        debug_bytestring(&self.bytes, f)
    }
}

impl fmt::Debug for DOSFilename {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        debug_bytestring(&self.bytes, f)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf8_lossy(null_terminated(&self.bytes)).fmt(f)
    }
}

impl fmt::Display for DOSFilename {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        String::from_utf8_lossy(null_terminated(&self.bytes)).fmt(f)
    }
}

impl<const LOW: u8, const HIGH: u8> fmt::Debug for RangedU8<LOW, HIGH> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_u8().fmt(f)
    }
}
