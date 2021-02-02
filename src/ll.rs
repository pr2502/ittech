//! Low-level structs for the parser.
//!
//! These are taken more or less verbatim from OpenMPT codebase and their layout in code relates
//! directly to the byte layout of the files. The intention is for documentation and not for
//! any parsing-by-transmute, so no `#[repr(C)]` here, all parsing is done using [`nom`].
//!
//! Structs in this module are do not contain all the necessary information to be used on their
//! own, use the data types from [`crate::hl`] module.

use std::fmt::{self, Write};

#[derive(Clone, Copy, Debug)]
pub struct ITFileHeader {
    // /// Magic Bytes (IMPM)
    // pub id: [u8; 4],
    /// Song Name, null-terminated (but may also contain nulls)
    pub songname: Name,
    /// Rows per Beat highlight
    pub highlight_minor: u8,
    /// Rows per Measure highlight
    pub highlight_major: u8,
    /// Number of Orders
    pub ordnum: u16,
    /// Number of Instruments
    pub insnum: u16,
    /// Number of Samples
    pub smpnum: u16,
    /// Number of Patterns
    pub patnum: u16,
    /// "Made With" Tracker
    pub cwtv: u16,
    /// "Compatible With" Tracker
    pub cmwt: u16,
    /// Header Flags
    pub flags: u16,
    /// Special Flags, for embedding extra information
    pub special: u16,
    /// Global Volume (0...128)
    pub globalvol: u8,
    /// Master Volume (0...128), referred to as Sample Volume in OpenMPT
    pub mv: u8,
    /// Initial Speed (1...255)
    pub speed: u8,
    /// Initial Tempo (31...255)
    pub tempo: u8,
    /// Pan Separation (0...128)
    pub sep: u8,
    /// Pitch Wheel Depth
    pub pwd: u8,
    /// Length of Song Message
    pub msglength: u16,
    /// Offset of Song Message in File (IT crops message after first null)
    pub msgoffset: u32,
    /// Some IT versions save an edit timer here. ChibiTracker writes "CHBI" here. OpenMPT writes
    /// "OMPT" here in some cases, see Load_it.cpp
    pub reserved: u32,
    /// Initial Channel Panning
    pub chnpan: [u8; 64],
    /// Initial Channel Volume
    pub chnvol: [u8; 64],
}

#[allow(non_upper_case_globals)]
impl ITFileHeader {
    pub const magic: [u8; 4] = *b"IMPM";
}

#[derive(Clone, Copy, Debug)]
pub struct InstrumentHeader {
    // /// Magic Bytes (IMPI)
    // pub id: [u8; 4],
    /// DOS Filename, null-terminated
    pub filename: DOSFilename,
    /// New Note Action
    pub nna: u8,
    /// Duplicate Note Check Type
    pub dct: u8,
    /// Duplicate Note Check Action
    pub dca: u8,
    /// Instrument Fadeout (0...256, although values up to 1024 would be sensible. Up to IT2.07,
    /// the limit was 0...128)
    pub fadeout: u16,
    /// Pitch/Pan Separatation
    pub pps: i8,
    /// Pitch/Pan Centre
    pub ppc: u8,
    /// Global Volume
    pub gbv: u8,
    /// Panning
    pub dfp: u8,
    /// Vol Swing
    pub rv: u8,
    /// Pan Swing
    pub rp: u8,
    /// Tracker ID
    pub trkver: u16,
    /// Number of embedded samples
    pub nos: u8,
    /// Reserved
    pub reserved1: u8,
    /// Instrument Name, null-terminated (but may also contain nulls)
    pub name: Name,
    /// Filter Cutoff
    pub ifc: u8,
    /// Filter Resonance
    pub ifr: u8,
    /// MIDI Channel
    pub mch: u8,
    /// MIDI Program
    pub mpr: u8,
    /// MIDI Bank
    pub mbank: [u8; 2],
    /// Sample / Transpose map
    pub keyboard: [(u8, u8); 120],
    /// Volume Envelope
    pub volenv: Envelope,
    /// Pan Envelope
    pub panenv: Envelope,
    /// Pitch / Filter Envelope
    pub pitchenv: Envelope,
    /// IT saves some additional padding bytes to match the size of the old instrument format for
    /// simplified loading. We use them for some hacks.
    pub dummy: [u8; 4],
}

#[allow(non_upper_case_globals)]
impl InstrumentHeader {
    pub const magic: [u8; 4] = *b"IMPI";

    pub const dfp_ignorePanning: u8   = 0x80;
    pub const ifc_enableCutoff: u8    = 0x80;
    pub const ifr_enableResonance: u8 = 0x80;

}

#[derive(Clone, Copy, Debug)]
pub struct Envelope {
    /// Envelope Flags
    pub flags: u8,
    /// Number of Envelope Nodes
    pub num: u8,
    /// Loop Start
    pub lpb: u8,
    /// Loop End
    pub lpe: u8,
    /// Sustain Start
    pub slb: u8,
    /// Sustain End
    pub sle: u8,
    /// Envelope Node Positions / Values
    pub data: [Node; 25],
    /// Reserved
    pub reserved: u8,
}

#[allow(non_upper_case_globals)]
impl Envelope {
    pub const flags_envEnabled: u8    = 0x01;
    pub const flags_envLoop: u8       = 0x02;
    pub const flags_envSustain: u8    = 0x04;
    pub const flags_envCarry: u8      = 0x08;
    pub const flags_envFilter: u8     = 0x80;
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Node {
    pub value: i8,
    pub tick: u16,
}

#[allow(non_snake_case)]
#[derive(Clone, Copy, Debug)]
pub struct SampleHeader {
    // /// Magic Bytes (IMPS)
    // pub id: [u8; 4],
    /// DOS Filename, null-terminated
    pub filename: DOSFilename,
    /// Global Volume
    pub gvl: u8,
    /// Sample Flags
    pub flags: u8,
    /// Default Volume
    pub vol: u8,
    /// Sample Name, null-terminated (but may also contain nulls)
    pub name: Name,
    /// Sample Import Format
    pub cvt: u8,
    /// Sample Panning
    pub dfp: u8,
    /// Sample Length (in samples)
    pub length: u32,
    /// Sample Loop Begin (in samples)
    pub loopbegin: u32,
    /// Sample Loop End (in samples)
    pub loopend: u32,
    /// C-5 frequency
    pub C5Speed: u32,
    /// Sample Sustain Begin (in samples)
    pub susloopbegin: u32,
    /// Sample Sustain End (in samples)
    pub susloopend: u32,
    /// Pointer to sample data
    pub samplepointer: u32,
    /// Auto-Vibrato Rate (called Sweep in IT)
    pub vis: u8,
    /// Auto-Vibrato Depth
    pub vid: u8,
    /// Auto-Vibrato Sweep (called Rate in IT)
    pub vir: u8,
    /// Auto-Vibrato Type
    pub vit: u8,
}

#[allow(non_upper_case_globals)]
impl SampleHeader {
    pub const magic: [u8; 4] = *b"IMPS";

    pub const flags_sampleDataPresent: u8 = 0x01;
    pub const flags_sample16Bit: u8       = 0x02;
    pub const flags_sampleStereo: u8      = 0x04;
    pub const flags_sampleCompressed: u8  = 0x08;
    pub const flags_sampleLoop: u8        = 0x10;
    pub const flags_sampleSustain: u8     = 0x20;
    pub const flags_sampleBidiLoop: u8    = 0x40;
    pub const flags_sampleBidiSustain: u8 = 0x80;

    pub const dfp_enablePanning: u8 = 0x80;

    pub const cvtSignedSample: u8   = 0x01;
    pub const cvtOPLInstrument: u8  = 0x40; // FM instrument in MPTM
    pub const cvtExternalSample: u8 = 0x80; // Keep MPTM sample on disk
    pub const cvtADPCMSample: u8    = 0xFF; // MODPlugin :(

    // ITTECH.TXT says these convert flags are "safe to ignore".
    // IT doesn't ignore them, though, so why should we? :)
    pub const cvtBigEndian: u8      = 0x02;
    pub const cvtDelta: u8          = 0x04;
    pub const cvtPTM8to16: u8       = 0x08;
}

#[derive(Clone, Copy)]
pub struct Name {
    pub bytes: [u8; 26],
}

#[derive(Clone, Copy)]
pub struct DOSFilename {
    pub bytes: [u8; 13],
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
