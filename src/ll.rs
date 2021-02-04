use bitflags::bitflags;
use std::fmt::{self, Write};

#[derive(Clone, Copy, Debug)]
pub struct InstrumentHeader {
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
    /// simplified loading. We [OpenMPT] use them for some hacks.
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

bitflags! {
    struct EnvelopeFlags: u8 {
        /// Envelope on/off, 1 = on, 0 = off
        const ENABLED = 1 << 0;

        /// Loop on/off, 1 = on, 0 = off
        const LOOP = 1 << 1;

        /// SusLoop on/off, 1 = on, 0 = off
        const SUSTAIN = 1 << 2;

        // These are not mentioned in ITTECH.TXT and there are no comment in OpenMPT, documentation
        // here is just my assumption.
        //
        // I assume CARRY is for the carry button in Instrument configuration in OpenMPT.
        const CARRY = 1 << 3;
        // Filter is probably only useful on a pitch envelope and makes it act like a filter
        // envelope instead.
        const FILTER = 1 << 7;
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Node {
    pub value: i8,
    pub tick: u16,
}

#[allow(non_snake_case)]
#[derive(Clone, Copy, Debug)]
pub struct SampleHeader {
    /// Sample Name, null-terminated (but may also contain nulls)
    pub name: Name,
    /// DOS Filename, null-terminated
    pub filename: DOSFilename,
    /// Global Volume
    pub gvl: u8,
    /// combined Sample Flags and Sample Import Format
    pub flags: SampleFlags,
    /// Default Volume
    pub vol: u8,
    /// Sample Panning
    pub dfp: u8,
    /// Sample Length (in samples)
    pub length: u32,
    /// Sample Loop Begin (in samples)
    pub loopbegin: u32,
    /// Sample Loop End (in samples)
    pub loopend: u32,
    /// C-5 frequency
    pub samplerate_c5: u32,
    /// Sample Sustain Begin (in samples)
    pub susloopbegin: u32,
    /// Sample Sustain End (in samples)
    pub susloopend: u32,
    /// Pointer to sample data
    pub samplepointer: u32,
    /// Auto-Vibrato Rate (called Sweep in IT)
    pub vibrato_speed: u8,
    /// Auto-Vibrato Depth
    pub vibrato_depth: u8,
    /// Auto-Vibrato Sweep (called Rate in IT)
    pub vibrato_rate: u8,
    /// Auto-Vibrato Type
    pub vibrato_type: u8,
}

bitflags! {
    pub struct SampleFlags: u16 {
        // Originally `flags` field.

        /// On = sample associated with header.
        const DATA_PRESENT = 1 << 0;

        /// On = 16 bit, Off = 8 bit.
        const DATA_16BIT = 1 << 1;

        /// On = stereo, Off = mono. Stereo samples not supported yet
        const STEREO = 1 << 2;

        /// On = compressed samples.
        const COMPRESSED = 1 << 3;

        /// On = Use loop
        const LOOP = 1 << 4;

        /// On = Use sustain loop
        const SUSTAIN = 1 << 5;

        /// On = Ping Pong loop, Off = Forwards loop
        const BIDI_LOOP = 1 << 6;

        /// On = Ping Pong Sustain loop, Off = Forwards Sustain loop
        const BIDI_SUSTAIN = 1 << 7;

        // Originally `cvt` field (shifted by additional 8 bits).
        //
        // From ITTECH.TXT:
        // > Convert - bits other than bit 0 are used internally for the loading of alternative
        // > formats.

        /// Off: Samples are unsigned   } IT 2.01 and below use unsigned samples
        ///  On: Samples are signed     } IT 2.02 and above use signed samples
        const DATA_SIGNED = 1 << 0 + 8;

        // From OpenMPT:
        // > ITTECH.TXT says these convert flags are "safe to ignore".
        // > IT doesn't ignore them, though, so why should we? :)

        /// Off: Intel lo-hi byte order for 16-bit samples
        ///  On: Motorola hi-lo byte order for 16-bit samples
        const DATA_BIG_ENDIAN = 1 << 1 + 8;
        /// Off: Samples are stored as PCM values
        ///  On: Samples are stored as Delta values
        const DELTA = 1 << 2 + 8;
        /// On: Samples are stored as byte delta values (for PTM loader)
        const PTM8_TO_16 = 1 << 3 + 8;

        // These seem to be missing from OpenMPT codebase, which hopefully means they're safe to
        // ignore.
        /// On: Samples are stored as TX-Wave 12-bit values
        const TX_WAVE = 1 << 4 + 8;
        /// On: Left/Right/All Stereo prompt
        const STEREO_PROMPT = 1 << 5 + 8;

        // These are OpenMPT extensions, ITTECH.TXT lists them as "Reserved"
        /// cvtOPLInstrument -- FM instrument in MPTM
        const OPL_INSTRUMENT = 1 << 6 + 8;
        /// cvtExternalSample -- Keep MPTM sample on disk
        const EXTERNAL_SAMPLE = 1 << 7 + 8;

        // I'm not sure what does this mean yet, but there is a frown next to it in the OpenMPT
        // codebase so I assume we don't support it, or something.
        /// cvtADPCMSample - MODPlugin :(
        const ADPCM_SAMPLE = 0xFF << 8; // MODPlugin :(
    }
}

impl SampleFlags {
    pub fn from_parts(flags: u8, cvt: u8) -> SampleFlags {
        let bits = (flags as u16) | ((cvt as u16) << 8);
        SampleFlags::from_bits_truncate(bits)
    }
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
