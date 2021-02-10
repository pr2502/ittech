use super::*;


#[derive(Clone, Debug)]
pub struct Sample {
    /// Sample Name, null-terminated (but may also contain nulls)
    pub name: Name,

    /// DOS Filename, null-terminated
    pub filename: DOSFilename,

    /// Global Volume
    pub global_volume: u8,

    /// Default Volume
    pub default_volume: u8,

    /// Default Panning
    pub default_panning: u8,

    /// Loop after the note has been released (Off ==) command, or directly after reaching the end
    /// point if the sustain loop is off.
    pub loop_: Option<SampleLoop>,

    /// Loop after reching the end point while holding the note.
    pub sustain_loop: Option<SampleLoop>,

    /// C-5 playback frequency.
    ///
    /// If set to the native sampling rate of the sound sample playing the sample at C-5 will play
    /// it back unchanged.
    pub samplerate_c5: u32,

    /// Auto-Vibrato Rate (called Sweep in IT)
    pub vibrato_speed: u8,

    /// Auto-Vibrato Depth
    pub vibrato_depth: u8,

    /// Auto-Vibrato Sweep (called Rate in IT)
    pub vibrato_rate: u8,

    /// Auto-Vibrato Type
    pub vibrato_type: u8,

    /// Sample samples converted to a normalized `f32` representation (values from -1.0 to 1.0)
    pub data: Option<Vec<f32>>,
}

pub(crate) struct SampleHeader {
    pub name: Name,
    pub filename: DOSFilename,
    pub global_volume: u8,
    pub default_volume: u8,
    pub default_panning: u8,
    pub loop_: Option<SampleLoop>,
    pub sustain_loop: Option<SampleLoop>,
    pub samplerate_c5: u32,
    pub vibrato_speed: u8,
    pub vibrato_depth: u8,
    pub vibrato_rate: u8,
    pub vibrato_type: u8,

    pub flags: SampleFlags,
    pub data_offset: u32,
    pub data_length: u32,
}

#[derive(Clone, Debug)]
pub struct SampleLoop {
    /// Start - offset into the sample in samples
    pub start: u32,

    /// End - offset into the sample in samples.
    ///
    /// Must be always `>= start`
    pub end: u32,

    /// Bidirectional loop (also ping-pong loop)
    ///
    /// * **false** after reading the sample at `end` offset the playback head jumps to the `start`
    ///   offset.
    /// * **true** after reading the sample at `end` offset the playback reverses and continues with
    ///   the previous sample until it reaches the `start` offset, then it changes direction to
    ///   normal.
    pub bidi: bool,
}

bitflags! {
    pub(crate) struct SampleFlags: u16 {
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
        const DATA_SIGNED = 1 << (0 + 8);

        // From OpenMPT:
        // > ITTECH.TXT says these convert flags are "safe to ignore".
        // > IT doesn't ignore them, though, so why should we? :)

        /// Off: Intel lo-hi byte order for 16-bit samples
        ///  On: Motorola hi-lo byte order for 16-bit samples
        const DATA_BIG_ENDIAN = 1 << (1 + 8);
        /// Off: Samples are stored as PCM values
        ///  On: Samples are stored as Delta values
        const DELTA = 1 << (2 + 8);
        /// On: Samples are stored as byte delta values (for PTM loader)
        const PTM8_TO_16 = 1 << (3 + 8);

        // These seem to be missing from OpenMPT codebase, which hopefully means they're safe to
        // ignore.
        /// On: Samples are stored as TX-Wave 12-bit values
        const TX_WAVE = 1 << (4 + 8);
        /// On: Left/Right/All Stereo prompt
        const STEREO_PROMPT = 1 << (5 + 8);

        // These are OpenMPT extensions, ITTECH.TXT lists them as "Reserved"
        /// FM instrument in MPTM
        const OPL_INSTRUMENT = 1 << (6 + 8);
        /// Keep MPTM sample on disk
        const EXTERNAL_SAMPLE = 1 << (7 + 8);

        // I'm not sure what does this mean yet, but there is a frown next to it in the OpenMPT
        // codebase so I assume we don't support it or something.
        /// MODPlugin :(
        const ADPCM_SAMPLE = 0xFF << 8;
    }
}

impl SampleFlags {
    pub fn from_parts(flags: u8, cvt: u8) -> SampleFlags {
        let bits = (flags as u16) | ((cvt as u16) << 8);
        SampleFlags::from_bits_truncate(bits)
    }
}
