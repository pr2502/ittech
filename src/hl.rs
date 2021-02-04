use crate::ll;
use bitflags::bitflags;

#[derive(Debug)]
pub struct Module {
    /// Song Name, null-terminated (but may also contain nulls)
    pub name: ll::Name,
    /// Rows per Measure highlight, Rows per Beat highlight
    pub highlight: (u8, u8),
    /// "Made With" Tracker
    pub made_with_version: u16,
    /// "Compatible With" Tracker
    pub compatible_with_version: u16,
    /// combined Header Flags and Special Flags, for embedding extra information
    pub flags: ModuleFlags,
    /// Global Volume (0...128)
    pub global_volume: u8,
    /// Sample Volume (0...128)
    pub sample_volume: u8,
    /// Initial Speed (1...255)
    pub speed: u8,
    /// Initial Tempo (31...255)
    pub tempo: u8,
    /// Pan Separation (0...128)
    pub pan_separation: u8,
    /// Pitch Wheel Depth
    pub pitch_wheel_depth: u8,
    /// Comment message
    pub message: String,
    /// Orders
    pub orders: Vec<Order>,
    /// Initial Channel Panning
    pub init_channel_panning: [u8; 64],
    /// Initial Channel Volume
    pub init_channel_volume: [u8; 64],
    /// Instrument headers (without samples)
    pub instruments: Vec<ll::InstrumentHeader>,
    /// Samples
    pub samples: Vec<Sample>,
    /// Patterns
    pub patterns: Vec<Pattern>,
}

bitflags! {
    pub struct ModuleFlags: u32 {
        // Originally `flags` field. This field has 16 bits however only the lower 8 bits are
        // documented in ITTECH.TXT.

        /// On = Stereo, Off = Mono
        const STEREO = 1 << 0;

        /// If on, no mixing occurs if the volume at mixing time is 0 (redundant v1.04+)
        const VOL_0_MIX_OPTIMIZATIONS = 1 << 1;

        /// On = Use instruments, Off = Use samples
        const USE_INSTRUMENTS = 1 << 2;

        /// On = Linear slides, Off = Amiga slides
        const LINEAR_SLIDES = 1 << 3;

        /// On = Old Effects, Off = IT Effects
        /// Differences:
        /// - Vibrato is updated EVERY frame in IT mode, whereas it is updated every non-row frame
        ///   in other formats.  Also, it is two times deeper with Old Effects ON.
        /// - Command Oxx will set the sample offset to the END of a sample instead of ignoring the
        ///   command under old effects mode.
        /// - (More to come, probably)
        const OLD_EFFECTS = 1 << 4;

        /// On = Link Effect G's memory with Effect E/F. Also Gxx with an instrument present will
        /// cause the envelopes to be retriggered. If you change a sample on a row with Gxx, it'll
        /// adjust the frequency of the current note according to:
        ///
        /// NewFrequency = OldFrequency * NewC5 / OldC5;
        const LINK_G_E_EFFECTS = 1 << 5;

        /// Use MIDI pitch controller, Pitch depth given by PWD
        const USE_MIDI_PITCH = 1 << 6;

        /// Request embedded MIDI configuration
        /// (Coded this way to permit cross-version saving)
        const REQUEST_MIDI_CONFIG_EMBEDDED = 1 << 7;

        // Originally `special` field (shifted by additional 16 bits).

        /// On = song message attached.
        /// Song message:
        ///  Stored at offset given by "Message Offset" field.
        ///  Length = MsgLgth.
        ///  NewLine = 0Dh (13 dec)
        ///  EndOfMsg = 0
        ///
        /// Note: v1.04+ of IT may have song messages of up to
        ///       8000 bytes included.
        const MESSAGE_ATTACHED = 1 << 0 + 16;

        /// MIDI configuration embedded
        const MIDI_CONIFG_EMBEDDED = 1 << 3 + 16;
    }
}

impl ModuleFlags {
    pub fn from_parts(flags: u16, special: u16) -> ModuleFlags {
        let bits = (flags as u32) | ((special as u32) << 16);
        ModuleFlags::from_bits_truncate(bits)
    }
}

#[derive(Debug)]
pub struct Instrument {
    header: ll::InstrumentHeader,
    samples: Vec<Sample>,
}


#[derive(Debug)]
pub enum Order {
    Index(u8),
    Separator,
    EndOfSong,
}

#[derive(Debug)]
pub struct Sample {
    pub name: ll::Name,
    pub filename: ll::DOSFilename,
    pub samplerate_c5: u32,
    pub do_loop: bool,
    pub data: Option<Vec<f32>>,
}

#[derive(Debug)]
pub struct Pattern {
    pub rows: Vec<Vec<Command>>,
}

#[derive(Clone, Debug)]
pub struct Command {
    pub channel: u8,
    pub note: Option<Note>,
    pub instrument: Option<u8>,
    pub volume: Option<Volume>,
    pub command: Option<(u8, u8)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Note {
    Tone(u8),
    Off,
    Cut,
    Fade,
}

#[derive(Clone, Copy, Debug)]
pub enum Volume {
    SetVolume(u8),
    Panning(u8),
    FineVolumeUp(u8),
    FineVolumeDown(u8),
    VolumeSlideUp(u8),
    VolumeSlideDown(u8),
    PitchSlideDown(u8),
    PitchSlideUp(u8),
    PortamentoTo(u8),
    Vibrato(u8),
}
