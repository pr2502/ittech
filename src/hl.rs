//! High-level representation of the IT file contents.

use crate::ll;

#[derive(Debug)]
pub struct IT {
    pub name: ll::Name,
    pub highlight: (u8, u8),
    pub made_with_version: u16,
    pub compatible_with_version: u16,
    pub flags: u16,
    pub special: u16,
    pub global_volume: u8,
    pub sample_volume: u8,
    pub speed: u8,
    pub tempo: u8,
    pub pan_separation: u8,
    pub pitch_wheel_depth: u8,
    pub message: String,
    pub orders: Vec<Order>,
    pub init_channel_panning: [u8; 64],
    pub init_channel_volume: [u8; 64],
    pub instruments: Vec<Instrument>,
    pub samples: Vec<Sample>,
    pub patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub enum Order {
    Index(u8),
    Separator,
    EndOfSong,
}

#[derive(Debug)]
pub struct Instrument {
    pub name: ll::Name,
    pub filename: ll::DOSFilename,
    pub nna: u8,
    pub dct: u8,
    pub dca: u8,
    pub fadeout: u16,
    pub pps: i8,
    pub ppc: u8,
    pub gbv: u8,
    pub dfp: u8,
    pub rv: u8,
    pub rp: u8,
    pub trkver: u16,
    pub nos: u8,
    pub ifc: u8,
    pub ifr: u8,
    pub mch: u8,
    pub mpr: u8,
    pub mbank: [u8; 2],
    pub keyboard: [(u8, u8); 120],
    pub volenv: ll::Envelope,
    pub panenv: ll::Envelope,
    pub pitchenv: ll::Envelope,
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
