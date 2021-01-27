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
    pub header: ll::InstrumentHeader,
}

#[derive(Debug)]
pub struct Sample {
    pub header: ll::SampleHeader,
}

#[derive(Debug)]
pub struct Pattern {
}
