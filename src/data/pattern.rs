use super::{ActiveChannels, Channel, RangedU8};
use std::convert::{TryFrom, TryInto};
use std::fmt::{self, Debug, Display};
use std::num::TryFromIntError;
use std::str;

#[derive(Debug)]
pub struct Pattern {
    pub active_channels: ActiveChannels,
    pub rows: Vec<Vec<Command>>,
}

#[derive(Clone, Debug)]
pub struct Command {
    pub channel: Channel,
    pub note: Option<NoteCmd>,
    pub instrument: Option<u8>,
    pub volume: Option<VolumeCmd>,
    pub command: Option<(u8, u8)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum NoteCmd {
    Tone(u8),
    Off,
    Cut,
    Fade,
}

#[derive(Clone, Copy, Debug)]
pub enum VolumeCmd {
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

#[derive(Clone, Copy)]
pub struct Note(RangedU8<0, 119>);

impl TryFrom<u8> for Note {
    type Error = TryFromIntError;
    fn try_from(raw: u8) -> Result<Self, Self::Error> {
        Ok(Note(raw.try_into()?))
    }
}

impl From<Note> for u8 {
    fn from(note: Note) -> u8 {
        note.0.as_u8()
    }
}

fn note_string(note: Note) -> [u8; 3] {
    const NAMES: [&[u8; 2]; 12] = [b"C-", b"C#", b"D-", b"D#", b"E-", b"F-", b"F#", b"G-", b"G#", b"A-", b"A#", b"B-"];
    let note = note.0.as_u8();
    let name = NAMES[(note % 12) as usize];
    let octave = b'0' + (note / 12);
    [name[0], name[1], octave]
}

impl Debug for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(unsafe { str::from_utf8_unchecked(&note_string(*self)) })
    }
}

impl Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(unsafe { str::from_utf8_unchecked(&note_string(*self)) })
    }
}
