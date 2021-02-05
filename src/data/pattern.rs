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
    pub instrument: Option<InstrumentId>,
    pub volume: Option<(VolumeCmd, u8)>,
    pub effect: Option<(EffectCmd, u8)>,
}

#[derive(Clone, Copy, Debug)]
pub enum NoteCmd {
    Play(Note),
    Off,
    Cut,
    Fade,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Note(RangedU8<0, 119>);

pub type InstrumentId = RangedU8<0, 98>;

#[derive(Clone, Copy, Debug)]
pub enum VolumeCmd {
    SetVolume,
    Panning,
    FineVolumeUp,
    FineVolumeDown,
    VolumeSlideUp,
    VolumeSlideDown,
    PitchSlideDown,
    PitchSlideUp,
    PortamentoTo,
    Vibrato,
}

#[derive(Clone, Copy, Debug)]
pub enum EffectCmd {
	Arpeggio,
	PortamentoUp,
	PortamentoDown,
	TonePortamento,
	Vibrato,
	TonePortaVol,
	VibratoVol,
	Tremolo,
	Panning8,
	Offset,
	VolumeSlide,
	PositionJump,
	Volume,
	PatternBreak,
	Retrig,
	Speed,
	Tempo,
	Tremor,
	MODCMDEX,
	S3MCMDEX,
	ChannelVolume,
	ChannelVolSlide,
	GlobalVolume,
	GlobalVolslide,
	KeyOff,
	FineVibrato,
	Panbrello,
	XFinePortaUpDown,
	PanningSlide,
	SetEnvPosition,
	MIDI,
	SmoothMIDI,
	DelayCut,
	XParam,
	NoteSlideUp,         // IMF Gxy / PTM Jxy (Slide y notes up every x ticks)
	NoteSlideDown,       // IMF Hxy / PTM Kxy (Slide y notes down every x ticks)
	NoteSlideUpRetrig,   // PTM Lxy (Slide y notes up every x ticks + retrigger note)
	NoteSlideDownRetrig, // PTM Mxy (Slide y notes down every x ticks + retrigger note)
	ReverseOffset,       // PTM Nxx Revert sample + offset
	DBMEcho,             // DBM enable/disable echo
	OffsetPercentage,    // PLM Percentage Offset
}


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
        f.write_str(str::from_utf8(&note_string(*self)).unwrap())
    }
}

impl Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(str::from_utf8(&note_string(*self)).unwrap())
    }
}
