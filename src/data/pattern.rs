use super::{ActiveChannels, Channel, RangedU8};
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display};
use std::num::TryFromIntError;
use std::str;


#[derive(Clone, Debug)]
pub struct Pattern {
    pub active_channels: ActiveChannels,
    pub rows: Vec<Vec<Command>>,
}

#[derive(Clone, Debug)]
pub struct Command {
    pub channel: Channel,
    pub note: Option<NoteCmd>,
    pub instrument: Option<InstrumentId>,
    pub volume: Option<VolumeCmd>,
    pub effect: Option<EffectCmd>,
}

#[derive(Clone, Copy, Debug)]
pub enum NoteCmd {
    Play(Note),
    Off,
    Cut,
    Fade,
}

#[derive(Clone, Copy)]
pub struct Note(u8);

ranged_u8_newtype!(InstrumentId, 0..=98);

#[derive(Clone, Copy, Debug)]
pub enum VolumeCmd {
    /// v?? Set volume
    SetVolume(RangedU8<0, 64>),

    /// p?? Set panning
    Panning(RangedU8<0, 64>),

    /// a?? Fine volume up
    FineVolumeUp(RangedU8<0, 9>),

    /// b?? Fine volume down
    FineVolumeDown(RangedU8<0, 9>),

    /// c?? Volume slide up
    VolumeSlideUp(RangedU8<0, 9>),

    /// d?? Volume slide down
    VolumeSlideDown(RangedU8<0, 9>),

    /// e?? Pitch slide down
    PitchSlideDown(RangedU8<0, 9>),

    /// f?? Pitch slide up
    PitchSlideUp(RangedU8<0, 9>),

    /// g?? Portamento to next note speed
    Portamento(RangedU8<0, 9>),

    /// h?? Vibrato depth
    Vibrato(RangedU8<0, 9>),
}

/// Effect column commands
///
/// Documentation for these is taken from the Schism Tracker help text.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectCmd {
    /// Axx Set song speed (hex)
    SetSpeed(u8),

    /// Bxx Jump to Order (hex)
    JumpOrder(u8),

    /// Cxx Break to row xx (hex) of next pattern
    BreakRow(u8),

    /// Dxx ...
    VolumeSlide(VolumeSlide),

    /// Exx ...
    PitchSlideDown(PitchSlideDown),

    /// Fxx ...
    PitchSlideUp(PitchSlideUp),

    /// Gxx Slide to note with speed xx
    SlideToNote(u8),

    /// Hxy Vibrato with speed x, depth y
    Vibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Ixy Tremor with ontime x and offtime y
    Tremor(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Jxy Arpeggio with halftones x and y
    Arpeggio(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Kxx Dual Command: H00 & Dxx
    // TODO what is this ???
    Kxx(u8),

    /// Lxx Dual Command: G00 & Dxx
    // TODO ???
    Lxx(u8),

    /// Mxx Set channel volume to xx (0->40h)
    SetChannelVolume(u8),

    /// Nxx ...
    ChannelVolumeSlide(ChannelVolumeSlide),

    /// Oxx Set sample offset to yxx00h, y set with SAy
    /// & SAy Set high value of sample offset yxx00h
    SetSampleOffset(SetSampleOffset),

    /// Pxx ...
    PanningSlide(PanningSlide),

    /// Qxy Retrigger note every y ticks with volume modifier x
    ///   Values for x:
    ///     0: No volume change         8: Not used
    ///     1: -1                       9: +1
    ///     2: -2                       A: +2
    ///     3: -4                       B: +4
    ///     4: -8                       C: +8
    ///     5: -16                      D: +16
    ///     6: *2/3                     E: *3/2
    ///     7: *1/2                     F: *2
    // TODO a reasonable way to express the disjoint interval for `x` in the type?
    Retrigger(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Rxy Tremolo with speed x, depth y
    Tremolo(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// S0x Set filter
    /// S1x Set glissando control
    /// S2x Set finetune
    /// S3x Set vibrato waveform to type x
    /// S4x Set tremolo waveform to type x
    /// S5x Set panbrello waveform to type x
    ///   Waveforms for commands S3x, S4x and S5x:
    ///     0: Sine wave
    ///     1: Ramp down
    ///     2: Square wave
    ///     3: Random wave
    /// S6x Pattern delay for x ticks
    /// S70 Past note cut
    /// S71 Past note off
    /// S72 Past note fade
    /// S73 Set NNA to note cut
    /// S74 Set NNA to continue
    /// S75 Set NNA to note off
    /// S76 Set NNA to note fade
    /// S77 Turn off volume envelope
    /// S78 Turn on volume envelope
    /// S79 Turn off panning envelope
    /// S7A Turn on panning envelope
    /// S7B Turn off pitch envelope
    /// S7C Turn on pitch envelope
    /// S8x Set panning position
    /// S91 Set surround sound
    /// S99 Toggle duck modulator
    Todo1,

    /// SAy Set high value of sample offset yxx00h
    Todo2,
    // NOTE This one is already accounted for together with Oxx, so leave it out of the Sxx
    // SetSampleOffsetHigh(RangedU8<0, 0x0F>),

    /// SB0 Set loopback point
    /// SBx Loop x times to loopback point
    /// SCx Note cut after x ticks
    /// SDx Note delay for x ticks
    /// SEx Pattern delay for x rows
    /// SFx Set parameterised MIDI Macro
    Todo3,

    /// Txx ...
    Tempo(Tempo),

    /// Uxy Fine vibrato with speed x, depth y
    FineVibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Vxx Set global volume to xx (0->80h)
    SetGlobalVolume(RangedU8<0, 0x80>),

    /// Wxx ...
    GlobalVolumeSlide(GlobalVolumeSlide),

    /// Xxx Set panning position (0->0FFh)
    SetPanningPosition(u8),

    /// Yxy Panbrello with speed x, depth y
    Panbrello(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// Zxx MIDI Macros
    MIDI(u8),
}

/// Effect Dxx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VolumeSlide {
    /// D00
    // TODO figure out what it does in OpenMPT (my guess is it acts as no effect at all)
    Continue,

    /// D0x Volume slide down by x
    Down(RangedU8<1, 0x0F>),

    /// Dx0 Volume slide up by x
    Up(RangedU8<1, 0x0F>),

    /// DFx Fine volume slide down by x
    FineDown(RangedU8<1, 0x0F>),

    /// DxF Fine volume slide up by x
    FineUp(RangedU8<1, 0x0E>),

    /// Dxx Catchall when the other variants don't make sense.
    Other(u8),
}

/// Effect Exx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PitchSlideDown {
    /// Exx Pitch slide down by xx
    // 0xDF is the highest parameter value not matched by the following two effects, therefore I
    // assume it's divided up into ranges.
    Coarse(RangedU8<0, 0xDF>),

    /// EFx Fine pitch slide down by x
    Fine(RangedU8<0, 0x0F>),

    /// EEx Extra fine pitch slide down by x
    ExtraFine(RangedU8<0, 0x0F>),
}

/// Effect Fxx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PitchSlideUp {
    /// Fxx Pitch slide up by xx
    // 0xDF is the highest parameter value not matched by the following two effects, therefore I
    // assume it's divided up into ranges.
    Coarse(RangedU8<0, 0xDF>),

    /// FFx Fine pitch slide up by x
    Fine(RangedU8<0, 0x0F>),

    /// FEx Extra fine pitch slide up by x
    ExtraFine(RangedU8<0, 0x0F>),
}

/// Effect Nxx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelVolumeSlide {
    /// N00
    // TODO figure out what it does in OpenMPT
    Continue,

    /// N0x Channel volume slide down by x
    Down(RangedU8<1, 0x0F>),

    /// Nx0 Channel volume slide up by x
    Up(RangedU8<1, 0x0F>),

    /// NFx Fine channel volume slide down by x
    FineDown(RangedU8<1, 0x0F>),

    /// NxF Fine channel volume slide up by x
    FineUp(RangedU8<1, 0x0E>),

    /// Nxx Catchall when the other variants don't make sense.
    Other(u8),
}

/// Effect Oxx and SAy combined
///
/// Because they're doing part of the same thing and this will just make it nicer to use,
/// hopefully.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SetSampleOffset {
    Low(u8),
    High(RangedU8<0, 0x0F>),
}

/// Effect Pxx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PanningSlide {
    /// P00
    // TODO figure out what it does in OpenMPT
    Continue,

    /// P0x Panning slide to right by x
    Right(RangedU8<1, 0x0F>),

    /// Px0 Panning slide to left by x
    Left(RangedU8<1, 0x0F>),

    /// PFx Fine panning slide to right by x
    FineRight(RangedU8<0, 0x0F>),

    /// PxF Fine panning slide to left by x
    FineLeft(RangedU8<0, 0x0F>),

    /// Pxx Catchall when the other variants don't make sense.
    Other(u8),
}

// TODO Effect Sxx ...

/// Effect Txx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Tempo {
    /// T0x Tempo slide down by x
    SlideDown(RangedU8<0, 0x0F>),

    /// T1x Tempo slide up by x
    SlideUp(RangedU8<0, 0x0F>),

    /// Txx Set Tempo to xx (20h->0FFh)
    Set(RangedU8<0x20, 0xFF>),
}

/// Effect Wxx ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GlobalVolumeSlide {
    /// W00
    // TODO figure out what it does in OpenMPT
    Continue,

    /// W0x Global volume slide down by x
    Down(RangedU8<1, 0x0F>),

    /// Wx0 Global volume slide up by x
    Up(RangedU8<1, 0x0F>),

    /// WFx Fine global volume slide down by x
    FineDown(RangedU8<1, 0x0F>),

    /// WxF Fine global volume slide up by x
    FineUp(RangedU8<1, 0x0E>),

    /// Wxx Catchall when the other variants don't make sense.
    Other(u8),
}


impl TryFrom<u8> for Note {
    type Error = TryFromIntError;
    fn try_from(raw: u8) -> Result<Self, Self::Error> {
        if (0..=119).contains(&raw) {
            Ok(Note(raw))
        } else {
            // There is no public constructor for `TryFromIntError` so we obtain it through a
            // definitely-out-of-range cast ... :/
            Err(u8::try_from(u16::MAX).unwrap_err())
        }
    }
}

impl From<Note> for u8 {
    fn from(note: Note) -> u8 {
        note.0
    }
}

fn note_string(Note(idx): Note) -> [u8; 3] {
    const NAMES: [&[u8; 2]; 12] = [b"C-", b"C#", b"D-", b"D#", b"E-", b"F-", b"F#", b"G-", b"G#", b"A-", b"A#", b"B-"];
    let name = NAMES[(idx % 12) as usize];
    let octave = b'0' + (idx / 12);
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

impl Note {
    pub fn freq(&self) -> f32 {
        let (Note(idx), Note(base)) = (*self, Note::A_4);
        let exp = ((idx as f32) - (base as f32)) / 12.0f32;
        440.0f32 * 2.0f32.powf(exp)
    }
}

/// Helper macro for defining all the notes as associated constants.
macro_rules! define_notes {
    // "User" API
    ( $( $notes: ident ),* $(,)? ) => {
        #[allow(non_upper_case_globals)]
        impl Note {
            define_notes!( @(0) $( $notes, )* );
        }
    };

    // Base-case, empty list.
    ( @($_: expr) ) => {};
    // Recursive-case, pops one note from the list and adds one to the accumulator expression.
    ( @($acc: expr) $note: ident, $( $notes: ident ),* $(,)? ) => {
        pub const $note: Note = Note($acc);
        define_notes!( @($acc + 1) $( $notes, )* );
    };
}

define_notes! {
    C_0, Cs0, D_0, Ds0, E_0, F_0, Fs0, G_0, Gs0, A_0, As0, B_0,
    C_1, Cs1, D_1, Ds1, E_1, F_1, Fs1, G_1, Gs1, A_1, As1, B_1,
    C_2, Cs2, D_2, Ds2, E_2, F_2, Fs2, G_2, Gs2, A_2, As2, B_2,
    C_3, Cs3, D_3, Ds3, E_3, F_3, Fs3, G_3, Gs3, A_3, As3, B_3,
    C_4, Cs4, D_4, Ds4, E_4, F_4, Fs4, G_4, Gs4, A_4, As4, B_4,
    C_5, Cs5, D_5, Ds5, E_5, F_5, Fs5, G_5, Gs5, A_5, As5, B_5,
    C_6, Cs6, D_6, Ds6, E_6, F_6, Fs6, G_6, Gs6, A_6, As6, B_6,
    C_7, Cs7, D_7, Ds7, E_7, F_7, Fs7, G_7, Gs7, A_7, As7, B_7,
    C_8, Cs8, D_8, Ds8, E_8, F_8, Fs8, G_8, Gs8, A_8, As8, B_8,
    C_9, Cs9, D_9, Ds9, E_9, F_9, Fs9, G_9, Gs9, A_9, As9, B_9,
}
