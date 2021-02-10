use super::*;
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display};
use std::num::TryFromIntError;
use std::str;


#[derive(Clone, Debug)]
pub struct Pattern {
    pub active_channels: ActiveChannels,
    pub rows: Vec<Row>,
}

#[derive(Clone)]
pub struct Row {
    map: Vec<(Channel, Command)>,
}

// TODO replace Row=Vec<Command> with Row=Map<Channel, Command> and remove channel from Command
// struct. implement this Map as a sorted Vec<(Channel, Command)> internally (don't expose this
// through the API). make the Map iterable in sorted order.

#[derive(Clone, Debug)]
pub struct Command {
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

/// Volume column commands
///
/// These commands are not parsed by nibbles and masks like [`EffectCmd`] but by ranges so the
/// `?xy` notation doesn't make sense here.
// Documentation for individual commands is adapted from OpenMPT UI.
#[derive(Clone, Copy, Debug)]
pub enum VolumeCmd {
    /// `v??` Set volume
    SetVolume(RangedU8<0, 64>),

    /// `p??` Set panning
    Panning(RangedU8<0, 64>),

    /// `a??` Fine volume up
    FineVolumeUp(RangedU8<0, 9>),

    /// `b??` Fine volume down
    FineVolumeDown(RangedU8<0, 9>),

    /// `c??` Volume slide up
    VolumeSlideUp(RangedU8<0, 9>),

    /// `d??` Volume slide down
    VolumeSlideDown(RangedU8<0, 9>),

    /// `e??` Pitch slide down
    PitchSlideDown(RangedU8<0, 9>),

    /// `f??` Pitch slide up
    PitchSlideUp(RangedU8<0, 9>),

    /// `g??` Portamento to next note speed
    Portamento(RangedU8<0, 9>),

    /// `h??` Vibrato depth
    Vibrato(RangedU8<0, 9>),
}

/// Effect column commands
///
/// Parameters are parsed by nibbles and masks on them. Each command has one byte of parameter data
/// to work with. `Axx` means the command `A` uses the whole byte as its argument, `Hxy` means the
/// command `H` uses the first nibble as parameter `x` and second nibble as parameter `y`.
///
/// Some commands like `D` use masks to further granularize the command, however some commands only
/// use a subset of the values `u8` can have. Currently handling of out-of-range values in this
/// crate is inconsistent, feel free to report a bug on inconsistency, ideally all values would be
/// parsable and interpretable in some way and with methods to canonicalize them.
///
/// ## Additional resources
/// - <https://wiki.openmpt.org/Manual:_Effect_Reference>
/// - <https://modarchive.org/forums/index.php?topic=2222.0>
// Documentation for these is adapted from the Schism Tracker help text and OpenMPT wiki.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectCmd {
    /// `Axx` Set Speed
    ///
    /// *no memory*
    ///
    /// Sets the module Speed (ticks per row).
    SetSpeed(u8),

    /// `Bxx` Jump to Order
    JumpOrder(u8),

    /// `Cxx` Break to row `xx` of next pattern
    BreakRow(u8),

    /// `Dxx` ...
    VolumeSlide(VolumeSlide),

    /// `Exx` ...
    PitchSlideDown(PitchSlideDown),

    /// `Fxx` ...
    PitchSlideUp(PitchSlideUp),

    /// `Gxx` Slide to note with speed `xx`
    Portamento(u8),

    /// `Hxy` Vibrato with speed `x`, depth `y`
    Vibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Ixy` Tremor with ontime `x` and offtime `y`
    Tremor(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Jxy` Arpeggio with halftones `x` and `y`
    Arpeggio(RangedU8<0, 0x0F>, Option<RangedU8<0, 0x0F>>),

    /// `Kxx` Dual Command: `H00` & `Dxx`
    VolumeSlideAndVibrato(VolumeSlide),

    /// `Lxx` Dual Command: `G00` & `Dxx`
    VolumeSlideAndPortamento(VolumeSlide),

    /// `Mxx` Set channel volume to `xx` (0->40h)
    // TODO limit range
    SetChannelVolume(u8),

    /// `Nxx` ...
    ChannelVolumeSlide(ChannelVolumeSlide),

    /// `Oxx` Set sample offset to `yxx00h`, `SAy` Set high value of sample offset `yxx00h`
    SetSampleOffset(SetSampleOffset),

    /// `Pxx` ...
    PanningSlide(PanningSlide),

    /// `Qxy` Retrigger note every `y` ticks with volume modifier `x`
    ///
    /// Values for `x`:
    /// ```txt
    ///     0: No volume change     8: Not used
    ///     1: -1                   9: +1
    ///     2: -2                   A: +2
    ///     3: -4                   B: +4
    ///     4: -8                   C: +8
    ///     5: -16                  D: +16
    ///     6: *2/3                 E: *3/2
    ///     7: *1/2                 F: *2
    /// ```
    // TODO a reasonable way to express the disjoint interval for `x` in the type?
    Retrigger(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Rxy` Tremolo with speed `x`, depth `y`
    Tremolo(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Sxx` ...
    Set(Set),

    /// `Txx` ...
    Tempo(Tempo),

    /// `Uxy` Fine vibrato with speed `x`, depth `y`
    FineVibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Vxx` Set global volume to `xx` (0->80h)
    SetGlobalVolume(RangedU8<0, 0x80>),

    /// `Wxx` ...
    GlobalVolumeSlide(GlobalVolumeSlide),

    /// `Xxx` Set panning position (0->0FFh)
    SetPanningPosition(u8),

    /// `Yxy` Panbrello with speed `x`, depth `y`
    Panbrello(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Zxx` MIDI Macros
    MIDI(u8),
}

/// TODO
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectType {
    GlobalTiming,
    GlobalPattern,
    Volume,
    Pitch,
    Panning,
    Misc,
}

/// Effect `Dxx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VolumeSlide {
    /// `D00` Reuse the previous value
    // TODO figure out which "previous values" are shared between which effects and what flags
    // affect it, there are some mentions in ITTECH.TXT but the naming is inconsistent.
    Continue,

    /// `D0x` Volume slide down by `x`
    Down(RangedU8<1, 0x0F>),

    /// `Dx0` Volume slide up by `x`
    Up(RangedU8<1, 0x0F>),

    /// `DFx` Fine volume slide down by `x`
    FineDown(RangedU8<1, 0x0F>),

    /// `DxF` Fine volume slide up by `x`
    FineUp(RangedU8<1, 0x0E>),

    /// `Dxx` Catchall when the other variants don't make sense.
    Other(u8),
}

/// Effect `Exx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PitchSlideDown {
    /// `Exx` Pitch slide down by `xx`
    // 0xDF is the highest parameter value not matched by the following two effects, therefore I
    // assume it's divided up into ranges.
    Coarse(RangedU8<0, 0xDF>),

    /// `EFx` Fine pitch slide down by `x`
    Fine(RangedU8<0, 0x0F>),

    /// `EEx` Extra fine pitch slide down by `x`
    ExtraFine(RangedU8<0, 0x0F>),
}

/// Effect `Fxx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PitchSlideUp {
    /// `Fxx` Pitch slide up by `xx`
    // 0xDF is the highest parameter value not matched by the following two effects, therefore I
    // assume it's divided up into ranges.
    Coarse(RangedU8<0, 0xDF>),

    /// `FFx` Fine pitch slide up by `x`
    Fine(RangedU8<0, 0x0F>),

    /// `FEx` Extra fine pitch slide up by `x`
    ExtraFine(RangedU8<0, 0x0F>),
}

/// Effect `Nxx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ChannelVolumeSlide {
    /// `N00` Reuse the previous value
    Continue,

    /// `N0x` Channel volume slide down by `x`
    Down(RangedU8<1, 0x0F>),

    /// `Nx0` Channel volume slide up by `x`
    Up(RangedU8<1, 0x0F>),

    /// `NFx` Fine channel volume slide down by `x`
    FineDown(RangedU8<1, 0x0F>),

    /// `NxF` Fine channel volume slide up by `x`
    FineUp(RangedU8<1, 0x0E>),

    /// `Nxx` Catchall when the other variants don't make sense.
    Other(u8),
}

/// Effects `Oxx` and `SAy` combined
///
/// Because they're doing part of the same thing and this will just make it nicer to use,
/// hopefully.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SetSampleOffset {
    Low(u8),
    High(RangedU8<0, 0x0F>),
}

/// Effect `Pxx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PanningSlide {
    /// `P00` Reuse the previous value
    Continue,

    /// `P0x` Panning slide to right by `x`
    Right(RangedU8<1, 0x0F>),

    /// `Px0` Panning slide to left by `x`
    Left(RangedU8<1, 0x0F>),

    /// `PFx` Fine panning slide to right by `x`
    FineRight(RangedU8<0, 0x0F>),

    /// `PxF` Fine panning slide to left by `x`
    FineLeft(RangedU8<0, 0x0F>),

    /// `Pxx` Catchall when the other variants don't make sense.
    Other(u8),
}

/// Effect `Sxx` ...
///
/// `SAy` is represented using [`SetSampleOffset::High`] in Rust.
///
/// All the `Sxx` commands share the same memory, this should include the `SAy` command.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Set {
    /// `S00` Continue
    ///
    /// ## Canonicalization
    /// Value of the second nibble is ignored, any value in range `S00..=S0F` will be parsed as
    /// this, but it will only be serialized as `S00`.
    Continue,

    // Schism Tracker and OpenMPT (IT Effects) documentation disagree on this one. OpenMPT says
    // `S00` recalls `Sxx` command memory but Schism Tracker says `S0x` sets filter, it is however
    // marked red which means (*TODO does it?*) it's not supported.
    //
    // /// `S0x` Set filter
    // Filter(RangedU8<1, 0x0F>),

    /// `S1x` Set glissando on/off
    ///
    /// Configures whether tone portamento effects slide by semitones or smoothly.
    /// - `S10` *disables* glissando, portamento slides smoothly.
    /// - `S11` *enables* glissando, portamento behaves like glissando and snaps to semitones.
    ///
    /// ## Canonicalization
    /// When the value `y` is more than `1` it gets converted to `true`.
    Glissando(bool),

    /// `S2x` Set finetune
    ///
    /// *Considered a legacy command.*
    ///
    /// Overrides the current sample's C-5 frequency with a MOD finetune value.
    ///
    /// TODO link to _what is_ MOD finetune value
    Finetune(RangedU8<0, 0x0F>),

    /// `S3x` Set vibrato waveform to type `x`
    VibratoWaveform(Waveform),

    /// `S4x` Set tremolo waveform to type `x`
    TremoloWaveform(Waveform),

    /// `S5x` Set panbrello waveform to type `x`
    PanbrelloWaveform(Waveform),

    /// `S6x` Pattern delay for `x` ticks
    PatternDelay(RangedU8<0, 0x0F>),

    /// `S70`, `S71`, `S72` Past note cut, off or fade
    PastNote(SetPastNote),

    /// `S73`, `S74`, `S75`, `S76` - Set NNA to note cut, continue, off or fade
    NewNoteAction(SetNewNoteAction),

    /// `S77`, `S78` Turn off/on volume envelope
    VolumeEnvelope(bool),

    /// `S79`, `S7A` Turn off/on panning envelope
    PanningEnvelope(bool),

    /// `S7B`, `S7C` Turn off/on pitch envelope
    PitchEnvelope(bool),

    /// `S8x` Set panning position to `x`
    Panning(RangedU8<0, 0x0F>),

    /// `S90`, `S91` Turn off/on surround sound
    ///
    /// Only `S91` (`Set::Surround(true)`) is supported in the original Impulse Tracker,
    /// other `S9x` commands are MPTM extensions.
    Surround(bool),

    /// `S98`, `S99` Turn off/on reverb
    ///
    /// *MPTM extension*
    Reverb(bool),

    // Collides with `Set::Reverb(true)`, this is from the Schism Tracker help but is listed
    // in red and marked as not implemented.
    //
    // /// `S99` Toggle duck modulator
    // ToggleDuckModulator,

    /// `S9A`, `S9B` Set Surround mode to Center or Quad
    ///
    /// *MPTM extension*
    SurroundMode(SurroundMode),

    /// `S9C`, `S9D` Set filter mode to Global or Local
    ///
    /// *MPTM extension*
    FilterMode(FilterMode),

    /// `S9E`, `S9F` Play Forward or Backward
    ///
    /// *MPTM extension*
    Direction(PlayDirection),

    // SAy is handled by [`SetSampleOffset`] together with the Oxx command.

    /// `SB0` Set loopback point
    LoopbackPoint,

    /// `SBx` Loop `x` times to loopback point
    LoopbackTimes(RangedU8<0x01, 0x0F>),

    /// `SCx` Note cut after `x` ticks
    NoteCut(RangedU8<0, 0x0F>),

    /// `SDx` Note delay for `x` ticks
    NoteDelay(RangedU8<0, 0x0F>),

    /// `SEx` Pattern delay for `x` rows
    PatternRowDelay(RangedU8<0, 0x0F>),

    /// `SFx` Set parameterised MIDI Macro
    MIDIParam(RangedU8<0, 0x0F>),
}

/// Waveforms for commands `S3x`, `S4x` and `S5x`
///
/// - __0__ - Sine wave
/// - __1__ - Ramp down
/// - __2__ - Square wave
/// - __3__ - Random wave
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Waveform {
    Sine,
    RampDown,
    Square,
    Random,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SetPastNote {
    Cut,
    Off,
    Fade,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SetNewNoteAction {
    Cut,
    Off,
    Fade,
    Continue,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SurroundMode {
    /// `S9A` Sets the surround mode to Center Surround for all channels.
    ///
    /// This is the default mode. The `S91` command will place the channel in the center of
    /// the rear channels. Any panning command will bring it back to the front channels.
    Center,

    /// `S9B` Sets the surround mode to Quad Surround for all channels.
    ///
    /// In this mode, panning commands can adjust the position of the rear channels. Switching
    /// between the front and rear channels can only be done by using the `S91` and `S90` commands.
    Quad,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FilterMode {
    /// `S9C` Sets filter mode to Global on all channels (Impulse Tracker behaviour).
    ///
    /// In this mode, when resonant filters are enabled with a `Zxx` effect, they will stay active
    /// until explicitly disabled by setting the cutoff frequency to the maximum (`Z7F`), and the
    /// resonance to the minimum (`Z80`).
    Global,

    /// `S9D` Sets filter mode to Local on all channels.
    ///
    /// In this mode, the resonant filter will only affect the current note and will revert when
    /// a new note is played.
    Local,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PlayDirection {
    /// `S9E` Forces the current sample to play forward.
    Forward,

    /// `S9F` Forces the current sample to play backward.
    Backward,
}

/// Effect `Txx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Tempo {
    /// `T0x` Tempo slide down by `x`
    SlideDown(RangedU8<0, 0x0F>),

    /// `T1x` Tempo slide up by `x`
    SlideUp(RangedU8<0, 0x0F>),

    /// `Txx` Set Tempo to `xx` (20h->0FFh)
    Set(RangedU8<0x20, 0xFF>),
}

/// Effect `Wxx` ...
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum GlobalVolumeSlide {
    /// `W00` Reuse the previous value
    Continue,

    /// `W0x` Global volume slide down by `x`
    Down(RangedU8<1, 0x0F>),

    /// `Wx0` Global volume slide up by `x`
    Up(RangedU8<1, 0x0F>),

    /// `WFx` Fine global volume slide down by `x`
    FineDown(RangedU8<1, 0x0F>),

    /// `WxF` Fine global volume slide up by `x`
    FineUp(RangedU8<1, 0x0E>),

    /// `Wxx` Catchall when the other variants don't make sense.
    Other(u8),
}


impl Row {
    pub const fn empty() -> Row {
        Row { map: Vec::new() }
    }

    pub(crate) fn from_vec(mut vec: Vec<(Channel, Command)>) -> Row {
        vec.sort_unstable_by_key(|(chan, _)| *chan);
        Row { map: vec }
    }

    pub fn iter(&self) -> impl Iterator<Item=(Channel, &Command)> + '_ {
        self.map
            .iter()
            .map(|(chan, command)| (*chan, command))
    }
}

impl Debug for Row {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(
                self.map
                    .iter()
                    .map(|pair| (&pair.0, &pair.1))
            )
            .finish()
    }
}

impl Get<Channel> for Row {
    type Output = Command;
    fn get(&self, index: Channel) -> Option<&Self::Output> {
        self.map
            .binary_search_by_key(&index, |(chan, _)| *chan)
            .ok()
            .map(|idx| &self.map[idx].1)
    }
}

impl_index_from_get!(Row, Channel);



impl TryFrom<u8> for Note {
    type Error = TryFromIntError;
    fn try_from(raw: u8) -> Result<Self, Self::Error> {
        if (0..=119).contains(&raw) {
            Ok(Note(raw))
        } else {
            // There is no public constructor for `TryFromIntError` so we obtain it through a
            // definitely-out-of-range cast ... :/
            //
            // TODO make a custom error for the ranged integers and replace this nonsense.
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
