use super::*;
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display};
use std::num::TryFromIntError;
use std::str;


/// Pattern grid
///
/// Patterns are represented as a grid of [`Command`]s, this grid is stored as a list of [`Row`]s.
#[derive(Clone, Debug)]
pub struct Pattern {
    pub active_channels: ActiveChannels,
    pub rows: Vec<Row>,
}

/// Pattern row
///
/// Row is represented by a sparse vector. It can be iterated or indexed by a [`Channel`].
#[derive(Clone)]
pub struct Row {
    map: Vec<(Channel, Command)>,
}

/// Pattern command
///
///
#[derive(Clone, Debug)]
pub struct Command {
    pub note: Option<NoteCmd>,
    pub instrument: Option<InstrumentId>,
    pub volume: Option<VolumeCmd>,
    pub effect: Option<EffectCmd>,
}

/// Note column commands
#[derive(Clone, Copy, Debug)]
pub enum NoteCmd {
    Play(Note),
    Off,
    Cut,
    Fade,
}

/// Note pitch representation
///
/// Ranges from C-0 to B-9, only exact pitches can be represented.
#[derive(Clone, Copy)]
pub struct Note(u8);

/// Volume column commands
///
/// All parameters are displayed in **decimal**.
#[derive(Clone, Copy, Debug)]
pub enum VolumeCmd {
    /// `vxx` Set volume
    ///
    /// Sets the current note volume to `xx`.
    SetVolume(RangedU8<0, 64>),

    /// `pxx` Set panning
    ///
    /// Sets the current channel's panning possition to `xx`.
    Panning(RangedU8<0, 64>),

    /// `a0x` Fine volume slide up
    ///
    /// Functions like `DxF` ([`VolumeSlide::FineUp`]]).
    ///
    /// Slides the volume up `x` units on the first tick.
    ///
    /// `None` uses the last value, memory is shared with all `Dxy` commands.
    FineVolumeUp(Option<RangedU8<1, 9>>),

    /// `b0x` Fine volume slide down
    ///
    /// Functions like `DFy` ([`VolumeSlide::FineDown`]]).
    ///
    /// Slides the volume down `x` units on the first tick.
    ///
    /// `None` uses the last value, memory is shared with all `Dxy` commands.
    FineVolumeDown(Option<RangedU8<1, 9>>),

    /// `c0x` Volume slide up
    ///
    /// Functions like `Dx0` ([`VolumeSlide::Up`]).
    ///
    /// Slides the volume up `x` units on all ticks except the first.
    ///
    /// `None` uses the last value, memory is shared with all `Dxy` commands.
    VolumeSlideUp(Option<RangedU8<1, 9>>),

    /// `d0x` Volume slide down
    ///
    /// Functions like `D0y` ([`VolumeSlide::Down`]).
    ///
    /// Slides the volume down `x` units on all ticks except the first.
    ///
    /// `None` uses the last value, memory is shared with all `Dxy` commands.
    VolumeSlideDown(Option<RangedU8<1, 9>>),

    /// `e0x` Portamento down
    ///
    /// Similar to `Exx` ([`EffectCmd::PortamentoDown`]).
    ///
    /// Compared to `Exx`, parameters are 4 times more coarse (e.g. `e01 == E04`).
    ///
    /// `None` uses the last value, memory is shared with `Exx`.
    PortamentoDown(Option<RangedU8<1, 9>>),

    /// `f0x` Portamento up
    ///
    /// Similar to `Fxx`.
    ///
    /// Compared to `Fxx`, parameters are 4 times more coarse (e.g. `f01 == F04`).
    ///
    /// `None` uses the last value, memory is shared with `Fxx`.
    PortamentoUp(Option<RangedU8<1, 9>>),

    /// `g0x` Portamento to next note speed
    ///
    /// Similar to `Gxx`.
    ///
    /// The paramets of `g0x` are mapped to paramets of `Gxx` using the following table.
    /// `0` and `00` are represented with `None`.
    /// ```txt
    ///   g0x   Gxx     g0x   Gxx
    ///     0    00       5    20
    ///     1    01       6    40
    ///     2    04       7    60
    ///     3    08       8    80
    ///     4    10       9    FF
    /// ```
    ///
    /// `None` uses the last value, memory is shared with `Gxx`.
    TonePortamento(Option<RangedU8<1, 9>>),

    /// `h0x` Vibrato depth
    ///
    /// Executes a vibrato with depth `x` and speed from the last `Hxy` or `Uxy` command.
    ///
    /// `None` uses the last value, memory is shared with `Hxy` and `Uxy`.
    Vibrato(Option<RangedU8<1, 9>>),
}

/// Effect column commands
///
/// All parameters are displayed in **hexadecimal**.
///
/// Parameters are parsed by nibbles and masks on them. Each command has one byte of parameter data
/// to work with. `Axx` means the command `A` uses the whole byte as its argument, `Hxy` means the
/// command `H` uses the first nibble as parameter `x` and second nibble as parameter `y`.
///
/// Some commands like `D` use masks to further granularize the command, however some commands only
/// use a subset of the values `u8` can have.
///
/// The parser should be handling of out-of-range values by canonicalizing them into some in range
/// value and this behaviour should be documented for each value in a "Canonicalization" section.
/// Parser should never crash because of an out-of-range value and the representation should not
/// allow for out-of-range values to get through.
///
/// ## Additional resources
/// - <https://wiki.openmpt.org/Manual:_Effect_Reference>
/// - <https://modarchive.org/forums/index.php?topic=2222.0>
// Documentation for these is adapted from the Schism Tracker help text and OpenMPT wiki.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectCmd {
    /// `Axx` Set Speed
    ///
    /// Sets the module Speed (ticks per row).
    ///
    /// ## Canonicalization
    /// The value `0` does nothing so the effect command get skipped by the parser.
    SetSpeed(RangedU8<1, 255>),

    /// `Bxx` Jump to Order
    ///
    /// Causes playback to jump to pattern position `xx`.
    ///
    /// `B00` would restart a song from the beginning (first pattern in the Order List).
    ///
    /// If `Cxx` is on the same row, the pattern specified by `Bxx` will be
    /// the pattern `Cxx` jumps into.
    JumpOrder(u8),

    /// `Cxx` Break to row `xx` of next pattern
    ///
    /// Jumps to row `xx` of the next pattern in the Order List.
    ///
    /// If the current pattern is the last pattern in the Order List, `Cxx` will jump to row `xx`
    /// of the first pattern.
    ///
    /// If `Bxx` is on the same row, the pattern specified by `Bxx` will be
    /// the pattern `Cxx` jumps into.
    ///
    /// Ranges from `0x00` to the next pattern's row length, higher values are to be treated as `0x00`.
    BreakRow(u8),

    /// `Dxx` Volume Slide or Fine Volume Slide
    ///
    /// Slides the current volume up or down.
    ///
    /// `None` uses the last value, memory is shared with `Hxy` and `Uxy`.
    VolumeSlide(Option<VolumeSlide>),

    /// `Exx`...
    ///
    /// `None` uses the last value, memory is shared with the `e0x` volume command.
    PortamentoDown(Option<Portamento>),

    /// `Fxx`...
    ///
    /// `None` uses the last value, memory is shared with the `f0x` volume command.
    PortamentoUp(Option<Portamento>),

    /// `Gxx` Slide to note with speed `xx`
    TonePortamento(Option<RangedU8<1, 0xFF>>),

    /// `Hxy` Vibrato with speed `x`, depth `y`
    Vibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Ixy` Tremor with ontime `x` and offtime `y`
    Tremor(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Jxy` Arpeggio with halftones `x` and `y`
    Arpeggio(RangedU8<0, 0x0F>, Option<RangedU8<0, 0x0F>>),

    /// `Kxx` Dual Command: `H00` & `Dxx`
    VolumeSlideAndVibrato(Option<VolumeSlide>),

    /// `Lxx` Dual Command: `G00` & `Dxx`
    VolumeSlideAndPortamento(Option<VolumeSlide>),

    /// `Mxx` Set channel volume to `xx`
    ///
    /// Sets the current channel volume, which multiplies all note volumes it encompasses.
    ///
    /// ## Canonicalization
    /// All values above `0x40` are clipped to `0x40`.
    SetChannelVolume(RangedU8<0, 0x40>),

    /// `Nxx` ...
    ChannelVolumeSlide(Option<VolumeSlide>),

    /// `Oxx` Set sample offset to `yxx00h`, `SAy` Set high value of sample offset `yxx00h`
    SetSampleOffset(SetSampleOffset),

    /// `Pxx` ...
    PanningSlide(Option<PanningSlide>),

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
    ///
    /// ## Canonicalization
    /// Value `8` is replaced by `0` during parsing.
    // TODO a reasonable way to express the disjoint interval for `x` in the type?
    //      maybe just use an enum..
    Retrigger(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Rxy` Tremolo with speed `x`, depth `y`
    Tremolo(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Sxx` ...
    ///
    /// `None` uses the last value, memory is shared by all `Sxx` subcommands.
    ///
    /// ## Canonicalization
    /// The second nibble is ignored when parsing the set subcommand, any value in range
    /// `S00..=S0F` will be parsed as `None`, but it will only be serialized as `S00`.
    Set(Option<Set>),

    /// `Txx` ...
    Tempo(Tempo),

    /// `Uxy` Fine vibrato with speed `x`, depth `y`
    FineVibrato(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Vxx` Set global volume to `xx`
    ///
    /// ## Canonicalization
    /// All values above `0x80` are clipped to `0x80`.
    SetGlobalVolume(RangedU8<0, 0x80>),

    /// `Wxx` ...
    GlobalVolumeSlide(Option<VolumeSlide>),

    /// `Xxx` Set panning position
    SetPanningPosition(u8),

    /// `Yxy` Panbrello with speed `x`, depth `y`
    Panbrello(RangedU8<0, 0x0F>, RangedU8<0, 0x0F>),

    /// `Zxx` MIDI Macros
    MIDI(u8),
}

/// Effect category
///
/// OpenMPT categorizes effects into groups which are then used in the UI for color highlighting.
/// These don't have any effect on parsing and are purely determined from the effect code.
///
/// Categorization is taken from OpenMPT wiki.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EffectCategory {
    GlobalTiming,
    GlobalPattern,
    Volume,
    Pitch,
    Panning,
    Misc,
}

/// Effects `Dxx`, `Kxx`, `Lxx`, `Nxx`, `Wxx` ...
///
/// All of these commands perform a volume slide but on different mixers.
///
/// - `Dxx`, `Kxx`, `Lxx` - note volume slide
/// - `Nxx` - channel volume slide
/// - `Wxx` - global volume slide
///
/// ## Canonicalization
/// Values where both nibbles are in `1..=0xE` at the same time don't have a defined meaning, these
/// get skipped by the parser.
///
/// TODO describe volume slide units
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum VolumeSlide {
    /// `D0x`, `K0x`, `L0x`, `N0x`, `W0x` Volume slide down by `x`
    ///
    /// Decreases mixer volume by `x` units on every tick of the row except the first.
    /// If `x` is `0xF`, volume decreases on every tick (including the first).
    Down(RangedU8<1, 0x0F>),

    /// `Dx0`, `Kx0`, `Lx0`, `Nx0`, `Wx0` Volume slide up by `x`
    ///
    /// Increases mixer volume by `x` units on every tick of the row except the first.
    /// Volume will not exceed `0x40`.
    Up(RangedU8<1, 0x0F>),

    /// `DFx`, `KFx`, `LFx`, `NFx`, `WFx` Fine volume slide down by `x`
    ///
    /// Finely decreases mixer volume by only applying `x` units on the first tick of the row.
    FineDown(RangedU8<1, 0x0E>),

    /// `DxF`, `KxF`, `LxF`, `NxF`, `WxF` Fine volume slide up by `x`
    ///
    /// Finely increases mixer volume by only applying `x` units on the first tick of the row.
    ///
    /// OpenMPT documents that this value cannot be `0xF` however both OpenMPT and Schism Tracker
    /// parse it this way so we allow it too.
    FineUp(RangedU8<1, 0x0F>),
}

/// Effects `Exx`, `Fxx` ...
///
/// Portamento down or up in coarse, fine or extra fine steps.
///
/// TODO describe frequency units
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Portamento {
    /// `Exx`, `Fxx` Pitch slide down/up by `xx`
    ///
    /// Decreases/increases current note pitch by `xx` units on every tick of the row except the
    /// first.
    // 0xDF is the highest parameter value not matched by the following two effects, therefore we
    // assume it's divided up into ranges.
    Coarse(RangedU8<1, 0xDF>),

    /// `EFx`, `FFx` Fine pitch slide down/up by `x`
    ///
    /// Finely decreases/increases note pitch by only applying `x` units on the first tick of the
    /// row.
    Fine(RangedU8<0, 0x0F>),

    /// `EEx`, `FEx` Extra fine pitch slide down/up by `x`
    ///
    /// Extra-finely decreases/increases note pitch by applying with 4 times the precision of
    /// `EFx`/`FFx` ([`Portamento::Fine`]).
    ExtraFine(RangedU8<0, 0x0F>),
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
///
/// ## Canonicalization
/// Values where both nibbles are in `1..=0xE` at the same time don't have a defined meaning, these
/// get skipped by the parser.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PanningSlide {
    /// `P0x` Panning slide to right by `x`
    Right(RangedU8<1, 0x0F>),

    /// `Px0` Panning slide to left by `x`
    Left(RangedU8<1, 0x0F>),

    /// `PFx` Fine panning slide to right by `x`
    FineRight(RangedU8<0, 0x0F>),

    /// `PxF` Fine panning slide to left by `x`
    FineLeft(RangedU8<0, 0x0F>),
}

/// Effect `Sxx` ...
///
/// `SAy` is represented using [`SetSampleOffset::High`] in Rust.
///
/// All the `Sxx` commands share the same memory, this should include the `SAy` command.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Set {
    // Schism Tracker and OpenMPT (IT Effects) documentation disagree on this one. OpenMPT says
    // `S00` recalls `Sxx` command memory but Schism Tracker says `S0x` sets filter, it is marked
    // in red which probably means we don't need to worry about it, at least for now.
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
    ///
    /// Sets the waveform of future [`Vibrato`](EffectCmd::Vibrato) effects.
    VibratoWaveform(Waveform),

    /// `S4x` Set tremolo waveform to type `x`
    ///
    /// Sets the waveform of future [`Tremolo`](EffectCmd::Tremolo) effects.
    TremoloWaveform(Waveform),

    /// `S5x` Set panbrello waveform to type `x`
    ///
    /// Sets the waveform of future [`Panbrello`](EffectCmd::Panbrello) effects.
    PanbrelloWaveform(Waveform),

    /// `S6x` Pattern delay for `x` ticks
    ///
    /// Extends the current row by `x` ticks. If multiple `S6x` commands are on the same row, the
    /// sum of their parameters is used.
    PatternTickDelay(RangedU8<0, 0x0F>),

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

/// Oscillator waveforms for commands `S3x`, `S4x` and `S5x`
///
/// IT retriggers the waveforms for each note - they start playing from their starting point when a
/// new note is played.
///
/// Every oscillator waveform is 64 points long, and the speed parameter denotes by how many points
/// per tick the play position is advanced. So at a vibrato speed of 2, the vibrato waveform
/// repeats after 32 ticks.
///
/// ## Canonicalization
/// The valid values for waveforms are `0..=3`, all out-of-range values are parsed as `3`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Waveform {
    /// Sine wave `0`
    Sine,

    /// Sawtooth (ramp-down) wave `1`
    Sawtooth,

    /// Square wave `2`
    Square,

    /// White noise `3`
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


impl Row {
    /// Create new empty row
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

/// Creates a formatted string for the note in the given buffer
const fn note_string(Note(idx): Note, buf: &mut [u8; 3]) -> &str {
    if idx >= 120 {
        panic!("Note inner value is out of range of 0..=119");
    }
    const NAMES: [&[u8; 2]; 12] = [b"C-", b"C#", b"D-", b"D#", b"E-", b"F-", b"F#", b"G-", b"G#", b"A-", b"A#", b"B-"];
    let name = NAMES[(idx % 12) as usize];
    let octave = b'0' + (idx / 12);
    buf[0] = name[0];
    buf[1] = name[1];
    buf[2] = octave;
    // SAFETY This function fills the buffer with valid UTF-8 itself.
    unsafe { str::from_utf8_unchecked(&*buf) }
}

impl Debug for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = [0; 3];
        f.write_str(note_string(*self, &mut buf))
    }
}

impl Display for Note {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = [0; 3];
        f.write_str(note_string(*self, &mut buf))
    }
}

impl Note {
    /// Convert note into its frequency in A=440Hz tuning
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


impl VolumeCmd {
    pub fn category(&self) -> EffectCategory {
        match self {
            VolumeCmd::FineVolumeUp(_) |
            VolumeCmd::FineVolumeDown(_) |
            VolumeCmd::VolumeSlideUp(_) |
            VolumeCmd::VolumeSlideDown(_) |
            VolumeCmd::SetVolume(_)
                => EffectCategory::Volume,

            VolumeCmd::PortamentoDown(_) |
            VolumeCmd::PortamentoUp(_) |
            VolumeCmd::TonePortamento(_) |
            VolumeCmd::Vibrato(_)
                => EffectCategory::Pitch,

            VolumeCmd::Panning(_)
                => EffectCategory::Panning,
        }
    }
}

impl EffectCmd {
    pub fn category(&self) -> EffectCategory {
        match self {
            EffectCmd::SetSpeed(_) |
            EffectCmd::Tempo(_)
                => EffectCategory::GlobalTiming,

            EffectCmd::JumpOrder(_) |
            EffectCmd::BreakRow(_) |
            EffectCmd::Set(Some(Set::PatternTickDelay(_))) |
            EffectCmd::Set(Some(Set::LoopbackPoint)) |
            EffectCmd::Set(Some(Set::LoopbackTimes(_))) |
            EffectCmd::Set(Some(Set::PatternRowDelay(_)))
                => EffectCategory::GlobalPattern,

            EffectCmd::VolumeSlide(_) |
            EffectCmd::Tremor(_, _) |
            EffectCmd::SetChannelVolume(_) |
            EffectCmd::ChannelVolumeSlide(_) |
            EffectCmd::Tremolo(_, _) |
            EffectCmd::Set(Some(Set::TremoloWaveform(_))) |
            EffectCmd::SetGlobalVolume(_) |
            EffectCmd::GlobalVolumeSlide(_)
                => EffectCategory::Volume,

            EffectCmd::PortamentoDown(_) |
            EffectCmd::PortamentoUp(_) |
            EffectCmd::TonePortamento(_) |
            EffectCmd::Vibrato(_, _) |
            EffectCmd::Arpeggio(_, _) |
            EffectCmd::Set(Some(Set::Glissando(_))) |
            EffectCmd::Set(Some(Set::Finetune(_))) |
            EffectCmd::Set(Some(Set::VibratoWaveform(_))) |
            EffectCmd::Set(Some(Set::PanbrelloWaveform(_))) |
            EffectCmd::FineVibrato(_, _)
                => EffectCategory::Pitch,

            EffectCmd::PanningSlide(_) |
            EffectCmd::Set(Some(Set::Panning(_))) |
            EffectCmd::SetPanningPosition(_) |
            EffectCmd::Panbrello(_, _)
                => EffectCategory::Panning,

            _ => EffectCategory::Misc,
        }
    }
}
