use super::*;
use std::convert::TryFrom;
use std::fmt::{self, Debug};
use std::ops::Index;

#[derive(Clone, Debug)]
pub struct InstrumentFile {
    pub instrument: Instrument,
    pub samples: Vec<Sample>,
}

#[derive(Clone, Debug)]
pub struct Instrument {
    /// Instrument Name, null-terminated (but may also contain nulls)
    pub name: Name,

    /// DOS Filename, null-terminated
    pub filename: DosFilename,

    pub flags: InstrumentFlags,

    /// New Note Action
    pub new_note_action: u8,

    /// Duplicate Note Check Type
    pub duplicate_check_type: u8,

    /// Duplicate Note Check Action
    pub duplicate_check_action: u8,

    /// Instrument Fadeout
    ///
    /// 0..=256, although values up to 1024 would be sensible. Up to IT2.07, the limit was 0..=128.
    ///
    /// OpenMPT uses `u16` but we replaced it with `u8` to limit the range.
    pub instrument_fadeout: u8,

    /// Pitch/Pan Separatation
    pub pitch_pan_separation: i8,

    /// Pitch/Pan Centre
    pub pitch_pan_centre: u8,

    /// Global Volume
    pub global_volume: u8,

    /// Panning
    pub default_panning: RangedU8<0, 128>,

    /// Random volume variation (percentage)
    pub random_volume_variation: RangedU8<0, 100>,

    /// Pan Swing
    pub random_panning_variation: RangedU8<0, 64>,

    /// Tracker ID
    pub trkver: u16,

    /// Number of embedded samples
    pub number_of_samples: u8,

    /// Filter Cutoff
    pub initial_filter_cutoff: RangedU8<0, 128>,

    /// Filter Resonance
    pub initial_filter_resonance: RangedU8<0, 128>,

    /// MIDI Channel
    pub mch: u8,

    /// MIDI Program
    pub mpr: u8,

    /// MIDI Bank
    pub mbank: [u8; 2],

    /// Sample / Transpose map
    pub sample_map: SampleMap,

    /// Volume Envelope
    pub volume_envelope: Envelope,

    /// Pan Envelope
    pub panning_envelope: Envelope,

    /// Pitch / Filter Envelope
    pub pitch_filter_envelope: Envelope,
}

bitflags! {
    /// Instrument flags
    ///
    /// In the IT file these are suffed in some of the bits of the values themselves, we move the
    /// boolean flags out of values and make the data validation clearer by using range-restricted
    /// types.
    #[derive(Default)]
    pub struct InstrumentFlags: u8 {
        /// Enable panning
        ///
        /// This is normally stored in the high bit of [`Instrument::default_panning`].
        const ENABLE_PANNING = 1 << 0;

        /// Enable filter cutoff
        ///
        /// This is normally stored in the high bit of [`Instrument::initial_filter_cutoff`].
        const ENABLE_FILTER_CUTOFF = 1 << 1;

        /// Enable filter resonance
        ///
        /// This is normally stored in the high bit of [`Instrument::initial_filter_resonance`].
        const ENABLE_FILTER_RESONANCE = 1 << 2;
    }
}

#[derive(Clone, Copy)]
pub struct SampleMap {
    pub(crate) map: [Option<SampleId>; 120],
}


#[allow(non_upper_case_globals)]
impl Instrument {
    pub(crate) const dfp_ignorePanning: u8 = 0x80;
    pub(crate) const ifc_enableCutoff: u8 = 0x80;
    pub(crate) const ifr_enableResonance: u8 = 0x80;
}

impl Default for SampleMap {
    fn default() -> SampleMap {
        SampleMap {
            map: [None; 120],
        }
    }
}

impl Debug for SampleMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(
                (0..120)
                    .zip(self.map.iter())
                    .filter_map(|(k, v)| v.map(|v| (Note::try_from(k).unwrap(), v)))
            )
            .finish()
    }
}

impl Index<Note> for SampleMap {
    type Output = Option<SampleId>;
    fn index(&self, index: Note) -> &Self::Output {
        &self.map[usize::from(u8::from(index))]
    }
}

impl Index<&Note> for SampleMap {
    type Output = Option<SampleId>;
    fn index(&self, index: &Note) -> &Self::Output {
        self.index(*index)
    }
}
