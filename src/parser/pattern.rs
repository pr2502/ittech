use super::*;


bitflags! {
    /// Command mask
    ///
    /// Instructs the parser how to implement the following bytes.
    ///
    /// - `READ_*` masks say the parser should read the value from the input
    /// - `LAST_*` masks say the last read value for that channel should be reused
    /// - if no mask is present the command does not contain that sub-command.
    struct Mask: u8 {
        const READ_NOTE = 1 << 0;
        const READ_INSTRUMENT = 1 << 1;
        const READ_VOLUME = 1 << 2;
        const READ_EFFECT = 1 << 3;
        const LAST_NOTE = 1 << 4;
        const LAST_INSTRUMENT = 1 << 5;
        const LAST_VOLUME = 1 << 6;
        const LAST_EFFECT = 1 << 7;
    }
}

bitflags! {
    struct ChannelMask: u8 {
        /// Top bit
        ///
        /// If present the parser should reuse the last command [`Mask`] for channel, otherwise it
        /// should read the next byte and use it as a command mask.
        const LAST_MASKVAR = 1 << 7;

        /// All bits but the top bit
        ///
        /// Mask for the bits that make up the channel index.
        const CHANNEL_INDEX = !ChannelMask::LAST_MASKVAR.bits();
    }
}

/// Command parser state
///
/// Holds the previous values for command mask and sub-commands for each channel.
struct State {
    last_maskvar: [Mask; 64],
    last_note: [Option<NoteCmd>; 64],
    last_instrument: [Option<InstrumentId>; 64],
    last_volume: [Option<VolumeCmd>; 64],
    last_effect: [Option<EffectCmd>; 64],
}

impl Default for State {
    fn default() -> State {
        State {
            last_maskvar: [Mask::empty(); 64],
            last_note: [None; 64],
            last_instrument: [None; 64],
            last_volume: [None; 64],
            last_effect: [None; 64],
        }
    }
}


pub(super) fn pattern<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], Pattern, E>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]> + 'i,
{
    let (input, length) = le_u16(input)?;
    let (input, rows) = le_u16(input)?;
    let (input, _padding) = take(4usize)(input)?;
    let (rest, input) = take(length)(input)?;

    let mut active_channels = ActiveChannels::empty();
    let mut state = State::default();

    let (_empty, rows) = context!(
        all_consuming(count(
            map(
                many_till(command(&mut state), tag(b"\0")),
                |(commands, _)| {
                    active_channels |= commands.iter().map(|(chan, _)| *chan).collect();
                    Row::from_vec(commands)
                },
            ),
            rows as usize,
        )),
        "in pattern",
    )(input)?;

    Ok((
        rest,
        Pattern {
            active_channels,
            rows,
        },
    ))
}

fn command<'i, 's, E>(state: &'s mut State) -> impl FnMut(&'i [u8]) -> IResult<&'i [u8], (Channel, Command), E> + 's
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]> + 'i,
    'i: 's,
{
    context!(
        move |input: &'i [u8]| {
            let (input, (channel_mask, channel)) = context!(
                move |input| {
                    let (input, channel_var) = le_u8(input)?;

                    assert!(
                        channel_var != 0,
                        "this is a bug: 0 marks end of row and should be handled by a different parser",
                    );

                    let channel_mask = ChannelMask::from_bits_truncate(channel_var);
                    let channel_num = channel_var & ChannelMask::CHANNEL_INDEX.bits();
                    if !(1..=64).contains(&channel_num) {
                        bail!(input, "value is out of range 1..=64");
                    }
                    let channel = Channel::from_u8_index(channel_num - 1);

                    Ok((input, (channel_mask, channel)))
                },
                "reading channel",
            )(input)?;

            let (input, mask_var) = mask_var(state, channel, channel_mask, input)?;

            let (input, (note, instrument, volume, effect)) = context!(
                |input| {
                    let (input, note) = note(state, channel, mask_var, input)?;
                    let (input, instrument) = instrument(state, channel, mask_var, input)?;
                    let (input, volume) = volume(state, channel, mask_var, input)?;
                    let (input, effect) = effect(state, channel, mask_var, input)?;
                    Ok((input, (note, instrument, volume, effect)))
                },
                "reading command with mask 0x{:02x} ({:?})",
                mask_var.bits(),
                mask_var,
            )(input)?;

            Ok((
                input,
                (
                    channel,
                    Command {
                        note,
                        instrument,
                        volume,
                        effect,
                    },
                ),
            ))
        },
        "in command",
    )
}

fn mask_var<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(
    state: &mut State,
    channel: Channel,
    channel_mask: ChannelMask,
    input: &'i [u8],
) -> IResult<&'i [u8], Mask, E> {
    if channel_mask.contains(ChannelMask::LAST_MASKVAR) {
        let (input, mask_var) = context!(le_u8, "reading mask var")(input)?;
        let mask_var = Mask::from_bits_truncate(mask_var);
        state.last_maskvar[channel.as_usize()] = mask_var;
        Ok((input, mask_var))
    } else {
        Ok((input, state.last_maskvar[channel.as_usize()]))
    }
}

fn note<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(
    state: &mut State,
    channel: Channel,
    mask_var: Mask,
    input: &'i [u8],
) -> IResult<&'i [u8], Option<NoteCmd>, E> {
    if mask_var.contains(Mask::READ_NOTE) && !mask_var.contains(Mask::LAST_NOTE) {
        let (input, note_var) = context!(le_u8, "reading note")(input)?;
        let note = match note_var {
            0 ..= 119 => NoteCmd::Play(note_var.try_into().unwrap()),
            255 => NoteCmd::Off,
            254 => NoteCmd::Cut,
            _ => NoteCmd::Fade,
        };
        state.last_note[channel.as_usize()] = Some(note);
        Ok((input, Some(note)))
    } else if mask_var.contains(Mask::LAST_NOTE) {
        Ok((input, state.last_note[channel.as_usize()]))
    } else {
        Ok((input, None))
    }
}

fn instrument<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(
    state: &mut State,
    channel: Channel,
    mask_var: Mask,
    input: &'i [u8],
) -> IResult<&'i [u8], Option<InstrumentId>, E> {
    if mask_var.contains(Mask::READ_INSTRUMENT) && !mask_var.contains(Mask::LAST_INSTRUMENT) {
        let (input, instrument) = context!(ranged(le_u8, 0..=99), "reading instrument id")(input)?;
        let instrument = match instrument {
            0 => None,
            1 ..= 99 => Some((instrument - 1).try_into().unwrap()),
            _ => unreachable!(), // Used `ranged` combinator to read the value.
        };
        state.last_instrument[channel.as_usize()] = instrument;
        Ok((input, instrument))
    } else if mask_var.contains(Mask::LAST_INSTRUMENT) {
        Ok((input, state.last_instrument[channel.as_usize()]))
    } else {
        Ok((input, None))
    }
}

fn volume<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(
    state: &mut State,
    channel: Channel,
    mask_var: Mask,
    input: &'i [u8],
) -> IResult<&'i [u8], Option<VolumeCmd>, E> {
    if mask_var.contains(Mask::READ_VOLUME) && !mask_var.contains(Mask::LAST_VOLUME) {
        let (input, x) = le_u8(input)?;
        let volume = match x {
              0 ..=  64 => VolumeCmd::SetVolume(x.cast()),
            128 ..= 192 => VolumeCmd::Panning((x - 128).cast()),
             65 ..=  74 => VolumeCmd::FineVolumeUp((x > 65).then(|| (x - 65).cast())),
             75 ..=  84 => VolumeCmd::FineVolumeDown((x > 75).then(|| (x - 75).cast())),
             85 ..=  94 => VolumeCmd::VolumeSlideUp((x > 85).then(|| (x - 85).cast())),
             95 ..= 104 => VolumeCmd::VolumeSlideDown((x > 95).then(|| (x - 95).cast())),
            105 ..= 114 => VolumeCmd::PortamentoDown((x > 105).then(|| (x - 105).cast())),
            115 ..= 124 => VolumeCmd::PortamentoUp((x > 115).then(|| (x - 115).cast())),
            193 ..= 202 => VolumeCmd::TonePortamento((x > 193).then(|| (x - 193).cast())),
            203 ..= 212 => VolumeCmd::Vibrato((x > 203).then(|| (x - 203).cast())),
            _ => {
                // There is a gap in between the intervals so we can't simply use `ranged`.
                bail!(input, "value is not a valid volume");
            },
        };
        state.last_volume[channel.as_usize()] = Some(volume);
        Ok((input, Some(volume)))
    } else if mask_var.contains(Mask::LAST_VOLUME) {
        Ok((input, state.last_volume[channel.as_usize()]))
    } else {
        Ok((input, None))
    }
}

fn effect<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(
    state: &mut State,
    channel: Channel,
    mask_var: Mask,
    input: &'i [u8],
) -> IResult<&'i [u8], Option<EffectCmd>, E> {
    if mask_var.contains(Mask::READ_EFFECT) && !mask_var.contains(Mask::LAST_EFFECT) {
        // We're using effects definition from OpenMPT but we're trying to adhere to ITTECH.TXT so
        // we only parse the first 31. In the future we might want to support the `itEx` format
        // from OpenMPT instead.
        let (rest, (effect, param)) = tuple((
            // TODO Invalid effects should be just ignored and the error logged.
            //      Postponing this until the warning logging is implemented.
            //
            // NOTE OpenMPT just completely ignores an effect if it's invalid and doesn't write the
            //      invalid effect back on save, this means our strategy of not letting invalid
            //      stuff through will give us mostly parity with OpenMPT, yay!
            context!(ranged(le_u8, 0..=0x1A), "reading effect number"),
            context!(le_u8, "reading effect parameter"),
        ))(input)?;

        let effect = if effect == 0x00 {
            None
        } else {
            // Extract param nibbles.
            let (x, y) = (param >> 4, param & 0x0F);

            // Effects are numbered 0x1..=0x1A (or 1..=26 decimal), these numbers are represented
            // as capital leters in in tracker UI and documentation. We convert it to ascii char
            // range here to make the large match statement more readable.
            let effect_code = (effect - 1 + b'A') as char;

            // For more information on the values here, see the documentation for `EffectCmd`
            // and its child enums.
            //
            // Parsing here is an attempt to be consistent with OpenMPT and Schism Tracker.
            // Inconsistencies must be documented and justified, otherwise that's a bug.
            //
            // Early returns in this match are not saving the, this should be only done when
            // encountering an error.
            let effect = match effect_code {
                'A' if param == 0 => return Ok((rest, None)),
                'A' => EffectCmd::SetSpeed(param.cast()),
                'B' => EffectCmd::JumpOrder(param),
                'C' => EffectCmd::BreakRow(param),
                'D' | 'K' | 'L' | 'N' | 'W' => {
                    // OpenMPT code for this part is extremely confusing, Schism Tracker code is
                    // better but there are three functions which are supposed to be doing the
                    // exact same parsing and their bodies are different. The code is complicated
                    // enough that we're not sure they're all behaving exactly the same.
                    //
                    // For reference see file `schismtracker/player/effects.c` functions
                    // `fx_volume_slide`, `fx_channel_vol_slide` and `fx_global_vol_slide`.
                    //
                    // This code was written with reference to `fx_volume_slide`.
                    let volume_slide = match (x, y) {
                        // For the first three branches the parsing is unambiguous and the order
                        // doesn't matter.
                        (0x0, 0x0) => None,
                        (p, 0x0) => Some(VolumeSlide::Up(p.cast())),
                        (0x0, p) => Some(VolumeSlide::Down(p.cast())),

                        // OpenMPT wiki says that `FineUp` and `FineDown` parameters can never be
                        // `0xF` however both OpenMPT and Schism Tracker seem to allow `0xF` with
                        // `FineUp`.
                        //
                        // TODO We might want to clip it to `0xE` in the future.
                        (p, 0xF) => Some(VolumeSlide::FineUp(p.cast())),
                        (0xF, p) => Some(VolumeSlide::FineDown(p.cast())),

                        // x, y are nibbles. These values are unrepresentable.
                        (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(),

                        // We'll be completely ignoring values in the invalid ranges, this
                        // behaviour almost matches the Schism Tracker player. The only difference
                        // is that we'll behave like we never saw the invalid values but SM would
                        // save them into memory before ignoring them, which means all recall
                        // commands (e.g. `D00`) would also get ignored by ST until a valid command
                        // is played. This inconsistency makes our API much cleaner and since these
                        // values are "invalid" anyway it shouldn't create any problems.
                        //
                        // TODO In the future we want to emit a warning when an out-of-range value
                        //      is detected and gets converted to something else.
                        _ => return Ok((rest, None)),
                    };
                    match effect_code {
                        'D' => EffectCmd::VolumeSlide(volume_slide),
                        'K' => EffectCmd::VolumeSlideAndVibrato(volume_slide),
                        'L' => EffectCmd::VolumeSlideAndPortamento(volume_slide),
                        'N' => EffectCmd::ChannelVolumeSlide(volume_slide),
                        'W' => EffectCmd::GlobalVolumeSlide(volume_slide),
                        _ => unreachable!(),
                    }
                },
                'E' | 'F' => {
                    let portamento = match param {
                        0x00 => None,
                        0x01 ..= 0xDF => Some(Portamento::Coarse(param.cast())),
                        0xF0 ..= 0xFF => Some(Portamento::Fine((param - 0xF0).cast())),
                        0xE0 ..= 0xEF => Some(Portamento::ExtraFine((param - 0xE0).cast())),
                    };
                    match effect_code {
                        'E' => EffectCmd::PortamentoDown(portamento),
                        'F' => EffectCmd::PortamentoUp(portamento),
                        _ => unreachable!(),
                    }
                },
                'G' => EffectCmd::TonePortamento((param > 0).then(|| param.cast())),
                'H' => EffectCmd::Vibrato(x.cast(), y.cast()),
                'I' => EffectCmd::Tremor(x.cast(), y.cast()),
                'J' => EffectCmd::Arpeggio((param > 0).then(|| (x.cast(), y.cast()))),
                // 'K' and 'L' are handled together with 'D' above.
                'M' => EffectCmd::SetChannelVolume(
                    // Clipping the value to the allowed maximum.
                    param.min(0x40).cast(),
                ),
                // 'N' is handled together with 'D' above.
                'O' => EffectCmd::SetSampleOffset(SetSampleOffset::Low(param)),
                'S' if x == 0xA => EffectCmd::SetSampleOffset(SetSampleOffset::High(y.cast())),
                'P' => EffectCmd::PanningSlide(match (x, y) {
                    // TODO This parsing requires the same treatment as the `VolumeSlide` family of
                    //      effects: comparison with OMPT/ST implementation and documentation.
                    (0x0, 0x0) => None,
                    (0x0, p) => Some(PanningSlide::Right(p.cast())),
                    (p, 0x0) => Some(PanningSlide::Left(p.cast())),
                    (0xF, p) => Some(PanningSlide::FineRight(p.cast())),
                    (p, 0xF) => Some(PanningSlide::FineLeft(p.cast())),
                    (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(), // x, y are nibbles.
                         // TODO log an error
                    _ => return Ok((rest, None)),
                }),
                'Q' => {
                    // Value 8 is documented as unused however we can't simply exclude it from the
                    // valid range so we'll instead map it to 0 which means "no volume change".
                    let x = if x == 8 { 0 } else { x };
                    EffectCmd::Retrigger(x.cast(), y.cast())
                }
                'R' => EffectCmd::Tremolo(x.cast(), y.cast()),
                'S' => EffectCmd::Set(if x == 0 {
                    None
                } else {
                    Some(match x {
                        0x1 => Set::Glissando(y != 0x0),
                        0x2 => Set::Finetune(y.cast()),
                        0x3 | 0x4 | 0x5 => {
                            let waveform = match y {
                                0x0 => Waveform::Sine,
                                0x1 => Waveform::Sawtooth,
                                0x2 => Waveform::Square,
                                // Parsing any value higher than `0x3` as `Random` too.
                                0x3 | _ => Waveform::Random,
                            };
                            match x {
                                0x3 => Set::VibratoWaveform(waveform),
                                0x4 => Set::TremoloWaveform(waveform),
                                0x5 => Set::PanbrelloWaveform(waveform),
                                _ => unreachable!(),
                            }
                        },
                        0x6 => Set::PatternTickDelay(y.cast()),
                        0x7 => match y {
                            0x0 => Set::PastNote(SetPastNote::Cut),
                            0x1 => Set::PastNote(SetPastNote::Off),
                            0x2 => Set::PastNote(SetPastNote::Fade),
                            0x3 => Set::NewNoteAction(SetNewNoteAction::Cut),
                            0x4 => Set::NewNoteAction(SetNewNoteAction::Continue),
                            0x5 => Set::NewNoteAction(SetNewNoteAction::Off),
                            0x6 => Set::NewNoteAction(SetNewNoteAction::Fade),
                            0x7 => Set::VolumeEnvelope(false),
                            0x8 => Set::VolumeEnvelope(true),
                            0x9 => Set::PanningEnvelope(false),
                            0xA => Set::PanningEnvelope(true),
                            0xB => Set::PitchEnvelope(false),
                            0xC => Set::PitchEnvelope(true),
                            // TODO Log error. ("command `S7y` parameter y={:#x} out of range 0x0..=0xC", y)
                            _ => return Ok((rest, None)),
                        },
                        0x8 => Set::Panning(y.cast()),
                        0x9 => match y {
                            0x0 => Set::Surround(false),
                            0x1 => Set::Surround(true),
                            // TODO Log error. ("command `S9y` parameter y={:#x} out of range 0x0..=0x1 and 0x8..=0xF", y)
                            0x2..=0x7 => return Ok((rest, None)),
                            0x8 => Set::Reverb(false),
                            0x9 => Set::Reverb(true),
                            0xA => Set::SurroundMode(SurroundMode::Center),
                            0xB => Set::SurroundMode(SurroundMode::Quad),
                            0xC => Set::FilterMode(FilterMode::Global),
                            0xD => Set::FilterMode(FilterMode::Local),
                            0xE => Set::Direction(PlayDirection::Forward),
                            0xF => Set::Direction(PlayDirection::Backward),
                            _ => unreachable!(),
                        },
                        // `SAy` handled together with `Oxx`, above.
                        0xB if y == 0x0 => Set::LoopbackPoint,
                        0xB => Set::LoopbackTimes(y.cast()),
                        0xC => Set::NoteCut(y.cast()),
                        0xD => Set::NoteDelay(y.cast()),
                        0xE => Set::PatternRowDelay(y.cast()),
                        0xF => Set::MIDIParam(y.cast()),
                        _ => unreachable!(),
                    })
                }),
                'T' => EffectCmd::Tempo(match x {
                    0x0 => Tempo::SlideDown(y.cast()),
                    0x1 => Tempo::SlideUp(y.cast()),
                    _ => Tempo::Set(param.cast()),
                }),
                'U' => EffectCmd::FineVibrato(x.cast(), y.cast()),
                'V' => EffectCmd::SetGlobalVolume(
                    // Clipping the value to the allowed maximum.
                    param.min(0x80).cast(),
                ),
                // 'W' is handled together with 'D' above.
                'X' => EffectCmd::SetPanningPosition(param),
                'Y' => EffectCmd::Panbrello(x.cast(), y.cast()),
                'Z' => EffectCmd::MIDI(param),
                _ => unreachable!(), // Used `ranged` parser to read the value.
            };
            Some(effect)
        };
        state.last_effect[channel.as_usize()] = effect;
        Ok((rest, effect))
    } else if mask_var.contains(Mask::LAST_EFFECT) {
        Ok((input, state.last_effect[channel.as_usize()]))
    } else {
        Ok((input, None))
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::error::{VerboseError, convert_error};
    use nom::Err;
    use pretty_assertions::assert_eq;

    fn ensure_parse<'i, O>(
        parser: impl FnOnce(&'i [u8]) -> Result<O, Err<VerboseError<&'i [u8]>>>,
        input: &'i [u8],
    ) -> O {
        match parser(input) {
            Ok(res) => res,
            Err(Err::Error(e)) | Err(Err::Failure(e)) => {
                panic!("parser failed\n\n{}", convert_error(input, e));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn effect_alphabet() {
        const DATA: &[u8] = include_bytes!("../../tests/effect_alphabet.it");
        let alphabet = &[
            // A
            EffectCmd::SetSpeed(0x12.cast()),

            // B
            EffectCmd::JumpOrder(0x12),

            // C
            EffectCmd::BreakRow(0x12),

            // D
            EffectCmd::VolumeSlide(None),
            EffectCmd::VolumeSlide(Some(VolumeSlide::Down(1.cast()))),
            EffectCmd::VolumeSlide(Some(VolumeSlide::Up(2.cast()))),
            EffectCmd::VolumeSlide(Some(VolumeSlide::FineDown(3.cast()))),
            EffectCmd::VolumeSlide(Some(VolumeSlide::FineUp(4.cast()))),
            // -- omitted invalid `D45`

            // E
            EffectCmd::PortamentoDown(Some(Portamento::Coarse(1.cast()))),
            EffectCmd::PortamentoDown(Some(Portamento::Fine(2.cast()))),
            EffectCmd::PortamentoDown(Some(Portamento::ExtraFine(3.cast()))),

            // F
            EffectCmd::PortamentoUp(Some(Portamento::Coarse(3.cast()))),
            EffectCmd::PortamentoUp(Some(Portamento::Fine(2.cast()))),
            EffectCmd::PortamentoUp(Some(Portamento::ExtraFine(1.cast()))),

            // G
            EffectCmd::TonePortamento(Some(0x42.cast())),

            // H
            EffectCmd::Vibrato(6.cast(), 7.cast()),

            // I
            EffectCmd::Tremor(1.cast(), 5.cast()),

            // J
            EffectCmd::Arpeggio(Some((2.cast(), 3.cast()))),

            // K
            EffectCmd::VolumeSlideAndVibrato(Some(VolumeSlide::Down(1.cast()))),

            // L
            EffectCmd::VolumeSlideAndPortamento(Some(VolumeSlide::Down(2.cast()))),

            // M
            EffectCmd::SetChannelVolume(0x40.cast()),

            // N
            EffectCmd::ChannelVolumeSlide(None),
            EffectCmd::ChannelVolumeSlide(Some(VolumeSlide::Down(1.cast()))),
            EffectCmd::ChannelVolumeSlide(Some(VolumeSlide::Up(2.cast()))),
            EffectCmd::ChannelVolumeSlide(Some(VolumeSlide::FineDown(3.cast()))),
            EffectCmd::ChannelVolumeSlide(Some(VolumeSlide::FineUp(4.cast()))),
            // -- omitted invalid `N42`

            // O
            EffectCmd::SetSampleOffset(SetSampleOffset::Low(0x21)),

            // P
            EffectCmd::PanningSlide(None),
            EffectCmd::PanningSlide(Some(PanningSlide::Right(6.cast()))),
            EffectCmd::PanningSlide(Some(PanningSlide::Left(5.cast()))),
            EffectCmd::PanningSlide(Some(PanningSlide::FineRight(4.cast()))),
            EffectCmd::PanningSlide(Some(PanningSlide::FineLeft(3.cast()))),
            // -- omitted invalid `P12`

            // Q
            EffectCmd::Retrigger(0xA.cast(), 0xB.cast()),

            // R
            EffectCmd::Tremolo(0xC.cast(), 0xD.cast()),

            // S
            EffectCmd::Set(None),
            EffectCmd::Set(None),
            EffectCmd::Set(Some(Set::Glissando(false))),
            EffectCmd::Set(Some(Set::Glissando(true))),
            EffectCmd::Set(Some(Set::Glissando(true))),
            EffectCmd::Set(Some(Set::Finetune(0xD.cast()))),
            EffectCmd::Set(Some(Set::VibratoWaveform(Waveform::Sawtooth))),
            EffectCmd::Set(Some(Set::TremoloWaveform(Waveform::Square))),
            EffectCmd::Set(Some(Set::PanbrelloWaveform(Waveform::Random))),
            EffectCmd::Set(Some(Set::PatternTickDelay(0x6.cast()))),
            EffectCmd::Set(Some(Set::PastNote(SetPastNote::Cut))),
            EffectCmd::Set(Some(Set::NewNoteAction(SetNewNoteAction::Continue))),
            EffectCmd::Set(Some(Set::PitchEnvelope(true))),
            EffectCmd::Set(Some(Set::Surround(true))),
            EffectCmd::Set(Some(Set::FilterMode(FilterMode::Local))),
            EffectCmd::Set(Some(Set::LoopbackPoint)),
            EffectCmd::Set(Some(Set::LoopbackTimes(0x3.cast()))),
            EffectCmd::Set(Some(Set::PatternRowDelay(0xD.cast()))),
            EffectCmd::Set(Some(Set::MIDIParam(0xF.cast()))),

            // T
            EffectCmd::Tempo(Tempo::SlideDown(1.cast())),
            EffectCmd::Tempo(Tempo::SlideUp(2.cast())),
            EffectCmd::Tempo(Tempo::Set(0x23.cast())),

            // U
            EffectCmd::FineVibrato(0xA.cast(), 0xC.cast()),

            // V
            EffectCmd::SetGlobalVolume(0x42.cast()),
            EffectCmd::SetGlobalVolume(0x80.cast()),

            // W
            EffectCmd::GlobalVolumeSlide(None),
            EffectCmd::GlobalVolumeSlide(Some(VolumeSlide::Down(1.cast()))),
            EffectCmd::GlobalVolumeSlide(Some(VolumeSlide::Up(2.cast()))),
            EffectCmd::GlobalVolumeSlide(Some(VolumeSlide::FineDown(3.cast()))),
            EffectCmd::GlobalVolumeSlide(Some(VolumeSlide::FineUp(4.cast()))),
            // -- omitted invalid `WAC`

            // X
            EffectCmd::SetPanningPosition(0x67),

            // Y
            EffectCmd::Panbrello(1.cast(), 3.cast()),

            // Z
            EffectCmd::MIDI(0xF1),
        ];

        let module = ensure_parse(module, DATA);

        let effects = module.patterns
            .into_iter().nth(0).unwrap()
            .rows.iter()
            .filter_map(|row| row.iter().next())
            .filter_map(|(chan, cmd)| { assert_eq!(chan.as_usize(), 0); cmd.effect })
            .collect::<Vec<_>>();

        dbg!(&effects);

        assert_eq!(effects, alphabet);
    }
}
