use super::*;


bitflags! {
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
        const LAST_MASKVAR = 1 << 7;

        /// All bits but the top bit
        const CHANNEL_INDEX = !ChannelMask::LAST_MASKVAR.bits();
    }
}

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
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let (input, length) = le_u16(input)?;
    let (input, rows) = le_u16(input)?;
    let (input, _padding) = take(4usize)(input)?;
    let (rest, input) = take(length)(input)?;

    let mut active_channels = ActiveChannels::empty();

    let (_empty, rows) = context!(
        all_consuming(count(
            map(
                many_till(command(State::default()), tag(b"\0")),
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

fn command<'i, E>(mut state: State) -> impl FnMut(&'i [u8]) -> IResult<&'i [u8], (Channel, Command), E>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    context!(
        move |input: &'i [u8]| {
            let (input, (channel_mask, channel)) = context!(
                |input| {
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

            let (input, mask_var) = mask_var(&mut state, channel, channel_mask, input)?;

            let (input, (note, instrument, volume, effect)) = context!(
                |input| {
                    let (input, note) = note(&mut state, channel, mask_var, input)?;
                    let (input, instrument) = instrument(&mut state, channel, mask_var, input)?;
                    let (input, volume) = volume(&mut state, channel, mask_var, input)?;
                    let (input, effect) = effect(&mut state, channel, mask_var, input)?;
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
        let (input, byte) = le_u8(input)?;
        let volume = match byte {
              0 ..=  64 => VolumeCmd::SetVolume(byte.cast()),
            128 ..= 192 => VolumeCmd::Panning((byte - 128).cast()),
             65 ..=  74 => VolumeCmd::FineVolumeUp((byte - 65).cast()),
             75 ..=  84 => VolumeCmd::FineVolumeDown((byte - 75).cast()),
             85 ..=  94 => VolumeCmd::VolumeSlideUp((byte - 85).cast()),
             95 ..= 104 => VolumeCmd::VolumeSlideDown((byte - 95).cast()),
            105 ..= 114 => VolumeCmd::PitchSlideDown((byte - 105).cast()),
            115 ..= 124 => VolumeCmd::PitchSlideUp((byte - 115).cast()),
            193 ..= 202 => VolumeCmd::Portamento((byte - 193).cast()),
            203 ..= 212 => VolumeCmd::Vibrato((byte - 203).cast()),
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
            let effect = match effect_code {
                'A' => EffectCmd::SetSpeed(param),
                'B' => EffectCmd::JumpOrder(param),
                'C' => EffectCmd::BreakRow(param),
                'D' | 'K' | 'L' => {
                    let volume_slide = match (x, y) {
                        (0x0, 0x0) => VolumeSlide::Continue,
                        (0x0, p) => VolumeSlide::Down(p.cast()),
                        (p, 0x0) => VolumeSlide::Up(p.cast()),
                        (0xF, p) => VolumeSlide::FineDown(p.cast()),
                        (p, 0xF) => VolumeSlide::FineUp(p.cast()),
                        (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(), // x, y are nibbles.
                        _ => VolumeSlide::Other(param),
                    };
                    match effect {
                        0x04 => EffectCmd::VolumeSlide(volume_slide),
                        0x0B => EffectCmd::VolumeSlideAndVibrato(volume_slide),
                        0x0C => EffectCmd::VolumeSlideAndPortamento(volume_slide),
                        _ => unreachable!(),
                    }
                },
                'E' => EffectCmd::PitchSlideDown(match param {
                    0x00 ..= 0xDF => PitchSlideDown::Coarse(param.cast()),
                    0xF0 ..= 0xFF => PitchSlideDown::Fine((param - 0xF0).cast()),
                    0xE0 ..= 0xEF => PitchSlideDown::ExtraFine((param - 0xE0).cast()),
                }),
                'F' => EffectCmd::PitchSlideUp(match param {
                    0x00 ..= 0xDF => PitchSlideUp::Coarse(param.cast()),
                    0xF0 ..= 0xFF => PitchSlideUp::Fine((param - 0xF0).cast()),
                    0xE0 ..= 0xEF => PitchSlideUp::ExtraFine((param - 0xE0).cast()),
                }),
                'G' => EffectCmd::Portamento(param),
                'H' => EffectCmd::Vibrato(x.cast(), y.cast()),
                'I' => EffectCmd::Tremor(x.cast(), y.cast()),
                'J' => EffectCmd::Arpeggio(x.cast(), if y != x { Some(y.cast()) } else { None }),
                // 0x0B and 0x0C are handled together with 0x04 above
                'M' => EffectCmd::SetChannelVolume(param),
                'N' => EffectCmd::ChannelVolumeSlide(match (x, y) {
                    (0x0, 0x0) => ChannelVolumeSlide::Continue,
                    (0x0, p) => ChannelVolumeSlide::Down(p.cast()),
                    (p, 0x0) => ChannelVolumeSlide::Up(p.cast()),
                    (0xF, p) => ChannelVolumeSlide::FineDown(p.cast()),
                    (p, 0xF) => ChannelVolumeSlide::FineUp(p.cast()),
                    (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(), // x, y are nibbles.
                    _ => ChannelVolumeSlide::Other(param),
                }),
                'O' => EffectCmd::SetSampleOffset(SetSampleOffset::Low(param)),
                'S' if x == 0xA => EffectCmd::SetSampleOffset(SetSampleOffset::High(y.cast())),
                'P' => EffectCmd::PanningSlide(match (x, y) {
                    (0x0, 0x0) => PanningSlide::Continue,
                    (0x0, p) => PanningSlide::Right(p.cast()),
                    (p, 0x0) => PanningSlide::Left(p.cast()),
                    (0xF, p) => PanningSlide::FineRight(p.cast()),
                    (p, 0xF) => PanningSlide::FineLeft(p.cast()),
                    (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(), // x, y are nibbles.
                    _ => PanningSlide::Other(param),
                }),
                'Q' => {
                    // Value 8 is documented as unused however we can't simply exclude it from the
                    // valid range so we'll instead map it to 0 which means "no volume change".
                    let x = if x == 8 { 0 } else { x };
                    EffectCmd::Retrigger(x.cast(), y.cast())
                }
                'R' => EffectCmd::Tremolo(x.cast(), y.cast()),
                'S' => EffectCmd::Set(match x {
                    0x0 => Set::Continue,
                    0x1 => Set::Glissando(y != 0x0),
                    0x2 => Set::Finetune(y.cast()),
                    0x3 | 0x4 | 0x5 => {
                        let waveform = match y {
                            0x0 => Waveform::Sine,
                            0x1 => Waveform::RampDown,
                            0x2 => Waveform::Square,
                            0x3 => Waveform::Random,
                            _ => bail!(input, "`S{}y` parameter y={:#x} out of range 0x0..=0x3", x, y),
                        };
                        match x {
                            0x3 => Set::VibratoWaveform(waveform),
                            0x4 => Set::TremoloWaveform(waveform),
                            0x5 => Set::PanbrelloWaveform(waveform),
                            _ => unreachable!(),
                        }
                    },
                    0x6 => Set::PatternDelay(y.cast()),
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
                        _ => bail!(input, "command `S7y` parameter y={:#x} out of range 0x0..=0xC", y),
                    },
                    0x8 => Set::Panning(y.cast()),
                    0x9 => match y {
                        0x0 => Set::Surround(false),
                        0x1 => Set::Surround(true),
                        0x2..=0x7 => bail!(input,  "command `S9y` parameter y={:#x} out of range 0x0..=0x1 and 0x8..=0xF", y),
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
                }),
                'T' => EffectCmd::Tempo(match x {
                    0x0 => Tempo::SlideDown(y.cast()),
                    0x1 => Tempo::SlideUp(y.cast()),
                    _ => Tempo::Set(param.cast()),
                }),
                'U' => EffectCmd::FineVibrato(x.cast(), y.cast()),
                'V' => {
                    // The value is only valid up to 0x80 according to Schism Tracker, however even
                    // OpenMPT will let it go higher. I'm not sure what would be the "most correct"
                    // way of handling this, so let's just limit the value for now.
                    let param = param.min(0x80);
                    EffectCmd::SetGlobalVolume(param.cast())
                },
                'W' => EffectCmd::GlobalVolumeSlide(match (x, y) {
                    (0x0, 0x0) => GlobalVolumeSlide::Continue,
                    (0x0, p) => GlobalVolumeSlide::Down(p.cast()),
                    (p, 0x0) => GlobalVolumeSlide::Up(p.cast()),
                    (0xF, p) => GlobalVolumeSlide::FineDown(p.cast()),
                    (p, 0xF) => GlobalVolumeSlide::FineUp(p.cast()),
                    (0x10..=0xFF, _) | (_, 0x10..=0xFF) => unreachable!(), // x, y are nibbles.
                    _ => GlobalVolumeSlide::Other(param),
                }),
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
            EffectCmd::SetSpeed(0x12),

            // B
            EffectCmd::JumpOrder(0x12),

            // C
            EffectCmd::BreakRow(0x12),

            // D
            EffectCmd::VolumeSlide(VolumeSlide::Continue),
            EffectCmd::VolumeSlide(VolumeSlide::Down(1.cast())),
            EffectCmd::VolumeSlide(VolumeSlide::Up(2.cast())),
            EffectCmd::VolumeSlide(VolumeSlide::FineDown(3.cast())),
            EffectCmd::VolumeSlide(VolumeSlide::FineUp(4.cast())),
            EffectCmd::VolumeSlide(VolumeSlide::Other(0x45)),

            // E
            EffectCmd::PitchSlideDown(PitchSlideDown::Coarse(1.cast())),
            EffectCmd::PitchSlideDown(PitchSlideDown::Fine(2.cast())),
            EffectCmd::PitchSlideDown(PitchSlideDown::ExtraFine(3.cast())),

            // F
            EffectCmd::PitchSlideUp(PitchSlideUp::Coarse(3.cast())),
            EffectCmd::PitchSlideUp(PitchSlideUp::Fine(2.cast())),
            EffectCmd::PitchSlideUp(PitchSlideUp::ExtraFine(1.cast())),

            // G
            EffectCmd::Portamento(0x42),

            // H
            EffectCmd::Vibrato(6.cast(), 7.cast()),

            // I
            EffectCmd::Tremor(1.cast(), 5.cast()),

            // J
            EffectCmd::Arpeggio(2.cast(), Some(3.cast())),

            // K
            EffectCmd::VolumeSlideAndVibrato(VolumeSlide::Down(1.cast())),

            // L
            EffectCmd::VolumeSlideAndPortamento(VolumeSlide::Down(2.cast())),

            // M
            EffectCmd::SetChannelVolume(0xE0),

            // N
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::Continue),
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::Down(1.cast())),
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::Up(2.cast())),
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::FineDown(3.cast())),
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::FineUp(4.cast())),
            EffectCmd::ChannelVolumeSlide(ChannelVolumeSlide::Other(0x42)),

            // O
            EffectCmd::SetSampleOffset(SetSampleOffset::Low(0x21)),

            // P
            EffectCmd::PanningSlide(PanningSlide::Continue),
            EffectCmd::PanningSlide(PanningSlide::Right(6.cast())),
            EffectCmd::PanningSlide(PanningSlide::Left(5.cast())),
            EffectCmd::PanningSlide(PanningSlide::FineRight(4.cast())),
            EffectCmd::PanningSlide(PanningSlide::FineLeft(3.cast())),
            EffectCmd::PanningSlide(PanningSlide::Other(0x12)),

            // Q
            EffectCmd::Retrigger(0xA.cast(), 0xB.cast()),

            // R
            EffectCmd::Tremolo(0xC.cast(), 0xD.cast()),

            // S
            EffectCmd::Set(Set::Continue),
            EffectCmd::Set(Set::Continue),
            EffectCmd::Set(Set::Glissando(false)),
            EffectCmd::Set(Set::Glissando(true)),
            EffectCmd::Set(Set::Glissando(true)),
            EffectCmd::Set(Set::Finetune(0xD.cast())),
            EffectCmd::Set(Set::VibratoWaveform(Waveform::RampDown)),
            EffectCmd::Set(Set::TremoloWaveform(Waveform::Square)),
            EffectCmd::Set(Set::PanbrelloWaveform(Waveform::Random)),
            EffectCmd::Set(Set::PatternDelay(0x6.cast())),
            EffectCmd::Set(Set::PastNote(SetPastNote::Cut)),
            EffectCmd::Set(Set::NewNoteAction(SetNewNoteAction::Continue)),
            EffectCmd::Set(Set::PitchEnvelope(true)),
            EffectCmd::Set(Set::Surround(true)),
            EffectCmd::Set(Set::FilterMode(FilterMode::Local)),
            EffectCmd::Set(Set::LoopbackPoint),
            EffectCmd::Set(Set::LoopbackTimes(0x3.cast())),
            EffectCmd::Set(Set::PatternRowDelay(0xD.cast())),
            EffectCmd::Set(Set::MIDIParam(0xF.cast())),

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
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::Continue),
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::Down(1.cast())),
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::Up(2.cast())),
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::FineDown(3.cast())),
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::FineUp(4.cast())),
            EffectCmd::GlobalVolumeSlide(GlobalVolumeSlide::Other(0xAC)),

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
