use crate::data::*;
use bitflags::bitflags;
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::Error;
use nom::multi::{count, many_till};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::convert::{TryFrom, TryInto};
use util::*;


mod util;


/// Parse Impulse Tracker module file (.it)
pub fn module(input: &[u8]) -> Result<Module, nom::Err<Error<&[u8]>>> {
    // Save the whole input for offset parsing.
    let whole_input = input;

    // Parse header.
    let (input, _) = tag(b"IMPM")(input)?;
    let (input, songname) = name(input)?;
    let (input, highlight_minor) = le_u8(input)?;
    let (input, highlight_major) = le_u8(input)?;
    let (input, ordnum) = le_u16(input)?;
    let (input, insnum) = le_u16(input)?;
    let (input, smpnum) = le_u16(input)?;
    let (input, patnum) = le_u16(input)?;
    let (input, cwtv) = le_u16(input)?;
    let (input, cmwt) = le_u16(input)?;
    let (input, flags) = le_u16(input)?;
    let (input, special) = le_u16(input)?;
    let (input, globalvol) = ranged(le_u8, 0..=128)(input)?;
    let (input, mv) = ranged(le_u8, 0..=128)(input)?;
    let (input, speed) = ranged(le_u8, 1..=255)(input)?;
    let (input, tempo) = ranged(le_u8, 31..=255)(input)?;
    let (input, sep) = ranged(le_u8, 0..=128)(input)?;
    let (input, pwd) = le_u8(input)?;
    let (input, msglength) = le_u16(input)?;
    let (input, msgoffset) = le_u32(input)?;
    let (input, _reserved) = le_u32(input)?;
    let (input, chnpan) = byte_array(input)?;
    let (input, chnvol) = byte_array(input)?;

    // Parse dynamic parts of the header.
    let (input, orders) = count(order, ordnum as usize)(input)?;
    let (input, ins_offsets) = count(le_u32, insnum as usize)(input)?;
    let (input, sam_offsets) = count(le_u32, smpnum as usize)(input)?;
    let (_rest, pat_offsets) = count(le_u32, patnum as usize)(input)?;

    // Offsets are relative to the start of the file, use the whole input every time.
    let (_, instrument_headers) = offset_list(instrument_header, ins_offsets)(whole_input)?;
    let (_, sample_headers) = offset_list(sample_header, sam_offsets)(whole_input)?;
    let patterns = {
        let mut patterns = Vec::with_capacity(pat_offsets.len());
        for offset in pat_offsets.into_iter().map(|o| o as usize) {
            // Pattern parsing is inlined from `offset_list` because we need to handle the special
            // case of offset 0 here.
            if offset == 0 {
                patterns.push(Pattern {
                    active_channels: ActiveChannels::NONE,
                    rows: vec![Vec::new(); 64]
                });
                continue
            }
            if offset >= input.len() {
                return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Eof)));
            }
            let (_, pat) = pattern(&whole_input[offset..])?;
            patterns.push(pat);
        }
        patterns
    };

    let samples = sample_headers.into_iter()
        .map(|header| {
            let data = sample_data(&header, whole_input)?;
            Ok(Sample {
                name: header.name,
                filename: header.filename,
                do_loop: header.flags.contains(SampleFlags::LOOP),
                samplerate_c5: header.samplerate_c5,
                data,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let flags = ModuleFlags::from_parts(flags, special);

    let message = {
        let offset = msgoffset as usize;
        if offset == 0 || offset >= whole_input.len() {
            String::new()
        } else {
            let (_, bytes) = take(msglength as usize)(whole_input)?;
            String::from_utf8_lossy(bytes)
                .to_string()
        }
    };

    Ok(Module {
        name: songname,
        highlight: (highlight_major, highlight_minor),
        made_with_version: cwtv,
        compatible_with_version: cmwt,
        flags,
        global_volume: globalvol.try_into().unwrap(),
        sample_volume: mv.try_into().unwrap(),
        speed: speed.try_into().unwrap(),
        tempo: tempo.try_into().unwrap(),
        pan_separation: sep.try_into().unwrap(),
        pitch_wheel_depth: pwd,
        message,
        orders,
        init_channel_panning: chnpan,
        init_channel_volume: chnvol,
        instruments: instrument_headers,
        samples,
        patterns,
    })
}

/// Parse Impulse Tracker instrument file (.iti)
pub fn instrument(input: &[u8]) -> Result<Instrument, nom::Err<Error<&[u8]>>> {
    // Save the whole input for offset parsing.
    let whole_input = input;

    let (input, header) = instrument_header(whole_input)?;
    let (_, _sample_headers) = count(sample_header, header.number_of_samples as usize)(input)?;
    todo!()
}

/// Parse Impulse Tracker sample file (.its)
pub fn sample(input: &[u8]) -> Result<Sample, nom::Err<Error<&[u8]>>> {
    // Save the whole input for offset parsing.
    let whole_input = input;

    let (_, _header) = sample_header(whole_input)?;
    todo!()
}

fn sample_data<'i>(header: &SampleHeader, whole_input: &'i [u8]) -> Result<Option<Vec<f32>>, nom::Err<Error<&'i [u8]>>> {
    let flags = header.flags;

    assert!(flags.contains(SampleFlags::DATA_SIGNED), "only signed samples are supported");
    assert!(!flags.contains(SampleFlags::STEREO), "only mono samples supported");

    assert!(!flags.contains(SampleFlags::COMPRESSED), "sample compression is not supported");
    assert!(!flags.contains(SampleFlags::OPL_INSTRUMENT), "OPL instrument is not supported");
    assert!(!flags.contains(SampleFlags::EXTERNAL_SAMPLE), "external samples are not supported");
    assert!(!flags.contains(SampleFlags::ADPCM_SAMPLE), "MODPlugin :(");
    assert!(!flags.contains(SampleFlags::DELTA), "delta samples are not supported");
    assert!(!flags.contains(SampleFlags::PTM8_TO_16), "PTM loader is not supported");

    if !flags.contains(SampleFlags::DATA_PRESENT) {
        Ok(None)
    } else {
        let offset = header.data_offset as usize;
        let length = header.data_length as usize;
        let input = &whole_input[offset..];

        let (_, data) = match (
            flags.contains(SampleFlags::DATA_16BIT),
            flags.contains(SampleFlags::DATA_BIG_ENDIAN),
        ) {
            (true, true) => count(map(be_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?,
            (true, false) => count(map(le_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?,
            (false, _) => count(map(le_i8, |s| f32::from(s) / f32::from(i8::MAX)), length)(input)?,
        };

        Ok(Some(data))
    }
}

fn order(input: &[u8]) -> IResult<&[u8], Order> {
    let (input, byte) = le_u8(input)?;
    let order = match byte {
        0..=199 => Order::Index(byte),
        200..=253 => {
            // TODO ITTECH.TXT says only 0..=199 are allowed, but 200..=253 are not used for
            // anything else so we might parse them too
            // Order::Index(byte),
            return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Verify)));
        },
        254 => Order::Separator,
        255 => Order::EndOfSong,
    };
    Ok((input, order))
}

fn pattern(input: &[u8]) -> IResult<&[u8], Pattern> {
    let (input, length) = le_u16(input)?;
    let (input, rows) = le_u16(input)?;
    let (input, _) = take(4usize)(input)?; // padding bytes
    let (_, input) = take(length)(input)?; // pattern size

    bitflags! {
        struct Mask: u8 {
            const READ_NOTE = 1 << 0;
            const READ_INSTRUMENT = 1 << 1;
            const READ_VOLUME = 1 << 2;
            const READ_COMMAND = 1 << 3;
            const LAST_NOTE = 1 << 4;
            const LAST_INSTRUMENT = 1 << 5;
            const LAST_VOLUME = 1 << 6;
            const LAST_COMMAND = 1 << 7;

            const EMPTY = 0;
        }
    }

    bitflags! {
        struct ChannelMask: u8 {
            const LAST_MASKVAR = 1 << 7;
        }
    }

    struct State {
        last_maskvar: [Mask; 64],
        last_note: [Option<NoteCmd>; 64],
        last_instrument: [Option<InstrumentId>; 64],
        last_volume: [Option<(VolumeCmd, u8)>; 64],
        last_effect: [Option<(EffectCmd, u8)>; 64],
    }

    impl Default for State {
        fn default() -> State {
            State {
                last_maskvar: [Mask::EMPTY; 64],
                last_note: [None; 64],
                last_instrument: [None; 64],
                last_volume: [None; 64],
                last_effect: [None; 64],
            }
        }
    }

    // TODO split this function up into more parsers
    fn command(state: &mut State) -> impl FnMut(&[u8]) -> IResult<&[u8], Command> + '_ {
        move |input| {
            let (input, channel_var) = le_u8(input)?;

            assert!(channel_var > 0, "this is a bug: 0 marks end of row and should be handled by a different parser");

            let channel_mask = ChannelMask::from_bits_truncate(channel_var);
            let channel = Channel::from_u8_truncate(channel_var - 1);

            let (input, mask_var) = if channel_mask.contains(ChannelMask::LAST_MASKVAR) {
                let (input, mask_var) = le_u8(input)?;
                let mask_var = Mask::from_bits_truncate(mask_var);
                state.last_maskvar[channel.as_usize()] = mask_var;
                (input, mask_var)
            } else {
                (input, state.last_maskvar[channel.as_usize()])
            };

            let (input, note) = if mask_var.contains(Mask::READ_NOTE) && !mask_var.contains(Mask::LAST_NOTE) {
                let (input, note_var) = le_u8(input)?;
                let note = match note_var {
                    0 ..= 119 => NoteCmd::Play(note_var.try_into().unwrap()),
                    255 => NoteCmd::Off,
                    254 => NoteCmd::Cut,
                    _ => NoteCmd::Fade,
                };
                state.last_note[channel.as_usize()] = Some(note);
                (input, Some(note))
            } else if mask_var.contains(Mask::LAST_NOTE) {
                (input, state.last_note[channel.as_usize()])
            } else {
                (input, None)
            };

            let (input, instrument) = if mask_var.contains(Mask::READ_INSTRUMENT) && !mask_var.contains(Mask::LAST_INSTRUMENT) {
                let (input, instrument) = ranged(le_u8, 0..=99)(input)?;
                let instrument = match instrument {
                    0 => None,
                    1 ..= 99 => Some((instrument - 1).try_into().unwrap()),
                    _ => unreachable!(),
                };
                state.last_instrument[channel.as_usize()] = instrument;
                (input, instrument)
            } else if mask_var.contains(Mask::LAST_INSTRUMENT) {
                (input, state.last_instrument[channel.as_usize()])
            } else {
                (input, None)
            };

            let (input, volume) = if mask_var.contains(Mask::READ_VOLUME) && !mask_var.contains(Mask::LAST_VOLUME) {
                let (input, byte) = le_u8(input)?;
                let volume = match byte {
                      0 ..=  64 => (VolumeCmd::SetVolume, byte),
                    128 ..= 192 => (VolumeCmd::Panning, byte - 128),
                     65 ..=  74 => (VolumeCmd::FineVolumeUp, byte - 65),
                     75 ..=  84 => (VolumeCmd::FineVolumeDown, byte - 75),
                     85 ..=  94 => (VolumeCmd::VolumeSlideUp, byte - 85),
                     95 ..= 104 => (VolumeCmd::VolumeSlideDown, byte - 95),
                    105 ..= 114 => (VolumeCmd::PitchSlideDown, byte - 105),
                    115 ..= 124 => (VolumeCmd::PitchSlideUp, byte - 115),
                    193 ..= 202 => (VolumeCmd::PortamentoTo, byte - 193),
                    203 ..= 212 => (VolumeCmd::Vibrato, byte - 203),
                    _ => return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Verify))),
                };
                state.last_volume[channel.as_usize()] = Some(volume);
                (input, Some(volume))
            } else if mask_var.contains(Mask::LAST_VOLUME) {
                (input, state.last_volume[channel.as_usize()])
            } else {
                (input, None)
            };

            let (input, effect) = if mask_var.contains(Mask::READ_COMMAND) && !mask_var.contains(Mask::LAST_COMMAND) {
                let (input, (effect, param)) = tuple((ranged(le_u8, 0..=31), le_u8))(input)?;
                let effect = match effect {
                     0 => None,
                     1 => Some(EffectCmd::Arpeggio),
                     2 => Some(EffectCmd::PortamentoUp),
                     3 => Some(EffectCmd::PortamentoDown),
                     4 => Some(EffectCmd::TonePortamento),
                     5 => Some(EffectCmd::Vibrato),
                     6 => Some(EffectCmd::TonePortaVol),
                     7 => Some(EffectCmd::VibratoVol),
                     8 => Some(EffectCmd::Tremolo),
                     9 => Some(EffectCmd::Panning8),
                    10 => Some(EffectCmd::Offset),
                    11 => Some(EffectCmd::VolumeSlide),
                    12 => Some(EffectCmd::PositionJump),
                    13 => Some(EffectCmd::Volume),
                    14 => Some(EffectCmd::PatternBreak),
                    15 => Some(EffectCmd::Retrig),
                    16 => Some(EffectCmd::Speed),
                    17 => Some(EffectCmd::Tempo),
                    18 => Some(EffectCmd::Tremor),
                    19 => Some(EffectCmd::MODCMDEX),
                    20 => Some(EffectCmd::S3MCMDEX),
                    21 => Some(EffectCmd::ChannelVolume),
                    22 => Some(EffectCmd::ChannelVolSlide),
                    23 => Some(EffectCmd::GlobalVolume),
                    24 => Some(EffectCmd::GlobalVolslide),
                    25 => Some(EffectCmd::KeyOff),
                    26 => Some(EffectCmd::FineVibrato),
                    27 => Some(EffectCmd::Panbrello),
                    28 => Some(EffectCmd::XFinePortaUpDown),
                    29 => Some(EffectCmd::PanningSlide),
                    30 => Some(EffectCmd::SetEnvPosition),
                    31 => Some(EffectCmd::MIDI),
                    32 => Some(EffectCmd::SmoothMIDI),
                    33 => Some(EffectCmd::DelayCut),
                    34 => Some(EffectCmd::XParam),
                    35 => Some(EffectCmd::NoteSlideUp),
                    36 => Some(EffectCmd::NoteSlideDown),
                    37 => Some(EffectCmd::NoteSlideUpRetrig),
                    38 => Some(EffectCmd::NoteSlideDownRetrig),
                    39 => Some(EffectCmd::ReverseOffset),
                    40 => Some(EffectCmd::DBMEcho),
                    41 => Some(EffectCmd::OffsetPercentage),
                    _ => return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Verify))),
                };
                let effect = effect.map(|effect| (effect, param));
                state.last_effect[channel.as_usize()] = effect;
                (input, effect)
            } else if mask_var.contains(Mask::LAST_COMMAND) {
                (input, state.last_effect[channel.as_usize()])
            } else {
                (input, None)
            };

            Ok((input, Command { channel, note, instrument, volume, effect }))
        }
    }

    let mut active_channels = ActiveChannels::NONE;

    let (input, rows) = all_consuming(count(
        map(
            many_till(command(&mut State::default()), tag(b"\0")),
            |(mut commands, _)| {
                commands.sort_unstable_by_key(|cmd| cmd.channel);
                active_channels |= commands.iter().map(|cmd| cmd.channel).collect();
                commands
            },
        ),
        rows as usize,
    ))(input)?;
    Ok((input, Pattern { active_channels, rows }))
}

fn name(input: &[u8]) -> IResult<&[u8], Name> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, Name { bytes }))
}

fn dosfilename(input: &[u8]) -> IResult<&[u8], DOSFilename> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, DOSFilename { bytes }))
}

fn instrument_header(input: &[u8]) -> IResult<&[u8], InstrumentHeader> {
    let (input, _) = tag(b"IMPI")(input)?;
    let (input, filename) = dosfilename(input)?;
    let (input, nna) = le_u8(input)?;
    let (input, dct) = le_u8(input)?;
    let (input, dca) = le_u8(input)?;
    let (input, fadeout) = le_u16(input)?;
    let (input, pps) = le_i8(input)?;
    let (input, ppc) = le_u8(input)?;
    let (input, gbv) = le_u8(input)?;
    let (input, dfp) = le_u8(input)?;
    let (input, rv) = le_u8(input)?;
    let (input, rp) = le_u8(input)?;
    let (input, trkver) = le_u16(input)?;
    let (input, nos) = le_u8(input)?;
    let (input, _reserved) = le_u8(input)?;
    let (input, name) = name(input)?;
    let (input, ifc) = le_u8(input)?;
    let (input, ifr) = le_u8(input)?;
    let (input, mch) = le_u8(input)?;
    let (input, mpr) = le_u8(input)?;
    let (input, mbank) = byte_array(input)?;
    // TODO while changing the representation also factor this parser out into its own function
    let (input, keyboard) = array(tuple((
        map(le_u8, |n| Note::try_from(n).unwrap()),
        map(ranged(le_u8, 0..=99), |s| match s { 0 => None, _ => Some((s-1).try_into().unwrap()) }),
    )))(input)?;
    let (input, volenv) = envelope(input)?;
    let (input, panenv) = envelope(input)?;
    let (input, pitchenv) = envelope(input)?;
    let (input, _dummy) = byte_array::<4>(input)?;

    let mut flags = InstrumentFlags::default();

    if dfp & InstrumentHeader::dfp_ignorePanning == 0 {
        flags |= InstrumentFlags::ENABLE_PANNING;
    }
    let dfp = dfp & !InstrumentHeader::dfp_ignorePanning;

    if ifc & InstrumentHeader::ifc_enableCutoff != 0 {
        flags |= InstrumentFlags::ENABLE_FILTER_CUTOFF;
    }
    let ifc = ifc & !InstrumentHeader::ifc_enableCutoff;

    if ifr & InstrumentHeader::ifr_enableResonance != 0 {
        flags |= InstrumentFlags::ENABLE_FILTER_RESONANCE;
    }
    let ifr = ifr & !InstrumentHeader::ifr_enableResonance;

    Ok((
        input,
        InstrumentHeader {
            name,
            filename,
            flags,
            new_note_action: nna,
            duplicate_check_type: dct,
            duplicate_check_action: dca,
            instrument_fadeout: fadeout.try_into().unwrap(),
            pitch_pan_separation: pps,
            pitch_pan_centre: ppc,
            global_volume: gbv,
            default_panning: dfp.try_into().unwrap(),
            random_volume_variation: rv.try_into().unwrap(),
            random_panning_variation: rp.try_into().unwrap(),
            trkver,
            number_of_samples: nos,
            initial_filter_cutoff: ifc.try_into().unwrap(),
            initial_filter_resonance: ifr.try_into().unwrap(),
            mch,
            mpr,
            mbank,
            keyboard: Box::new(keyboard),
            volume_envelope: volenv,
            panning_envelope: panenv,
            pitch_filter_envelope: pitchenv,
        },
    ))
}

fn envelope(input: &[u8]) -> IResult<&[u8], Envelope> {
    let (input, flags) = le_u8(input)?;
    let (input, num) = le_u8(input)?;
    let (input, lpb) = le_u8(input)?;
    let (input, lpe) = le_u8(input)?;
    let (input, slb) = le_u8(input)?;
    let (input, sle) = le_u8(input)?;
    let (input, data) = array(node)(input)?;
    let (input, _reserved) = le_u8(input)?;

    let flags = EnvelopeFlags::from_bits_truncate(flags);
    let data: [_; 25] = data;
    assert!(num <= 25);
    assert!(lpb <= 25);
    assert!(lpe <= 25 && lpb <= lpe);
    assert!(slb <= 25);
    assert!(sle <= 25 && slb <= sle);
    let nodes = Vec::from(&data[..(num as usize)]);

    Ok((
        input,
        Envelope {
            flags,
            loop_start: lpb,
            loop_end: lpe,
            sustain_loop_start: slb,
            sustain_loop_end: sle,
            nodes,
        },
    ))
}

fn node(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, value) = le_i8(input)?;
    let (input, tick) = le_u16(input)?;
    Ok((input, Node { value, tick }))
}

fn sample_header(input: &[u8]) -> IResult<&[u8], SampleHeader> {
    let (input, _) = tag(b"IMPS")(input)?;
    let (input, filename) = dosfilename(input)?;
    let (input, gvl) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, vol) = le_u8(input)?;
    let (input, name) = name(input)?;
    let (input, cvt) = le_u8(input)?;
    let (input, dfp) = le_u8(input)?;
    let (input, length) = le_u32(input)?;
    let (input, loopbegin) = le_u32(input)?;
    let (input, loopend) = le_u32(input)?;
    let (input, c5speed) = le_u32(input)?;
    let (input, susloopbegin) = le_u32(input)?;
    let (input, susloopend) = le_u32(input)?;
    let (input, samplepointer) = le_u32(input)?;
    let (input, vis) = le_u8(input)?;
    let (input, vid) = le_u8(input)?;
    let (input, vir) = le_u8(input)?;
    let (input, vit) = le_u8(input)?;
    Ok((
        input,
        SampleHeader {
            name,
            filename,
            flags: SampleFlags::from_parts(flags, cvt),
            global_volume: gvl,
            default_volume: vol,
            sample_panning: dfp,
            loop_start: loopbegin,
            loop_end: loopend,
            samplerate_c5: c5speed,
            sustain_loop_start: susloopbegin,
            sustain_loop_end: susloopend,
            data_offset: samplepointer,
            data_length: length,
            vibrato_speed: vis,
            vibrato_depth: vid,
            vibrato_rate: vir,
            vibrato_type: vit,
        },
    ))
}
