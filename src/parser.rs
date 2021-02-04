use crate::hl::{Command, Instrument, Module, ModuleFlags, Note, Order, Pattern, Sample, Volume};
use crate::ll::{DOSFilename, Envelope, InstrumentHeader, Name, Node, SampleFlags, SampleHeader};
use bitflags::bitflags;
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::Error;
use nom::multi::{count, many_till};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use util::{array, byte_array, offset_list, ranged};

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
            assert!(header.flags.contains(SampleFlags::DATA_SIGNED), "only signed samples are supported");
            assert!(!header.flags.contains(SampleFlags::STEREO), "only mono samples supported");

            assert!(!header.flags.contains(SampleFlags::COMPRESSED), "sample compression is not supported");
            assert!(!header.flags.contains(SampleFlags::OPL_INSTRUMENT), "OPL instrument is not supported");
            assert!(!header.flags.contains(SampleFlags::EXTERNAL_SAMPLE), "external samples are not supported");
            assert!(!header.flags.contains(SampleFlags::ADPCM_SAMPLE), "MODPlugin :(");
            assert!(!header.flags.contains(SampleFlags::DELTA), "delta samples are not supported");
            assert!(!header.flags.contains(SampleFlags::PTM8_TO_16), "PTM loader is not supported");

            let data = if !header.flags.contains(SampleFlags::DATA_PRESENT) {
                None
            } else {
                let offset = header.samplepointer as usize;
                let length = header.length as usize;
                let input = &whole_input[offset..];
                let (_, data) = if header.flags.contains(SampleFlags::DATA_16BIT) {
                    if header.flags.contains(SampleFlags::DATA_BIG_ENDIAN) {
                        count(map(be_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?
                    } else {
                        count(map(le_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?
                    }
                } else {
                    count(map(le_i8, |s| f32::from(s) / f32::from(i8::MAX)), length)(input)?
                };
                Some(data)
            };

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
        global_volume: globalvol,
        sample_volume: mv,
        speed,
        tempo,
        pan_separation: sep,
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
pub fn instrument(whole_input: &[u8]) -> Result<Instrument, nom::Err<Error<&[u8]>>> {
    let (input, header) = instrument_header(whole_input)?;
    let (_, _sample_headers) = count(sample_header, header.nos as usize)(input)?;
    todo!()
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
        struct Channel: u8 {
            const LAST_MASKVAR = 1 << 7;
        }
    }

    struct State {
        last_maskvar: [Mask; 64],
        last_note: [Note; 64],
        last_instrument: [u8; 64],
        last_volume: [Volume; 64],
        last_command: [(u8, u8); 64],
    }

    impl Default for State {
        fn default() -> State {
            State {
                last_maskvar: [Mask::EMPTY; 64],
                last_note: [Note::Off; 64],
                last_instrument: [0u8; 64],
                last_volume: [Volume::SetVolume(0); 64],
                last_command: [(0u8, 0u8); 64],
            }
        }
    }

    fn command(state: &mut State) -> impl FnMut(&[u8]) -> IResult<&[u8], Command> + '_ {
        move |input| {
            let (input, channel_var) = le_u8(input)?;

            let channel_mask = Channel::from_bits_truncate(channel_var);
            let channel = ((channel_var - 1) & 0b0011_1111) as usize;

            let (input, mask_var) = if channel_mask.contains(Channel::LAST_MASKVAR) {
                let (input, mask_var) = le_u8(input)?;
                let mask_var = Mask::from_bits_truncate(mask_var);
                state.last_maskvar[channel] = mask_var;
                (input, mask_var)
            } else {
                (input, state.last_maskvar[channel])
            };

            let (input, note) = if mask_var.contains(Mask::READ_NOTE) && !mask_var.contains(Mask::LAST_NOTE) {
                let (input, note_var) = le_u8(input)?;
                let note = match note_var {
                    0..=119 => Note::Tone(note_var),
                    255 => Note::Off,
                    254 => Note::Cut,
                    _ => Note::Fade,
                };
                state.last_note[channel] = note;
                (input, Some(note))
            } else if mask_var.contains(Mask::LAST_NOTE) {
                (input, Some(state.last_note[channel]))
            } else {
                (input, None)
            };

            let (input, instrument) = if mask_var.contains(Mask::READ_INSTRUMENT) && !mask_var.contains(Mask::LAST_INSTRUMENT) {
                let (input, instrument) = ranged(le_u8, 0..=99)(input)?;
                state.last_instrument[channel] = instrument;
                (input, Some(instrument))
            } else if mask_var.contains(Mask::LAST_INSTRUMENT) {
                (input, Some(state.last_instrument[channel]))
            } else {
                (input, None)
            };

            let (input, volume) = if mask_var.contains(Mask::READ_VOLUME) && !mask_var.contains(Mask::LAST_VOLUME) {
                let (input, byte) = le_u8(input)?;
                let volume = match byte {
                    0..=64 => Volume::SetVolume(byte),
                    128..=192 => Volume::Panning(byte - 128),
                    65..=74 => Volume::FineVolumeUp(byte - 65),
                    75..=84 => Volume::FineVolumeDown(byte - 75),
                    85..=94 => Volume::VolumeSlideUp(byte - 85),
                    95..=104 => Volume::VolumeSlideDown(byte - 95),
                    105..=114 => Volume::PitchSlideDown(byte - 105),
                    115..=124 => Volume::PitchSlideUp(byte - 115),
                    193..=202 => Volume::PortamentoTo(byte - 193),
                    203..=212 => Volume::Vibrato(byte - 203),
                    _ => return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Verify))),
                };
                state.last_volume[channel] = volume;
                (input, Some(volume))
            } else if mask_var.contains(Mask::LAST_VOLUME) {
                (input, Some(state.last_volume[channel]))
            } else {
                (input, None)
            };

            let (input, command) = if mask_var.contains(Mask::READ_COMMAND) && !mask_var.contains(Mask::LAST_COMMAND) {
                // TODO parse more
                let (input, command) = tuple((ranged(le_u8, 0..=31), le_u8))(input)?;
                state.last_command[channel] = command;
                (input, Some(command))
            } else if mask_var.contains(Mask::LAST_COMMAND) {
                (input, Some(state.last_command[channel]))
            } else {
                (input, None)
            };

            let channel = channel as u8;
            Ok((input, Command { channel, note, instrument, volume, command }))
        }
    }

    let (input, rows) = all_consuming(count(
        map(
            many_till(command(&mut State::default()), tag(b"\0")),
            |(mut commands, _)| {
                commands.sort_unstable_by_key(|cmd| cmd.channel);
                commands
            },
        ),
        rows as usize,
    ))(input)?;
    Ok((input, Pattern { rows }))
}

fn name(input: &[u8]) -> IResult<&[u8], Name> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, Name { bytes }))
}

fn dosfilename(input: &[u8]) -> IResult<&[u8], DOSFilename> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, DOSFilename { bytes }))
}

pub fn instrument_header(input: &[u8]) -> IResult<&[u8], InstrumentHeader> {
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
    let (input, keyboard) = array(tuple((le_u8, le_u8)))(input)?;
    let (input, volenv) = envelope(input)?;
    let (input, panenv) = envelope(input)?;
    let (input, pitchenv) = envelope(input)?;
    let (input, dummy) = byte_array(input)?;
    Ok((
        input,
        InstrumentHeader {
            filename,
            nna,
            dct,
            dca,
            fadeout,
            pps,
            ppc,
            gbv,
            dfp,
            rv,
            rp,
            trkver,
            nos,
            name,
            ifc,
            ifr,
            mch,
            mpr,
            mbank,
            keyboard,
            volenv,
            panenv,
            pitchenv,
            dummy,
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
    let (input, reserved) = le_u8(input)?;
    Ok((
        input,
        Envelope {
            flags,
            num,
            lpb,
            lpe,
            slb,
            sle,
            data,
            reserved,
        },
    ))
}

fn node(input: &[u8]) -> IResult<&[u8], Node> {
    let (input, value) = le_i8(input)?;
    let (input, tick) = le_u16(input)?;
    Ok((input, Node { value, tick }))
}

pub fn sample_header(input: &[u8]) -> IResult<&[u8], SampleHeader> {
    let (input, _) = tag(b"IMPS")(input)?;
    let (input, filename) = dosfilename(input)?;
    let (input, gvl) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;//, SampleFlags::from_bits_truncate)(input)?;
    let (input, vol) = le_u8(input)?;
    let (input, name) = name(input)?;
    let (input, cvt) = le_u8(input)?;// CvtFlags::from_bits_truncate)(input)?;
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
    let flags = SampleFlags::from_parts(flags, cvt);
    Ok((
        input,
        SampleHeader {
            name,
            filename,
            gvl,
            flags,
            vol,
            dfp,
            length,
            loopbegin,
            loopend,
            samplerate_c5: c5speed,
            susloopbegin,
            susloopend,
            samplepointer,
            vibrato_speed: vis,
            vibrato_depth: vid,
            vibrato_rate: vir,
            vibrato_type: vit,
        },
    ))
}
