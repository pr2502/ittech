use crate::hl::{Command, Instrument, Note, Order, Pattern, Sample, Volume, IT};
use crate::ll::{DOSFilename, Envelope, ITFileHeader, InstrumentHeader, Name, Node, SampleHeader};
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::Error;
use nom::multi::{count, many_till};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::IResult;
use util::{array, byte_array, offset_list, ranged};

mod util;

fn is_set_bit(val: u8, bit: u8) -> bool {
    val & bit == bit
}

pub fn it(whole_input: &[u8]) -> Result<IT, nom::Err<Error<&[u8]>>> {
    let (input, header) = it_file_header(whole_input)?;
    let (input, orders) = count(order, header.ordnum as usize)(input)?;
    let (input, ins_offsets) = count(le_u32, header.insnum as usize)(input)?;
    let (input, sam_offsets) = count(le_u32, header.smpnum as usize)(input)?;
    let (_rest, pat_offsets) = count(le_u32, header.patnum as usize)(input)?;

    // Offsets are relative to the start of the file, use the whole input each time.
    let (_, instruments) = offset_list(instrument, ins_offsets)(whole_input)?;
    let (_, sample_headers) = offset_list(sample_header, sam_offsets)(whole_input)?;
    let patterns = {
        let mut patterns = Vec::with_capacity(pat_offsets.len());
        for offset in pat_offsets.into_iter().map(|o| o as usize) {
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
            assert!(!is_set_bit(header.flags, SampleHeader::flags_sampleStereo), "only mono samples supported");
            assert!(!is_set_bit(header.flags, SampleHeader::flags_sampleCompressed), "sample compression not supported");

            assert!(is_set_bit(header.cvt, SampleHeader::cvtSignedSample), "only signed samples are supported");
            assert!(!is_set_bit(header.cvt, SampleHeader::cvtDelta), "delta samples not supported");
            assert!(!is_set_bit(header.cvt, SampleHeader::cvtPTM8to16), "PTM loader is not supported");

            let data = if !is_set_bit(header.flags, SampleHeader::flags_sampleDataPresent) {
                None
            } else {
                let offset = header.samplepointer as usize;
                let length = header.length as usize;
                let input = &whole_input[offset..];
                let (_, data) = if is_set_bit(header.flags, SampleHeader::flags_sample16Bit) {
                    if is_set_bit(header.cvt, SampleHeader::cvtBigEndian) {
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
                samplerate_c5: header.C5Speed,
                data,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let message = {
        let offset = header.msgoffset as usize;
        if offset == 0 || offset >= whole_input.len() {
            String::new()
        } else {
            let (_, bytes) = take(header.msglength as usize)(whole_input)?;
            String::from_utf8_lossy(bytes)
                .to_string()
        }
    };

    Ok(IT {
        name: header.songname,
        highlight: (header.highlight_major, header.highlight_minor),
        made_with_version: header.cmwt,
        compatible_with_version: header.cwtv,
        flags: header.flags,
        special: header.special,
        global_volume: header.globalvol,
        sample_volume: header.mv,
        speed: header.speed,
        tempo: header.tempo,
        pan_separation: header.sep,
        pitch_wheel_depth: header.pwd,
        message,
        orders,
        init_channel_volume: header.chnvol,
        init_channel_panning: header.chnpan,
        instruments,
        samples,
        patterns,
    })
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

fn instrument(input: &[u8]) -> IResult<&[u8], Instrument> {
    let (input, header) = instrument_header(input)?;
    Ok((
        input,
        Instrument {
            name: header.name,
            filename: header.filename,
            nna: header.nna,
            dct: header.dct,
            dca: header.dca,
            fadeout: header.fadeout,
            pps: header.pps,
            ppc: header.ppc,
            gbv: header.gbv,
            dfp: header.dfp,
            rv: header.rv,
            rp: header.rp,
            trkver: header.trkver,
            nos: header.nos,
            ifc: header.ifc,
            ifr: header.ifr,
            mch: header.mch,
            mpr: header.mpr,
            mbank: header.mbank,
            keyboard: header.keyboard,
            volenv: header.volenv,
            panenv: header.panenv,
            pitchenv: header.pitchenv,
        },
    ))
}

fn pattern(input: &[u8]) -> IResult<&[u8], Pattern> {
    let (input, length) = le_u16(input)?;
    let (input, rows) = le_u16(input)?;
    let (input, _) = take(4usize)(input)?; // padding bytes
    let (_, input) = take(length)(input)?; // pattern size


    let mut last_maskvar = [0u8; 64];
    let mut last_note = [Note::Off; 64];
    let mut last_instrument = [0u8; 64];
    let mut last_volume = [Volume::SetVolume(0); 64];
    let mut last_command = [(0u8, 0u8); 64];

    // masks
    const LAST_MASKVAR: u8 = 128;
    const READ_NOTE: u8 = 1;
    const READ_INSTRUMENT: u8 = 2;
    const READ_VOLUME: u8 = 4;
    const READ_COMMAND: u8 = 8;
    const LAST_NOTE: u8 = 16;
    const LAST_INSTRUMENT: u8 = 32;
    const LAST_VOLUME: u8 = 64;
    const LAST_COMMAND: u8 = 128;

    let (input, rows) = all_consuming(count(
        map(
            many_till(
                |input| {
                    let (input, channel_var) = le_u8(input)?;

                    let channel = ((channel_var - 1) & 63) as usize;

                    let (input, mask_var) = if is_set_bit(channel_var, LAST_MASKVAR) {
                        let (input, mask_var) = le_u8(input)?;
                        last_maskvar[channel] = mask_var;
                        (input, mask_var)
                    } else {
                        (input, last_maskvar[channel])
                    };

                    let (input, note) = if is_set_bit(mask_var, READ_NOTE) && !is_set_bit(mask_var, LAST_NOTE) {
                        let (input, note_var) = le_u8(input)?;
                        let note = match note_var {
                            0..=119 => Note::Tone(note_var),
                            255 => Note::Off,
                            254 => Note::Cut,
                            _ => Note::Fade,
                        };
                        last_note[channel] = note;
                        (input, Some(note))
                    } else if is_set_bit(mask_var, LAST_NOTE) {
                        (input, Some(last_note[channel]))
                    } else {
                        (input, None)
                    };

                    let (input, instrument) = if is_set_bit(mask_var, READ_INSTRUMENT) && !is_set_bit(mask_var, LAST_INSTRUMENT) {
                        let (input, instrument) = ranged(le_u8, 0..=99)(input)?;
                        last_instrument[channel] = instrument;
                        (input, Some(instrument))
                    } else if is_set_bit(mask_var, LAST_INSTRUMENT) {
                        (input, Some(last_instrument[channel]))
                    } else {
                        (input, None)
                    };

                    let (input, volume) = if is_set_bit(mask_var, READ_VOLUME) && !is_set_bit(mask_var, LAST_VOLUME) {
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
                        last_volume[channel] = volume;
                        (input, Some(volume))
                    } else if is_set_bit(mask_var, LAST_VOLUME) {
                        (input, Some(last_volume[channel]))
                    } else {
                        (input, None)
                    };

                    let (input, command) = if is_set_bit(mask_var, READ_COMMAND) && !is_set_bit(mask_var, LAST_COMMAND) {
                        // TODO parse more
                        let (input, command) = tuple((ranged(le_u8, 0..=31), le_u8))(input)?;
                        last_command[channel] = command;
                        (input, Some(command))
                    } else if is_set_bit(mask_var, LAST_COMMAND) {
                        (input, Some(last_command[channel]))
                    } else {
                        (input, None)
                    };

                    let channel = channel as u8;
                    Ok((input, Command { channel, note, instrument, volume, command }))
                },
                tag(b"\0"),
            ),
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

// Helper macro for parsing struct by fields.
macro_rules! parse_struct {
    (
        $struct: ident;
        $( $field: ident = $parser: expr ),* $(,)?
    ) => {{
        (|input: &[u8]| -> IResult<&[u8], $struct> {
            $( let (input, $field) = $parser(input)?; )*
            Ok((
                input,
                $struct { $( $field ),* },
            ))
        }) as fn(&[u8]) -> IResult<&[u8], $struct>
    }};
}

pub fn it_file_header(input: &[u8]) -> IResult<&[u8], ITFileHeader> {
    let (input, _) = tag(ITFileHeader::magic)(input)?;
    parse_struct!(ITFileHeader;
        songname        = name,
        highlight_minor = le_u8,
        highlight_major = le_u8,
        ordnum          = le_u16,
        insnum          = le_u16,
        smpnum          = le_u16,
        patnum          = le_u16,
        cwtv            = le_u16,
        cmwt            = le_u16,
        flags           = le_u16,
        special         = le_u16,
        globalvol       = ranged(le_u8, 0..=128),
        mv              = ranged(le_u8, 0..=128),
        speed           = ranged(le_u8, 1..=255),
        tempo           = ranged(le_u8, 31..=255),
        sep             = ranged(le_u8, 0..=128),
        pwd             = le_u8,
        msglength       = le_u16,
        msgoffset       = le_u32,
        reserved        = le_u32,
        chnpan          = byte_array,
        chnvol          = byte_array,
    )(input)
}

pub fn instrument_header(input: &[u8]) -> IResult<&[u8], InstrumentHeader> {
    let (input, _) = tag(InstrumentHeader::magic)(input)?;
    parse_struct!(InstrumentHeader;
        filename  = dosfilename,
        nna       = le_u8,
        dct       = le_u8,
        dca       = le_u8,
        fadeout   = le_u16,
        pps       = le_i8,
        ppc       = le_u8,
        gbv       = le_u8,
        dfp       = le_u8,
        rv        = le_u8,
        rp        = le_u8,
        trkver    = le_u16,
        nos       = le_u8,
        reserved1 = le_u8,
        name      = name,
        ifc       = le_u8,
        ifr       = le_u8,
        mch       = le_u8,
        mpr       = le_u8,
        mbank     = byte_array,
        keyboard  = array(tuple((le_u8, le_u8))),
        volenv    = envelope,
        panenv    = envelope,
        pitchenv  = envelope,
        dummy     = byte_array,
    )(input)
}

fn envelope(input: &[u8]) -> IResult<&[u8], Envelope> {
    parse_struct!(Envelope;
        flags    = le_u8,
        num      = le_u8,
        lpb      = le_u8,
        lpe      = le_u8,
        slb      = le_u8,
        sle      = le_u8,
        data     = array(node),
        reserved = le_u8,
    )(input)
}

fn node(input: &[u8]) -> IResult<&[u8], Node> {
    parse_struct!(Node;
        value = le_i8,
        tick  = le_u16,
    )(input)
}

#[allow(non_snake_case)]
pub fn sample_header(input: &[u8]) -> IResult<&[u8], SampleHeader> {
    let (input, _) = tag(SampleHeader::magic)(input)?;
    parse_struct!(SampleHeader;
        filename      = dosfilename,
        gvl           = le_u8,
        flags         = le_u8,
        vol           = le_u8,
        name          = name,
        cvt           = le_u8,
        dfp           = le_u8,
        length        = le_u32,
        loopbegin     = le_u32,
        loopend       = le_u32,
        C5Speed       = le_u32,
        susloopbegin  = le_u32,
        susloopend    = le_u32,
        samplepointer = le_u32,
        vis           = le_u8,
        vid           = le_u8,
        vir           = le_u8,
        vit           = le_u8,
    )(input)
}
