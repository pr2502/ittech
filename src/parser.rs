//! Parsing functions

use crate::data::*;
use crate::error::ContextError;
use bitflags::bitflags;
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::{ErrorKind, ParseError};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::{Err, IResult};
use pattern::pattern;
use std::convert::{TryFrom, TryInto};


mod pattern;
pub(crate) mod scan;
mod util;

pub use pattern::parse_effect as effect;

use util::*;
pub use scan::scan;


/// Parse Impulse Tracker module file (.it)
pub fn module<'i, E>(input: &'i [u8]) -> Result<Module, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]> + 'i,
{
    let (_, header) = module_header(input)?;

    // Offsets are relative to the start of the file, use the whole input every time.
    let (_, instrument_headers) = offset_list(instrument_header, header.instrument_offsets)(input)?;
    let (_, sample_headers) = offset_list(sample_header, header.sample_offsets)(input)?;
    let patterns = {
        let mut patterns = Vec::with_capacity(header.pattern_offsets.len());
        for offset in header.pattern_offsets.into_iter().map(|o| o as usize) {
            // Pattern parsing is inlined from `offset_list` because we need to handle the special
            // case of offset 0 here.
            if offset == 0 {
                patterns.push(Pattern {
                    active_channels: ActiveChannels::empty(),
                    rows: vec![Row::empty(); 64]
                });
                continue
            }
            if offset >= input.len() {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)));
            }
            let (_, pat) = pattern(&input[offset..])?;
            patterns.push(pat);
        }
        patterns
    };

    let samples = sample_headers.into_iter()
        .map(|header| sample_data(header, input))
        .collect::<Result<Vec<_>, _>>()?;

    let message = {
        let offset = header.message_offset as usize;
        if offset == 0 || offset >= input.len() {
            String::new()
        } else {
            let (_, bytes) = take(header.message_length as usize)(input)?;
            String::from_utf8_lossy(bytes)
                .to_string()
        }
    };

    Ok(Module {
        name: header.name,
        highlight: header.highlight,
        made_with_version: header.made_with_version,
        compatible_with_version: header.compatible_with_version,
        flags: header.flags,
        global_volume: header.global_volume,
        sample_volume: header.sample_volume,
        speed: header.speed,
        tempo: header.tempo,
        pan_separation: header.pan_separation,
        pitch_wheel_depth: header.pitch_wheel_depth,
        message,
        orders: header.orders,
        init_channel_panning: header.init_channel_panning,
        init_channel_volume: header.init_channel_volume,
        instruments: instrument_headers,
        samples,
        patterns,
    })
}

/// Parse Impulse Tracker instrument file (.iti)
pub fn instrument<'i, E>(input: &'i [u8]) -> Result<Instrument, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let (input2, header) = instrument_header(input)?;
    let (_, sample_headers) = count(sample_header, header.number_of_samples as usize)(input2)?;
    let samples = sample_headers.into_iter()
        .map(|header| sample_data(header, input))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Instrument { header, samples })
}

/// Parse Impulse Tracker sample file (.its)
pub fn sample<'i, E>(input: &'i [u8]) -> Result<Sample, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let (_, header) = sample_header(input)?;
    sample_data(header, input)
}


fn module_header<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], ModuleHeader, E>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    // Parse static parts.
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
    let orders = orders.into_iter().filter_map(|x|x).collect();
    let (input, ins_offsets) = count(le_u32, insnum as usize)(input)?;
    let (input, sam_offsets) = count(le_u32, smpnum as usize)(input)?;
    let (_rest, pat_offsets) = count(le_u32, patnum as usize)(input)?;

    let flags = ModuleFlags::from_parts(flags, special);

    Ok((
        input,
        ModuleHeader {
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
            message_length: msglength,
            message_offset: msgoffset,
            init_channel_panning: chnpan,
            init_channel_volume: chnvol,
            orders,
            instrument_offsets: ins_offsets,
            sample_offsets: sam_offsets,
            pattern_offsets: pat_offsets,
        },
    ))
}

fn order<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Option<Order>, E> {
    map(
        le_u8,
        |byte| match byte {
            0 ..= 199 => Some(Order::Index(byte.cast())),
            254 => Some(Order::Separator),
            255 => Some(Order::EndOfSong),
            // Invalid values get skipped.
            // TODO log errors
            _ => None,
        },
    )(input)
}

fn name<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Name, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, Name { bytes }))
}

fn dosfilename<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], DOSFilename, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, DOSFilename { bytes }))
}

fn instrument_header<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], InstrumentHeader, E> {
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
    let (input, sample_map) = sample_map(input)?;
    let (input, volenv) = envelope(input)?;
    let (input, panenv) = envelope(input)?;
    let (input, pitchenv) = envelope(input)?;
    let (input, _dummy) = byte_array::<_, 4>(input)?;

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
            sample_map,
            volume_envelope: volenv,
            panning_envelope: panenv,
            pitch_filter_envelope: pitchenv,
        },
    ))
}

fn sample_map<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], SampleMap, E> {
    scan_count(
        120,
        tuple((ranged(le_u8, 0..=119), ranged(le_u8, 0..=99))),
        SampleMap::default(),
        |sm: &mut SampleMap, (note, sample)| {
            if sample > 0 {
                let sample = SampleId::try_from(sample - 1).unwrap();
                sm.map[note as usize] = Some(sample);
            }
        },
    )(input)
}

fn envelope<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Envelope, E> {
    let (input, flags) = le_u8(input)?;
    let (input, num) = context!(ranged(le_u8, 0..=25), "reading envelope size")(input)?;
    let (input, lpb) = le_u8(input)?;
    let (input, lpe) = le_u8(input)?;
    let (input, slb) = le_u8(input)?;
    let (input, sle) = le_u8(input)?;
    let (input, data) = array(node)(input)?;
    let (input, _reserved) = le_u8(input)?;

    let flags = EnvelopeFlags::from_bits_truncate(flags);
    let data: [_; 25] = data;
    assert!(num <= 25);
    assert!(lpb <= lpe);
    assert!(lpe < num);
    assert!(slb <= sle);
    assert!(sle < num);
    let nodes = Vec::from(&data[..(num as usize)]);

    Ok((
        input,
        Envelope {
            flags,
            loop_: EnvelopeLoop { start: lpb, end: lpe },
            sustain_loop: EnvelopeLoop { start: slb, end: sle },
            nodes,
        },
    ))
}

fn node<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Node, E> {
    let (input, value) = le_i8(input)?;
    let (input, tick) = le_u16(input)?;
    Ok((input, Node { value, tick }))
}

fn sample_header<'i, E>(input: &'i [u8]) -> IResult<&'i [u8], SampleHeader, E>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let (input, _) = tag(b"IMPS")(input)?;
    let (input, filename) = dosfilename(input)?;
    let (input, gvl) = le_u8(input)?;
    let (input, flags) = le_u8(input)?;
    let (input, vol) = le_u8(input)?;
    let (input, name) = name(input)?;
    let (input, cvt) = le_u8(input)?;

    let flags = SampleFlags::from_parts(flags, cvt);

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

    let loop_ = if flags.contains(SampleFlags::LOOP) {
        assert!(loopbegin < loopend);
        assert!(loopend <= length);
        Some(SampleLoop {
            start: loopbegin,
            end: loopend,
            bidi: flags.contains(SampleFlags::BIDI_LOOP),
        })
    } else {
        None
    };

    let sustain_loop = if flags.contains(SampleFlags::SUSTAIN) {
        assert!(susloopbegin < susloopend);
        assert!(susloopend <= length);
        Some(SampleLoop {
            start: susloopbegin,
            end: susloopend,
            bidi: flags.contains(SampleFlags::BIDI_SUSTAIN),
        })
    } else {
        None
    };

    Ok((
        input,
        SampleHeader {
            name,
            filename,
            global_volume: gvl,
            default_volume: vol,
            default_panning: dfp,
            loop_,
            sustain_loop,
            samplerate_c5: c5speed,
            vibrato_speed: vis,
            vibrato_depth: vid,
            vibrato_rate: vir,
            vibrato_type: vit,

            flags,
            data_offset: samplepointer,
            data_length: length,
        },
    ))
}

fn sample_data<'i, E>(header: SampleHeader, input: &'i [u8]) -> Result<Sample, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let flags = header.flags;

    let data = if !flags.contains(SampleFlags::DATA_PRESENT) {
        None
    } else {
        // TODO add support for more sample formats

        assert!(flags.contains(SampleFlags::DATA_SIGNED), "only signed samples are supported");
        assert!(!flags.contains(SampleFlags::STEREO), "only mono samples supported");

        assert!(!flags.contains(SampleFlags::COMPRESSED), "sample compression is not supported");
        assert!(!flags.contains(SampleFlags::OPL_INSTRUMENT), "OPL instrument is not supported");
        assert!(!flags.contains(SampleFlags::EXTERNAL_SAMPLE), "external samples are not supported");
        assert!(!flags.contains(SampleFlags::ADPCM_SAMPLE), "MODPlugin :(");
        assert!(!flags.contains(SampleFlags::DELTA), "delta samples are not supported");
        assert!(!flags.contains(SampleFlags::PTM8_TO_16), "PTM loader is not supported");

        let offset = header.data_offset as usize;
        let length = header.data_length as usize;
        let input = &input[offset..];

        let (_, data) = match (
            flags.contains(SampleFlags::DATA_16BIT),
            flags.contains(SampleFlags::DATA_BIG_ENDIAN),
        ) {
            (true, true) => count(map(be_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?,
            (true, false) => count(map(le_i16, |s| f32::from(s) / f32::from(i16::MAX)), length)(input)?,
            (false, _) => count(map(le_i8, |s| f32::from(s) / f32::from(i8::MAX)), length)(input)?,
        };

        Some(data)
    };

    Ok(Sample {
        name: header.name,
        filename: header.filename,
        global_volume: header.global_volume,
        default_volume: header.default_volume,
        default_panning: header.default_panning,
        loop_: header.loop_,
        sustain_loop: header.sustain_loop,
        samplerate_c5: header.samplerate_c5,
        vibrato_speed: header.vibrato_speed,
        vibrato_depth: header.vibrato_depth,
        vibrato_rate: header.vibrato_rate,
        vibrato_type: header.vibrato_type,
        data,
    })
}
