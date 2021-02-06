use crate::data::*;
use bitflags::bitflags;
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{count, many_till};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::{Err, IResult};
use pattern::pattern;
use std::convert::{TryFrom, TryInto};
use util::*;


mod pattern;
mod util;


/// Parse Impulse Tracker module file (.it)
pub fn module<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> Result<Module, Err<E>> {
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
                    active_channels: ActiveChannels::empty(),
                    rows: vec![Vec::new(); 64]
                });
                continue
            }
            if offset >= input.len() {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::Eof)));
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
pub fn instrument<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> Result<Instrument, Err<E>> {
    // Save the whole input for offset parsing.
    let whole_input = input;

    let (input, header) = instrument_header(whole_input)?;
    let (_, _sample_headers) = count(sample_header, header.number_of_samples as usize)(input)?;
    todo!()
}

/// Parse Impulse Tracker sample file (.its)
pub fn sample<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> Result<Sample, Err<E>> {
    // Save the whole input for offset parsing.
    let whole_input = input;

    let (_, _header) = sample_header(whole_input)?;
    todo!()
}

fn sample_data<'i, E: ParseError<&'i [u8]>>(header: &SampleHeader, whole_input: &'i [u8]) -> Result<Option<Vec<f32>>, Err<E>> {
    let flags = header.flags;

    if !flags.contains(SampleFlags::DATA_PRESENT) {
        Ok(None)
    } else {
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

fn order<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Order, E> {
    let (input, byte) = le_u8(input)?;
    let order = match byte {
        0..=199 => Order::Index(byte.cast()),
        200..=253 => {
            // TODO ITTECH.TXT says only 0..=199 are allowed, but 200..=253 are not used for
            // anything else so we could parse them too.
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::Verify)));
        },
        254 => Order::Separator,
        255 => Order::EndOfSong,
    };
    Ok((input, order))
}

fn name<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Name, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, Name { bytes }))
}

fn dosfilename<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], DOSFilename, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, DOSFilename { bytes }))
}

fn instrument_header<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], InstrumentHeader, E> {
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

fn sample_map<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], SampleMap, E> {
    scan_count(
        120,
        tuple((ranged(le_u8, 0..=119), ranged(le_u8, 0..=99))),
        SampleMap::default(),
        |sm: &mut SampleMap, (note, sample)| {
            if sample >= 1 {
                let sample = SampleId::try_from(sample).unwrap();
                sm.map[note as usize] = Some(sample);
            }
        },
    )(input)
}

fn envelope<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Envelope, E> {
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

fn node<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Node, E> {
    let (input, value) = le_i8(input)?;
    let (input, tick) = le_u16(input)?;
    Ok((input, Node { value, tick }))
}

fn sample_header<'i, E: ParseError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], SampleHeader, E> {
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
