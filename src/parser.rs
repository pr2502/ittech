//! Parsing functions

use crate::data::*;
use crate::error::ContextError;
use bitflags::bitflags;
use nom::bytes::complete::{tag, take};
use nom::combinator::{all_consuming, map};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{count, many_till};
use nom::number::complete::{be_i16, le_i16, le_i8, le_u16, le_u32, le_u8};
use nom::sequence::tuple;
use nom::{Err, IResult};
use pattern::pattern;
use std::cmp::{min, max};
use std::convert::{TryFrom, TryInto};
use std::num::Wrapping;
use std::ops::{RangeInclusive, Add};


macro_rules! info {
    ( $($tt:tt)* ) => {
        #[cfg(feature = "tracing")]
        ::tracing::info!($($tt)*);
    };
}


mod pattern;
pub(crate) mod scan;
mod util;

pub use pattern::parse_effect as effect;

use util::*;
pub use scan::scan;


/// Parse Impulse Tracker module file (.it)
pub fn module_file<'i, E>(input: &'i [u8]) -> Result<Module, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]> + 'i,
{
    let (_, header) = module_header(input)?;

    // Offsets are relative to the start of the file, use the whole input every time.
    let (_, instruments) = offset_list(instrument, header.instrument_offsets)(input)?;
    let (_, sample_headers) = offset_list(sample_header, header.sample_offsets)(input)?;
    let patterns = {
        let mut patterns = Vec::with_capacity(header.pattern_offsets.len());
        for offset in header.pattern_offsets.into_iter().map(<_>::cast) {
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
        let offset = header.message_offset.cast::<usize>();
        if offset == 0 || offset >= input.len() {
            String::new()
        } else {
            let (_, bytes) = take(header.message_length.cast::<usize>())(&input[offset..])?;
            //according to ITTECH.TXT, a \0 is always the end of a message
            let bytes_before_terminator = bytes.split(|&x| x == b'\0').next().unwrap();
            String::from_utf8_lossy(bytes_before_terminator)
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
        instruments,
        samples,
        patterns,
    })
}

/// Parse Impulse Tracker instrument file (.iti)
pub fn instrument_file<'i, E>(input: &'i [u8]) -> Result<InstrumentFile, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let (input2, instrument) = instrument(input)?;
    let (_, sample_headers) = count(sample_header, instrument.number_of_samples.into())(input2)?;
    let samples = sample_headers.into_iter()
        .map(|header| sample_data(header, input))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(InstrumentFile { instrument, samples })
}

/// Parse Impulse Tracker sample file (.its)
pub fn sample_file<'i, E>(input: &'i [u8]) -> Result<Sample, Err<E>>
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
    let (input, globalvol) = le_u8(input)?;
    let (input, mv) = le_u8(input)?;
    let (input, speed) = le_u8(input)?;
    let (input, tempo) = le_u8(input)?;
    let (input, sep) = le_u8(input)?;
    let (input, pwd) = le_u8(input)?;
    let (input, msglength) = le_u16(input)?;
    let (input, msgoffset) = le_u32(input)?;
    let (input, _reserved) = le_u32(input)?;
    let (input, chnpan) = byte_array(input)?;
    let (input, chnvol) = byte_array(input)?;

    // Parse dynamic parts of the header.
    let (input, orders) = count(order, ordnum.into())(input)?;
    let orders = orders.into_iter().flatten().collect();
    let (input, ins_offsets) = count(le_u32, insnum.into())(input)?;
    let (input, sam_offsets) = count(le_u32, smpnum.into())(input)?;
    let (_rest, pat_offsets) = count(le_u32, patnum.into())(input)?;

    let flags = ModuleFlags::from_parts(flags, special);

    // Check ranged values and canonicalize out-of-range values.
    fn ranged(value: u8, range: RangeInclusive<u8>, or_else: impl FnOnce(u8) -> u8) -> u8 {
        if range.contains(&value) {
            value
        } else {
            let value = or_else(value);
            assert!(range.contains(&value), "BUG: fallback value is also out of range");
            value
        }
    }
    let globalvol = ranged(globalvol, 0..=128, |_| {
        info!(globalvol, "global_volume cannot be more than 128, clipping");
        128
    });
    let mv = ranged(mv, 0..=128, |_| {
        info!(mv, "sample_volume cannot be more than 128, clipping");
        128
    });
    let speed = ranged(speed, 1..=255, |_| {
        info!("speed must be at least 1, using default of 6");
        6
    });
    let tempo = ranged(tempo, 31..=255, |_| {
        info!("tempo must be at least 31, using default of 120");
        120
    });
    let sep = ranged(sep, 0..=128, |_| {
        info!("pan_separation cannot be more than 128, clipping");
        128
    });

    Ok((
        input,
        ModuleHeader {
            name: songname,
            highlight: (highlight_major, highlight_minor),
            made_with_version: cwtv,
            compatible_with_version: cmwt,
            flags,
            global_volume: globalvol.cast(),
            sample_volume: mv.cast(),
            speed: speed.cast(),
            tempo: tempo.cast(),
            pan_separation: sep.cast(),
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
        |value| match value {
            0 ..= 199 => Some(Order::Index(value.cast())),
            254 => Some(Order::Separator),
            255 => Some(Order::EndOfSong),
            // Invalid values get skipped.
            _ => {
                info!(value, "order value is out of range 0..=199,254,255, skipping");
                None
            }
        },
    )(input)
}

fn name<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Name, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, Name { bytes }))
}

fn dosfilename<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], DosFilename, E> {
    let (input, bytes) = byte_array(input)?;
    Ok((input, DosFilename { bytes }))
}

fn instrument<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Instrument, E> {
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

    if dfp & Instrument::dfp_ignorePanning == 0 {
        flags |= InstrumentFlags::ENABLE_PANNING;
    }
    let dfp = dfp & !Instrument::dfp_ignorePanning;

    if ifc & Instrument::ifc_enableCutoff != 0 {
        flags |= InstrumentFlags::ENABLE_FILTER_CUTOFF;
    }
    let ifc = ifc & !Instrument::ifc_enableCutoff;

    if ifr & Instrument::ifr_enableResonance != 0 {
        flags |= InstrumentFlags::ENABLE_FILTER_RESONANCE;
    }
    let ifr = ifr & !Instrument::ifr_enableResonance;

    Ok((
        input,
        Instrument {
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
        tuple((le_u8, le_u8)),
        SampleMap::default(),
        |sm: &mut SampleMap, (note, sample)| {
            match (note, sample) {
                (0..=119, 0) => {
                    // Explicit map to None, but don't override previous mapping.
                }
                (0..=119, 1..=99) => {
                    let sample = SampleId::try_from(sample - 1).unwrap();
                    sm.map[usize::from(note)] = Some(sample);
                }
                _ => {
                    info!(
                        note, sample,
                        "note or sample out of range 0..=119 and 0..=99 respectively"
                    );
                }
            }
        },
    )(input)
}

fn envelope<'i, E: ParseError<&'i [u8]> + ContextError<&'i [u8]>>(input: &'i [u8]) -> IResult<&'i [u8], Envelope, E> {
    let (input, flags) = le_u8(input)?;
    let (input, num) = le_u8(input)?;
    let (input, lpb) = le_u8(input)?;
    let (input, lpe) = le_u8(input)?;
    let (input, slb) = le_u8(input)?;
    let (input, sle) = le_u8(input)?;
    let (input, data): (_, [_; 25]) = array(node)(input)?;
    let (input, _reserved) = le_u8(input)?;

    let envelope_loop;
    let sustain_loop;
    let num = if num > 25 {
        info!(len = num, "envelope size out of range 0..=25, using 0");
        envelope_loop = None;
        sustain_loop = None;
        0
    } else {
        envelope_loop = if lpb <= lpe && lpe < num {
            Some(EnvelopeLoop { start: lpb, end: lpe })
        } else {
            info!(
                start = lpb, end = lpe, len = num,
                "invalid loop points, ignoring envelope loop",
            );
            None
        };
        sustain_loop = if slb <= sle && sle < num {
            Some(EnvelopeLoop { start: slb, end: sle })
        } else {
            info!(
                start = lpb, end = lpe, len = num,
                "invalid loop points, ignoring sustain loop",
            );
            None
        };
        num
    };

    let flags = EnvelopeFlags::from_bits_truncate(flags);
    let nodes = Vec::from(&data[..usize::from(num)]);

    Ok((
        input,
        Envelope {
            flags,
            envelope_loop,
            sustain_loop,
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
        // TODO canonicalize/skip invalid values
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
        // TODO canonicalize/skip invalid values
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



// used as reference for sample decompression:
// https://github.com/nicolasgramlich/AndEngineMODPlayerExtension/blob/master/jni/loaders/itsex.c
// https://github.com/B0ney/xmodits/blob/main/xmodits-lib-old/src/fmt/compression.rs

// This section of code looks the way it looks because an integer with the bits
// cba98 76543210
// may be stored like this:
// 210##### a9876543 ######cb
// Next time you see Jeffrey Lim, tell him that he should feel very bad

type BitInput<'a> = (&'a [u8], usize);

fn get_bit(mut input: BitInput) -> IResult<BitInput, usize> {
    let bit = (input.0.first().unwrap() >> input.1) & 1;
    input.1 += 1;
    if input.1 == 8 {
        input.1 = 0;
        input.0 = &input.0[1..];
    }
    Ok((input, bit.into()))
}

fn get_bits(mut input: BitInput, bits: usize) -> IResult<BitInput, usize> {
    let mut result: usize = 0;
    for i in 0..bits {
        let bit;
        (input, bit) = get_bit(input)?;
        result |= bit << i;
    }
    Ok((input, result))
}

fn integrate_with_wrap<T>(state: &mut Wrapping<T>, x: T) -> Option<T> 
where 
    T: Copy, 
    Wrapping<T>: Add<Output = Wrapping<T>>
{
    *state = *state + Wrapping(x);
    Some((*state).0)
}

trait SampleValue {
    fn normalize(&self) -> f32;
    fn bits() -> usize;
    fn bits_log2() -> usize;
    fn as_signed_int(unsigned: usize) -> Self;
}

impl SampleValue for i8 {
    fn normalize(&self) -> f32 {
        *self as f32 / -f32::from(Self::MIN)
    }
    fn bits() -> usize {
        8
    }
    fn bits_log2() -> usize {
        3
    }
    fn as_signed_int(unsigned: usize) -> Self {
        unsigned as Self
    }
}

impl SampleValue for i16 {
    fn normalize(&self) -> f32 {
        *self as f32 / -f32::from(Self::MIN)
    }
    fn bits() -> usize {
        16
    }
    fn bits_log2() -> usize {
        4
    }
    fn as_signed_int(unsigned: usize) -> Self {
        unsigned as Self
    }
}

fn decompress_block<'i, T, E>(input: &'i [u8], samples: usize, delta: bool) -> Result<Vec<f32>, Err<E>> 
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
    T: SampleValue + std::ops::Shr<i32, Output = T> + Copy + Default, 
    Wrapping<T>: Add<Output = Wrapping<T>>
{
    let mut decompressed_block: Vec<T> = Vec::with_capacity(samples);
    let mut input: BitInput = (input, 0);
    let mut bits_per_sample:usize = T::bits() + 1;
    while decompressed_block.len() < samples {
        let sample_value: usize;
        (input, sample_value) = get_bits(input, bits_per_sample).unwrap();
        if let Some(new_bits_per_sample) = match bits_per_sample {
            1..=6 => {
                // only msb set?
                (sample_value == (1 << (bits_per_sample - 1))).then(|| {
                    let mut encoded_new_bps;
                    (input, encoded_new_bps) = get_bits(input, T::bits_log2()).unwrap();
                    encoded_new_bps += 1;
                    if encoded_new_bps < bits_per_sample {encoded_new_bps} else {encoded_new_bps + 1}
                })
            },
            7.. if bits_per_sample <= T::bits() => {
                // the values around 10000... are reserved for bps changes
                let new_bps_range_size = T::bits();
                let new_bps_range_start: usize = (1 << (bits_per_sample - 1)) - (new_bps_range_size / 2);
                (new_bps_range_start..new_bps_range_start + new_bps_range_size)
                    .contains(&sample_value).then(|| {
                        let encoded_new_bps = (sample_value - new_bps_range_start) + 1;
                        if encoded_new_bps < bits_per_sample {encoded_new_bps} else {encoded_new_bps + 1}
                    })
            }
            _ if bits_per_sample == (T::bits() + 1) => {
                // msb set?
                ((sample_value >> T::bits()) == 1).then(||{
                    (sample_value & 0xff) + 1
                })
            }
            _ => unreachable!()
        } {
            assert!((1..=(T::bits() + 1)).contains(&new_bits_per_sample), "broken sample, invalid bits/sample value: {}, old value: {}", new_bits_per_sample, bits_per_sample);
            bits_per_sample = new_bits_per_sample;
            continue;
        }
        // shifting is necessary because the msb is the sign bit, not bit 7/15
        let shift = max(T::bits() as i32 - bits_per_sample as i32, 0);
        let signed_sample_value = T::as_signed_int(sample_value << shift) >> shift;
        decompressed_block.push(signed_sample_value);
    }

    // integrate twice if the delta flag is set, otherwise once
    Ok(
        decompressed_block.into_iter()
            .scan(Wrapping(T::default()), integrate_with_wrap::<T>)
            .scan(Wrapping(T::default()), |state, x| if delta {integrate_with_wrap(state, x)} else {Some(x)})
            .map(|x| x.normalize())
            .collect()
    )
}

fn decompress<'i, T, E>(mut input: &'i [u8], length: usize, delta: bool) -> Result<Vec<f32>, Err<E>> 
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>, 
    T: SampleValue + std::ops::Shr<i32, Output = T> + Default + Copy,
    Wrapping<T>: Add<Output = Wrapping<T>>
{
    let mut decompressed_sample: Vec<f32> = Vec::with_capacity(length);
    while decompressed_sample.len() < length {
        let block_data_length;
        (input, block_data_length) = le_u16(input)?;
        let block_data: &[u8];
        (input, block_data) = take(block_data_length)(input)?;
        const BLOCK_SAMPLES_MAX_BYTE_LENGTH: usize = 0x8000;
        let block_samples = min(length - decompressed_sample.len(), BLOCK_SAMPLES_MAX_BYTE_LENGTH / (T::bits() / 8));
        decompressed_sample.append(&mut decompress_block::<T, _>(block_data, block_samples, delta)?)
    }
    Ok(decompressed_sample)
}

fn sample_data<'i, E>(header: SampleHeader, input: &'i [u8]) -> Result<Sample, Err<E>>
where
    E: ParseError<&'i [u8]> + ContextError<&'i [u8]>,
{
    let flags = header.flags;

    let data = if !flags.contains(SampleFlags::DATA_PRESENT) {
        None
    } else {
        // TODO add support for more sample formats, do not panic
        assert!(flags.contains(SampleFlags::DATA_SIGNED), "only signed samples are supported");
        assert!(!flags.contains(SampleFlags::STEREO), "only mono samples supported");

        assert!(!flags.contains(SampleFlags::OPL_INSTRUMENT), "OPL instrument is not supported");
        assert!(!flags.contains(SampleFlags::EXTERNAL_SAMPLE), "external samples are not supported");
        assert!(!flags.contains(SampleFlags::ADPCM_SAMPLE), "MODPlugin :(");
        assert!(!(flags.contains(SampleFlags::DELTA) && !flags.contains(SampleFlags::COMPRESSED)), "delta samples without compression are not supported");
        assert!(!flags.contains(SampleFlags::PTM8_TO_16), "PTM loader is not supported");

        let offset = header.data_offset.cast();
        let length = header.data_length.cast();
        let input = &input[offset..];

        let data = match (
            flags.contains(SampleFlags::DATA_16BIT),
            flags.contains(SampleFlags::DATA_BIG_ENDIAN),
            flags.contains(SampleFlags::COMPRESSED)
        ) {
            (true, true, false) => count(map(be_i16, |s| s.normalize()), length)(input)?.1,
            (true, false, false) => count(map(le_i16, |s| s.normalize()), length)(input)?.1,
            (false, _, false) => count(map(le_i8, |s| s.normalize()), length)(input)?.1,
            (true, false, true) => decompress::<i16, _>(input, length, flags.contains(SampleFlags::DELTA))?,
            (false, _, true) => decompress::<i8, _>(input, length, flags.contains(SampleFlags::DELTA))?,
            (true, true, true) => todo!("compressed 16 bit big endian samples not supported")
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
    fn compressed_samples(){
        const COMPRESSED_INST_DATA: &[u8] = include_bytes!("../tests/compression/compressed.iti");
        const SAMPLE_8_DATA: &[u8] = include_bytes!("../tests/compression/sample_8.raw");
        const SAMPLE_16_DATA: &[u8] = include_bytes!("../tests/compression/sample_16.raw");

        let instrument = ensure_parse(instrument_file, COMPRESSED_INST_DATA);
        let mut samples = instrument.samples.into_iter();
        assert_eq!(samples.next().unwrap().data.unwrap(), SAMPLE_8_DATA.iter().map(|x| (*x as i8).normalize()).collect::<Vec<_>>());
        assert_eq!(samples.next().unwrap().data.unwrap(), SAMPLE_16_DATA.chunks_exact(2).map(|chunk| i16::from_le_bytes([chunk[0], chunk[1]]).normalize()).collect::<Vec<_>>());

        // todo: checking if the samples are in fact compressed wouldn't hurt :)
    }

    #[test]
    fn song_message(){
        const MODULE_DATA: &[u8] = include_bytes!("../tests/song_message.it");
        const MODULE_SONG_MESSAGE: &str = "lorem ipsum";

        let module = ensure_parse(module_file, MODULE_DATA);
        assert_eq!(module.message, MODULE_SONG_MESSAGE.to_string());
    }
}