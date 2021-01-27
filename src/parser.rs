use crate::hl::{Instrument, Order, Pattern, Sample, IT};
use crate::ll::{DOSFilename, Envelope, ITFileHeader, InstrumentHeader, Name, Node, SampleHeader};
use nom::bytes::complete::{tag, take};
use nom::error::Error;
use nom::multi::count;
use nom::number::complete::{le_i8, le_u16, le_u32, le_u8};
use nom::IResult;
use util::{array, byte_array, offset_list, ranged};

mod util;


pub fn it(whole_input: &[u8]) -> Result<IT, nom::Err<Error<&[u8]>>> {
    let (input, header) = it_file_header(whole_input)?;
    let (input, orders) = count(order, header.ordnum as usize)(input)?;
    let (input, ins_offsets) = count(le_u32, header.insnum as usize)(input)?;
    let (input, sam_offsets) = count(le_u32, header.smpnum as usize)(input)?;
    let (_rest, pat_offsets) = count(le_u32, header.patnum as usize)(input)?;

    // Offsets are relative to the start of the file, use the whole input each time.
    let (_, instruments) = offset_list(instrument, ins_offsets)(whole_input)?;
    let (_, samples) = offset_list(sample, sam_offsets)(whole_input)?;
    let (_, patterns) = offset_list(pattern, pat_offsets)(whole_input)?;

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
    Ok((
        input,
        match byte {
            0..=199 => Order::Index(byte),
            // ITTECH.TXT says only 0..=199 are allowed but these are not used for anything else so
            // we're going to parse them too.
            200..=253 => Order::Index(byte),
            254 => Order::Separator,
            255 => Order::EndOfSong,
        },
    ))
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
            reserved1: header.reserved1,
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

fn sample(input: &[u8]) -> IResult<&[u8], Sample> {
    let (input, header) = sample_header(input)?;
    Ok((
        input,
        Sample {
            name: header.name,
            filename: header.filename,
        },
    ))
}

fn pattern(input: &[u8]) -> IResult<&[u8], Pattern> {
    Ok((input, Pattern {}))
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
        keyboard  = byte_array,
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
