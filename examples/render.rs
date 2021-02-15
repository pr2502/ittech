#![feature(or_patterns)]

use anyhow::{Context, Result};
use ittech::error::{convert_error, VerboseError};
use ittech::parser;
use ittech::{ActiveChannels, Command, Module, Note, NoteCmd, Sample};
use nom::Err;
use std::{env, fs, iter};

const USAGE: &str = "usage: cargo run --example render -- <itmodule> <outputwav>";

fn main() -> Result<()> {
    let inpname = env::args().nth(1).context(USAGE)?;
    let outname = env::args().nth(2).context(USAGE)?;

    let data = fs::read(&inpname)
        .with_context(|| format!("failed to read file {}", &inpname))?;
    let module = match parser::module::<VerboseError<_>>(&data) {
        Ok(module) => module,
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            eprintln!("parser failed\n\n{}", convert_error(&data, e));
            return Ok(());
        }
        _ => unreachable!(),
    };

    let buffer = render(module).context("failed to render file")?;

    let buffer = buffer.into_iter()
        .map(|f| (f * (i16::MAX as f32)).round() as i16)
        .collect();

    let fmt = wav::Format::new(
        wav::Channels::Mono,
        SR as u32,
        wav::BitDepth::B16,
    );

    let mut outfile = fs::File::create(&outname)
        .with_context(|| format!("failed to write file {}", &outname))?;

    wav::write(fmt, wav::PCMData::B16(buffer), &mut outfile)
        .with_context(|| format!("failed to write file {}", &outname))?;

    Ok(())
}

const SR: usize = 44_100;

fn resample(sample: &Sample, note: Note) -> Box<dyn Iterator<Item=f32> + '_> {
    let note_ratio = note.freq() / Note::C_5.freq();
    let sr_ratio = (SR as f32) / (sample.samplerate_c5 as f32);
    let incr = note_ratio / sr_ratio;
    let table = sample.data.as_ref().unwrap();
    let mut offset = 0.0f32;

    Box::new(
        iter::from_fn(move || {
            if offset.trunc() as usize + 1 >= table.len() {
                if sample.loop_.is_some() {
                    offset -= table.len() as f32;
                } else {
                    return None
                }
            }
            let idx = offset.trunc() as usize;
            let frac = offset.fract();
            let lerp = table[idx] * (1.0 - frac) + table[idx+1] * frac;
            offset += incr;
            Some(lerp)
        })
        .fuse()
    )
}

fn render(module: Module) -> Result<Vec<f32>> {
    let active_channels = module.active_channels();
    let amp = 1.0 / (active_channels.count() as f32);

    let total_rows = module.ordered_patterns()
        .map(|pat| pat.rows.len())
        .sum::<usize>();

    let samples_per_row = SR * 60 / 4 / (module.tempo.as_u8() as usize);

    let mut buffer = vec![0.0f32; total_rows * samples_per_row];
    const NONE: Option<Box<dyn Iterator<Item=f32>>> = None;
    let mut generators = [NONE; ActiveChannels::all().count()];

    module.ordered_patterns()
        .flat_map(|pat| pat.rows.iter())
        .zip((0..).step_by(samples_per_row))
        .for_each(|(row, offset)| {
            let buffer = &mut buffer[offset..][..samples_per_row];
            for (chan, Command { note, instrument, .. }) in row.iter() {
                match (*note, instrument) {
                    (Some(NoteCmd::Play(note)), Some(instrument)) => {
                        let instrument = &module[instrument];
                        if let Some(sample) = instrument.sample_map[note] {
                            let sample = &module[sample];
                            generators[chan.as_usize()] = Some(Box::new(resample(sample, note.into())));
                        }
                    }
                    (Some(NoteCmd::Cut | NoteCmd::Fade), _) => {
                        generators[chan.as_usize()] = None;
                    }
                    _ => {}
                }
            }
            for channel in active_channels.iter() {
                if let Some(generator) = &mut generators[channel.as_usize()] {
                    buffer.iter_mut()
                        .zip(generator)
                        .for_each(|(o, i)| *o += i * amp);
                }
            }
        });

    Ok(buffer)
}
