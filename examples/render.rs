#![feature(or_patterns)]

use anyhow::{Context, Result};
use hound::{SampleFormat, WavSpec, WavWriter};
use ittech::parser;
use ittech::{Channel, Command, Module, Note, NoteCmd, Sample};
use std::{env, fs, iter};

const USAGE: &str = "usage: cargo run --example render -- <itmodule> <outputwav>";

fn main() -> Result<()> {
    let inpname = env::args().nth(1).context(USAGE)?;
    let outname = env::args().nth(2).context(USAGE)?;

    let data = fs::read(&inpname)
        .with_context(|| format!("failed to read file {}", &inpname))?;
    // leaking because we need `data` to be able to leave main on error,
    //   this is just an example, don't do this in real programs
    let module = parser::module(Box::leak(data.into_boxed_slice()))
        .context("failed to parse data")?;

    let buffer = render(module).context("failed to render file")?;

    let spec = WavSpec {
        channels: 1,
        sample_rate: SR as u32,
        bits_per_sample: 32,
        sample_format: SampleFormat::Float,
    };
    let mut outfile = WavWriter::create(&outname, spec)
        .with_context(|| format!("failed to write file {}", &outname))?;

    buffer.iter()
        .try_for_each(|&sample| outfile.write_sample(sample))
        .context("failed writing sample")?;

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
                if sample.do_loop {
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
    let mut channels = iter::from_fn(|| Some(None))
        .take(Channel::MAX as usize + 1)
        .collect::<Vec<_>>();

    module.ordered_patterns()
        .flat_map(|pat| pat.rows.iter())
        .zip((0..).step_by(samples_per_row))
        .for_each(|(row, offset)| {
            let buffer = &mut buffer[offset..][..samples_per_row];
            for Command { channel, note, instrument, .. } in row {
                match (*note, instrument) {
                    (Some(NoteCmd::Play(note)), Some(instrument)) => {
                        let instrument = &module[instrument];
                        if let Some(sample) = instrument.sample_map[note] {
                            let sample = &module[sample];
                            channels[channel.as_usize()] = Some(Box::new(resample(sample, note.into())));
                        }
                    }
                    (Some(NoteCmd::Cut | NoteCmd::Fade), _) => {
                        channels[channel.as_usize()] = None;
                    }
                    _ => {}
                }
            }
            for channel in active_channels.iter() {
                if let Some(generator) = &mut channels[channel.as_usize()] {
                    buffer.iter_mut()
                        .zip(generator)
                        .for_each(|(o, i)| *o += i * amp);
                }
            }
        });

    Ok(buffer)
}
