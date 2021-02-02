use anyhow::{Context, Result};
use hound::{SampleFormat, WavSpec, WavWriter};
use ittech::hl::{Command, Note, Order, IT, Sample};
use ittech::parser;
use std::{env, fs};

const USAGE: &str = "usage: cargo run --example render -- <itmodule> <outputwav>";

fn main() -> Result<()> {
    let inpname = env::args().nth(1).context(USAGE)?;
    let outname = env::args().nth(2).context(USAGE)?;

    let data = fs::read(&inpname)
        .with_context(|| format!("failed to read file {}", &inpname))?;
    // leaking because we need `data` to be able to leave main on error,
    //   this is just an example, don't do this in real programs
    let it = parser::it(Box::leak(data.into_boxed_slice()))
        .context("failed to parse data")?;

    let buffer = render(it).context("failed to render file")?;

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
static EMPTY: Vec<Vec<Command>> = Vec::new();

const A4: u8 = 57;
const C5: u8 = 60;

fn note_freq(note: u8) -> f32 {
    let exp = ((note as f32) - (A4 as f32)) / 12.0;
    440.0f32 * 2.0f32.powf(exp)
}

fn resample(sample: &Sample, note: u8) -> impl Iterator<Item=f32> + '_ {
    let ratio = note_freq(note) / note_freq(C5);
    let incr = ratio;
    let table = sample.data.as_ref().unwrap();
    let mut offset = 0.0f32;

    std::iter::from_fn(move || {
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
}

fn render(it: IT) -> Result<Vec<f32>> {
    let active_channels = it.patterns
        .iter()
        .flat_map(|pat| {
            pat.rows.iter()
                .flat_map(|row| row.iter())
                .map(|cmd| cmd.channel as usize)
        })
        .max()
        .context("file is silent (no active channels)")? + 1;
    let amp = 1.0 / (active_channels as f32);

    let total_rows = it.orders
        .iter()
        .map(|ord| match *ord {
            Order::Index(idx) => it.patterns[idx as usize].rows.len(),
            _ => 0,
        })
        .sum::<usize>();

    let samples_per_row = SR * 60 / 4 / (it.tempo as usize);

    let mut buffer = vec![0.0f32; total_rows * samples_per_row];

    it.orders
        .iter()
        .flat_map(|ord| match *ord {
            Order::Index(idx) => it.patterns[idx as usize].rows.iter(),
            _ => EMPTY.iter(),
        })
        .zip((0..).step_by(samples_per_row))
        .for_each(|(row, offset)| {
            let buffer = &mut buffer[offset..][..samples_per_row];
            for Command { note, instrument, .. } in row {
                if let (Some(Note::Tone(note)), Some(instrument)) = (note, instrument) {
                    if let Some(instrument) = &it.instruments.get((*instrument as usize).wrapping_sub(1)) {
                        if let Some((_, sample)) = instrument.keyboard.iter().find(|(n, _)| n == note) {
                            if let Some(sample) = &it.samples.get((*sample as usize).wrapping_sub(1)) {
                                buffer.iter_mut()
                                    .zip(resample(&sample, *note))
                                    .for_each(|(o, i)| *o = i * amp);
                            }
                        }
                    }
                }
            }
        });

    Ok(buffer)
}
