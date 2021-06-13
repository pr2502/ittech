use anyhow::{Context, Result};
use ittech::error::{convert_error, VerboseError};
use ittech::parser;
use nom::Err;
use std::{env, fs};

fn main() -> Result<()> {
    let fname = env::args().nth(1)
        .context("usage: cargo run --example dump -- <itmodule>")?;
    let data = fs::read(&fname)
        .with_context(|| format!("failed to read file {}", &fname))?;
    match parser::module_file::<VerboseError<&[u8]>>(&data) {
        Ok(it) => println!("{:#X?}", it),
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            eprintln!("parser failed\n\n{}", convert_error(&data, e));
        }
        _ => unreachable!(),
    }
    Ok(())
}
