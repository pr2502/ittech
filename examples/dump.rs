use anyhow::{Context, Result};
use ittech::parser;
use std::{env, fs};

fn main() -> Result<()> {
    let fname = env::args().nth(1)
        .context("usage: cargo run --example dump -- <itmodule>")?;
    let data = fs::read(&fname)
        .with_context(|| format!("failed to read file {}", &fname))?;
    // leaking because we need `data` to be able to leave main on error,
    //   this is just an example, don't do this in real programs
    let it = parser::it(Box::leak(data.into_boxed_slice()))
        .context("failed to parse data")?;
    println!("{:#X?}", &it);
    Ok(())
}
