use ittech::parser;
use std::{env, fs};

fn main() {
    let fname = env::args().nth(1).expect("usage: cargo run -- <itmodule>");
    let data = fs::read(fname).expect("failed to read file");
    let it = parser::it(&data).unwrap();
    println!("{:#X?}", it);
}
