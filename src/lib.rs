#![feature(external_doc)]

//! # Impulse Tracker module file parser and writer
//!
//! ## Additional resources
//! - <https://github.com/OpenMPT/openmpt/blob/master/soundlib/ITTools.h>
//! - <https://github.com/OpenMPT/openmpt/blob/master/soundlib/ITTools.cpp>
//! - <https://github.com/OpenMPT/openmpt/blob/master/soundlib/Load_it.cpp>


/// ITTECH.TXT (documentation only)
///
/// This is a file documenting Impulse Tracker taken from
/// <https://github.com/schismtracker/schismtracker/wiki/ITTECH.TXT>
///
#[doc(include = "../ITTECH.txt")]
pub mod ittech_txt {}

pub mod error;

mod data;
pub use data::*;

pub mod parser;
pub mod writer;
