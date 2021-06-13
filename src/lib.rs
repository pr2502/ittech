#![feature(const_mut_refs)]
#![feature(const_panic)]
#![feature(const_str_from_utf8_unchecked)]
#![feature(stmt_expr_attributes)]

// We use all these clippy lints to help avoid silent data loss in the parser/serializer,
// none of these warnings should make it into a release.
#![warn(clippy::as_conversions)]
#![warn(clippy::cast_lossless)]
#![warn(clippy::cast_possible_truncation)]
#![warn(clippy::cast_possible_wrap)]
#![warn(clippy::cast_precision_loss)]
#![warn(clippy::cast_sign_loss)]
#![warn(clippy::unnecessary_cast)]

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
#[doc = include_str!("../ITTECH.txt")]
pub mod ittech_txt {}

#[macro_use]
// Macro exporting is still weird. We want the macros to be `pub(crate)`, the combination of
// `#[macro_use]`, the module containing them being lexically first, never importing the macros
// with `use` and using it as `macro!()` not `$crate::macro!()` seems to work out to that.
pub mod error;

mod data;
pub use data::*;

pub mod parser;
pub mod writer;

pub use parser::scan::FileType;
