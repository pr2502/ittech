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
//! > Impulse Tracker is a multi-track digital sound tracker (music sequencer). Originally released
//! > in 1995 by Jeffrey Lim as freeware with commercial extensions, it was one of the last tracker
//! > programs for the DOS platform.
//! >
//! > -- Wikipedia ([article link](https://en.wikipedia.org/wiki/Impulse_Tracker))
//!
//! This crate is a parser (and in the near future also hopefuly a writer) for the Impulse Tracker
//! native module file format. The Rust representation attempts to both express the module file in
//! a manner that's lossless for valid files and that forbids creating invalid files.
//!
//!
//! # Lossy parsing and canonicalization
//!
//! For files outside of the specification there are multiple ways to handle them, the parser can
//! either abort or can canonicalize the value to some "most sane" value, or can try to skip the
//! invalid part.
//!
//! Currently the parser will panic in cases that are not yet supported (implemented), but this
//! should not be the case once enough of the format is implemented.
//!
//! If the feature `log` is enabled, the crate with log an info message whenever some data from the
//! input is lost and should explain what value was found, what is wrong with it and how it has
//! been fixed. The canonicalization logic is documented under the "Canonicalization" section on
//! each specific value type. Please report issues with any inconsistencies between the parsed
//! results of and the documentation.
//!
//!
//! ## Structure and modfile representation
//!
//! The general structure of a complete modfile (.it) can be simplified to this self-referencing tree.
//! Complete modfiles are parsed using the [`parser::module_file`] function.
//!
//! ```txt
//! Module
//!   ├ Order ┄┄┄┐
//!   │          ┊
//!   ├ Pattern <┘
//!   │ │
//!   │ └ Row
//!   │   │
//!   │   └ Command ┐
//!   │             ┊
//!   ├ Instrument <┘
//!   │         ┊
//!   └ Sample <┘
//! ```
//!
//! [`Module`] contains [`Order`]s, [`Pattern`]s, [`Instrument`]s and [`Sample`]s. Orders reference
//! Patterns through [`PatternId`]s, Commands reference Instruments through [`InstrumentId`]s and
//! Instruments reference Samples through [`SampleId`]s.
//!
//! Patterns are a 2D sparse matrices of [`Command`]s stored row-wise in [`Row`]s.
//!
//!
//! Instruments can also exist on their own in instrument files (.iti). [`InstrumentFile`] only
//! contains a single Instrument and its referenced samples. These files are parsed using the
//! [`parser::instrument_file`] function.
//!
//! ```txt
//! InstrumentFile
//!   ├ Instrument
//!   │         ┊
//!   └ Sample <┘
//! ```
//!
//! Samples can also exist in their own files (.its) as just a lone [`Sample`]. These files are
//! parsed using the [`parser::sample_file`] function.
//!
//!
//! ## Additional resources
//!
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
