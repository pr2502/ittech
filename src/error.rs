//! Error management
//!
//! This module reimplements/modifies a lot of the default `nom` error behaviour to make it
//! actually suitable for debugging a binary parser such as this. It may be slower than the nom
//! version but what use is a parser that's fast but gives useless output..

use nom::error::{ErrorKind, ParseError};
use nom::{Err, IResult};
use nom::{Offset, Parser};
use std::borrow::Cow;
use std::fmt::{self, Debug, Display, Write};
use std::iter;

pub use crate::parser::scan::ScanError;


#[derive(Debug)]
pub struct OutOfRangeError<const LOW: u8, const HIGH: u8>(pub(crate) u8);

impl<const LOW: u8, const HIGH: u8> Display for OutOfRangeError<LOW, HIGH> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "conversion failed, value {} is out of range {}..={}", self.0, LOW, HIGH)
    }
}

impl<const LOW: u8, const HIGH: u8> std::error::Error for OutOfRangeError<LOW, HIGH> {}


/// This error type accumulates errors and their position when backtracking
/// through a parse tree. With some post processing (cf `examples/json.rs`),
/// it can be used to display user friendly error messages
#[derive(Clone, Debug, PartialEq)]
pub struct VerboseError<I> {
    /// List of errors accumulated by `VerboseError`, containing the affected
    /// part of input data, and some context
    pub errors: Vec<(I, VerboseErrorKind)>,
}

/// Error context for `VerboseError`
#[derive(Clone, Debug, PartialEq)]
pub enum VerboseErrorKind {
    /// String added by the `context` function
    Context(Cow<'static, str>),

    /// Error kind given by various nom parsers
    Nom(ErrorKind),
}

impl<I> ParseError<I> for VerboseError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        VerboseError {
            errors: vec![(input, VerboseErrorKind::Nom(kind))],
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.errors.push((input, VerboseErrorKind::Nom(kind)));
        other
    }

    fn from_char(_input: I, _c: char) -> Self {
        unimplemented!("chars don't really make sense for a binary parser")
    }
}

pub trait ContextError<I>: Sized {
    fn add_context(_input: I, _ctx: Cow<'static, str>, other: Self) -> Self;
    fn new(_input: I, _ctx: Cow<'static, str>) -> Self;
}

impl<I> ContextError<I> for VerboseError<I> {
    fn add_context(input: I, ctx: Cow<'static, str>, mut other: Self) -> Self {
        other.errors.push((input, VerboseErrorKind::Context(ctx)));
        other
    }

    fn new(input: I, ctx: Cow<'static, str>) -> Self {
        VerboseError {
            errors: vec![(input, VerboseErrorKind::Context(ctx))],
        }
    }
}

/// Create a new error from an input position, a static string and an existing error.
/// This is used mainly in the [context!] combinator, to add user friendly information
/// to errors when backtracking through a parse tree
pub(crate) fn context<'i, I, E, F, O>(
    context: impl Fn() -> Cow<'static, str>,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: Clone,
    E: ContextError<I> + 'i,
{
    move |i: I| match f.parse(i.clone()) {
        Ok(o) => Ok(o),
        Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
        Err(Err::Error(e)) => Err(Err::Error(E::add_context(i, context(), e))),
        Err(Err::Failure(e)) => Err(Err::Failure(E::add_context(i, context(), e))),
    }
}

macro_rules! context {
    ( $parser: expr, $msg: literal $(,)? ) => {
        $crate::error::context(move || ::std::borrow::Cow::Borrowed($msg), $parser)
    };
    ( $parser: expr, $fmt: literal $(, $args: expr )+ $(,)? ) => {
        $crate::error::context(move || ::std::borrow::Cow::Owned(::std::format!($fmt, $($args),+)), $parser)
    };
    ( $parser: expr, $payload: expr $(,)? ) => {
        $crate::error::context(move || ::std::borrow::Cow::Owned($payload.to_string()), $parser)
    };
}

macro_rules! error {
    ( $input: expr, $msg: literal $(,)? ) => {
        E::new($input, ::std::borrow::Cow::Borrowed($msg))
    };
    ( $input: expr, $fmt: literal $(, $args: expr )+ $(,)? ) => {
        E::new($input, ::std::borrow::Cow::Owned(::std::format!($fmt, $($args),+)))
    };
    ( $input: expr, $payload: expr $(,)? ) => {
        E::new($input, ::std::borrow::Cow::Owned($payload.to_string()))
    };
}

macro_rules! bail {
    ($($tt:tt)*) => {
        return ::std::result::Result::Err(::nom::Err::Error(error!($($tt)*)))
    };
}

/// Transforms a `VerboseError` into a trace with input position information.
///
/// This function is modified from the original [`nom::error::convert_error`] to be used with
/// binary input to a context. The trace is instead of lines shown on an `xxd`-style hexdump.
pub fn convert_error(
    input: &[u8],
    err: VerboseError<&[u8]>,
) -> String {
    // We're using `write!` on a `String` buffer, which is infallible so `unwrap`s here are fine.
    let mut result = String::new();

    for (i, (substring, kind)) in err.errors.iter().enumerate() {
        let offset = input.offset(substring);

        if input.is_empty() {
            use VerboseErrorKind::*;
            match kind {
                Context(s) => write!(&mut result, "{}: in {}, got empty input\n\n", i, s).unwrap(),
                Nom(e) => write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e).unwrap(),
            }
        } else {
            // Find the line that includes the subslice.
            //
            // Our "line" is a 16-byte string, therefore the beginning of our line is just the
            // offset rounded down to the nearest multiple of 16.
            let line_begin = offset - (offset % 16);

            // Format the line into a hexdump, if there are not 16 bytes left till the end of the
            // input fill the rest with whitespace to match the alignment.
            //
            // A line should look like this:
            // 00000000: 0000 0000 0000 0000 0000 0000 0000 0000  ................
            // ^offset   ^16 bytes in {:02x} grouped by 2         ^ char if ascii printable or ' ',
            //  {:08x}                                              otherwise a '.'
            let line = {
                let mut buf = String::new();

                // offset
                write!(&mut buf, "{:08x}:", line_begin).unwrap();

                // hexdump
                let line = input[line_begin..]
                    .iter()
                    .map(|&byte| Some(byte))
                    .chain(iter::repeat(None))
                    .take(16)
                    .enumerate();

                for (i, byte) in line.into_iter() {
                    if i % 2 == 0 {
                        buf.push(' ');
                    }
                    if let Some(byte) = byte {
                        write!(&mut buf, "{:02x}", byte).unwrap();
                    } else {
                        buf.push_str("  ");
                    }
                }

                buf.push_str("  ");

                // ascii representation
                for &byte in input[line_begin..].iter().take(16) {
                    if byte.is_ascii_graphic() || byte == b' ' {
                        buf.push(byte as char);
                    } else {
                        buf.push('.');
                    }
                }

                buf
            };

            const CARET: &str = "^---";

            // The caret is positioned beneath the hex representation of the byte.
            let caret_position = {
                let line_offset = offset % 16;
                10 + CARET.len() + (line_offset / 2) * 5 + (line_offset % 2) * 2
            };

            match kind {
                VerboseErrorKind::Context(s) => write!(
                    &mut result,
                    "{i}: at offset {offset:#x}, {context}:\n\
                    {line}\n\
                    {caret:>column$}\n\n",
                    i = i,
                    offset = offset,
                    context = s,
                    line = line,
                    caret = CARET,
                    column = caret_position,
                ).unwrap(),
                VerboseErrorKind::Nom(_) => {},
                // write!(
                //     &mut result,
                //     "{i}: at offset {offset:#x}, in {nom_err:?}:\n\
                //     {line}\n\
                //     {caret:>column$}\n\n",
                //     i = i,
                //     offset = offset,
                //     nom_err = e,
                //     line = line,
                //     caret = CARET,
                //     column = caret_position,
                // ).unwrap(),
            }
        }
    }

    result
}
