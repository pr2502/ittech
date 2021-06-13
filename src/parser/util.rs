use nom::bytes::complete::take;
use nom::error::{make_error, ErrorKind, ParseError};
use nom::multi::count;
use nom::Err::Error;
use nom::{IResult, Parser};
use std::convert::{TryFrom, TryInto};


/// Helper trait for `.try_into().unwrap()` for cases where a panic is meant to be a bug.
pub(crate) trait Cast {
    #[track_caller]
    fn cast<T>(self) -> T
    where
        Self: Sized,
        T: TryFrom<Self>,
        <T as TryFrom<Self>>::Error: std::fmt::Debug,
    {
        T::try_from(self).unwrap()
    }
}

impl<T> Cast for T {}

/// Consumes N bytes and returns the result as an array.
pub(crate) fn byte_array<'i, E: ParseError<&'i [u8]>, const N: usize>(input: &'i [u8]) -> IResult<&'i [u8], [u8; N], E> {
    let (rest, slice) = take(N)(input)?;
    let mut array = [0u8; N];
    array.copy_from_slice(slice);
    Ok((rest, array))
}

/// Runs the embedded parser N times and returns the result as an array.
pub(crate) fn array<I, O, E, F, const N: usize>(
    f: F,
) -> impl FnOnce(I) -> IResult<I, [O; N], E>
where
    I: Clone + PartialEq,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    move |input: I| {
        let (rest, vec): (_, Vec<_>) = count(f, N)(input)?;
        let arr = vec.try_into().unwrap_or_else(|_| unreachable!());
        Ok((rest, arr))
    }
}

pub(crate) fn offset_list<'i, O, E, F>(
    mut f: F,
    offset_list: Vec<u32>,
) -> impl FnMut(&'i [u8]) -> IResult<&'i [u8], Vec<O>, E>
where
    E: ParseError<&'i [u8]>,
    F: Parser<&'i [u8], O, E>,
{
    move |input: &'i [u8]| {
        let mut output_list = Vec::with_capacity(offset_list.len());
        for &offset in &offset_list {
            let offset = offset.try_into().unwrap();
            if offset == 0 {
                todo!("not yet handled")
            }
            if offset >= input.len() {
                return Err(Error(make_error(input, ErrorKind::Eof)));
            }
            let (_, i) = f.parse(&input[offset..])?;
            output_list.push(i);
        }
        Ok((input, output_list))
    }
}

pub(crate) fn scan_count<I, O, E, F, G, R>(
    count: usize,
    mut parse: F,
    init: R,
    fold: G,
) -> impl FnMut(I) -> IResult<I, R, E>
where
    I: Clone + PartialEq,
    F: Parser<I, O, E>,
    G: Fn(&mut R, O),
    E: ParseError<I>,
    R: Clone,
{
    move |mut input: I| {
        let mut acc = init.clone();
        for _ in 0..count {
            let (tail, value) = parse.parse(input.clone())?;
            fold(&mut acc, value);
            input = tail;
        }
        Ok((input, acc))
    }
}
