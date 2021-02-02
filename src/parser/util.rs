use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::error::ParseError;
use nom::multi::count;
use nom::{IResult, Parser};
use std::ops::RangeBounds;

/// Consumes N bytes and returns the result as an array.
pub fn byte_array<const N: usize>(input: &[u8]) -> IResult<&[u8], [u8; N]> {
    let (rest, slice) = take(N)(input)?;
    let mut array = [0u8; N];
    array.copy_from_slice(slice);
    Ok((rest, array))
}

/// Runs the embedded parser N times and returns the result as an array.
pub fn array<I, O, E, F, const N: usize>(
    f: F,
) -> impl FnOnce(I) -> IResult<I, [O; N], E>
where
    I: Clone + PartialEq,
    O: Copy + Default,
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    move |input: I| {
        let (rest, vec): (_, Vec<_>) = count(f, N)(input)?;
        let mut arr = [O::default(); N];
        arr.copy_from_slice(&vec);
        Ok((rest, arr))
    }
}

/// Runs the embedded parser and verifies the result lies withing the given range.
pub fn ranged<I, O, E, F, R>(
    f: F,
    range: R,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone,
    O: PartialOrd<O>,
    E: ParseError<I>,
    F: Parser<I, O, E>,
    R: RangeBounds<O>,
{
    verify(f, move |val| range.contains(val))
}

pub fn offset_list<'a, O, E, F>(
    mut f: F,
    offset_list: Vec<u32>,
) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], Vec<O>, E>
where
    E: ParseError<&'a [u8]>,
    F: Parser<&'a [u8], O, E>,
{
    move |input: &'a [u8]| {
        let mut output_list = Vec::with_capacity(offset_list.len());
        for &offset in &offset_list {
            let offset = offset as usize;
            if offset == 0 {
                todo!()
            }
            if offset >= input.len() {
                return Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Eof)));
            }
            let (_, i) = f.parse(&input[offset..])?;
            output_list.push(i);
        }
        Ok((input, output_list))
    }
}
