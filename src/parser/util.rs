use crate::error::ContextError;
use nom::bytes::complete::take;
use nom::error::{make_error, ErrorKind, ParseError};
use nom::multi::count;
use nom::Err::Error;
use nom::{IResult, Parser};
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::ops::RangeBounds;


/// Helper trait for `.try_into().unwrap()` for cases where a panic is meant to be a bug.
pub(crate) trait Cast<T> {
    fn cast(self) -> T;
}

impl<F, T> Cast<T> for F
where
    T: TryFrom<F>,
    <T as TryFrom<F>>::Error: std::fmt::Debug,
{
    #[track_caller]
    fn cast(self) -> T {
        self.try_into().unwrap()
    }
}


/// Consumes N bytes and returns the result as an array.
pub fn byte_array<'i, E: ParseError<&'i [u8]>, const N: usize>(input: &'i [u8]) -> IResult<&'i [u8], [u8; N], E> {
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
    E: ParseError<I>,
    F: Parser<I, O, E>,
{
    move |input: I| {
        let (rest, vec): (_, Vec<_>) = count(f, N)(input)?;
        let arr = vec.try_into().unwrap_or_else(|_| unreachable!());
        Ok((rest, arr))
    }
}

/// Runs the embedded parser and verifies the result lies withing the given range.
pub fn ranged<I, O, E, F, R>(
    mut f: F,
    range: R,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone,
    O: PartialOrd<O> + Debug,
    E: ParseError<I> + ContextError<I>,
    F: Parser<I, O, E>,
    R: RangeBounds<O> + Clone,
{
    move |input: I| {
        use std::ops::Bound;

        let (rest, val) = f.parse(input.clone())?;
        if !range.contains(&val) {
            bail!(
                input,
                "value must be in range {}..{}",
                match range.start_bound() {
                    Bound::Unbounded => String::new(),
                    Bound::Included(b) => format!("{:#x?}", b),
                    Bound::Excluded(_) => unreachable!(), // doesn't make sense for starting bound
                },
                match range.end_bound() {
                    Bound::Unbounded => String::new(),
                    Bound::Included(b) => format!("={:#x?}", b),
                    Bound::Excluded(b) => format!("{:#x?}", b),
                },
            )
        }
        Ok((rest, val))
    }
}

pub fn offset_list<'i, O, E, F>(
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
            let offset = offset as usize;
            if offset == 0 {
                todo!()
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

pub fn scan_count<I, O, E, F, G, R>(
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
