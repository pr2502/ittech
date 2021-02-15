use crate::error::ContextError;
use nom::bytes::complete::take;
use nom::error::{make_error, ErrorKind, ParseError};
use nom::Err::Error;
use nom::{IResult, Parser};
use std::convert::{TryFrom, TryInto};
use std::fmt::Debug;
use std::ops::RangeBounds;


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


///////////////////////////////////////////////////////////////////////////////////////////////////
// XXX TODO FIXME NOTE
//
// Polyfills for `nom`s `alloc` feature
//
// We wan't the `alloc` feature but we don't want the `bitvec` feature which at this time causes
// the build to fail. We can remove these once `bitvec` gets fixed and/or updated in `nom` or once
// `nom`s feature definitions get fixed to the point that `alloc` doesn't pull in `bitvec` with it.
//
// These functions are copied almost verbatim from `nom` v6.1.0
//

use nom::Err;

pub fn count<I, O, E, F>(mut f: F, count: usize) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
  I: Clone + PartialEq,
  F: Parser<I, O, E>,
  E: ParseError<I>,
{
  move |i: I| {
    let mut input = i.clone();
    let mut res = Vec::with_capacity(count);

    for _ in 0..count {
      let input_ = input.clone();
      match f.parse(input_) {
        Ok((i, o)) => {
          res.push(o);
          input = i;
        }
        Err(Err::Error(e)) => {
          return Err(Err::Error(E::append(i, ErrorKind::Count, e)));
        }
        Err(e) => {
          return Err(e);
        }
      }
    }

    Ok((input, res))
  }
}

pub fn many_till<I, O, P, E, F, G>(
  mut f: F,
  mut g: G,
) -> impl FnMut(I) -> IResult<I, (Vec<O>, P), E>
where
  I: Clone + PartialEq,
  F: Parser<I, O, E>,
  G: Parser<I, P, E>,
  E: ParseError<I>,
{
  move |mut i: I| {
    let mut res = Vec::new();
    loop {
      match g.parse(i.clone()) {
        Ok((i1, o)) => return Ok((i1, (res, o))),
        Err(Err::Error(_)) => {
          match f.parse(i.clone()) {
            Err(Err::Error(err)) => return Err(Err::Error(E::append(i, ErrorKind::ManyTill, err))),
            Err(e) => return Err(e),
            Ok((i1, o)) => {
              // loop trip must always consume (otherwise infinite loops)
              if i1 == i {
                return Err(Err::Error(E::from_error_kind(i1, ErrorKind::ManyTill)));
              }

              res.push(o);
              i = i1;
            }
          }
        }
        Err(e) => return Err(e),
      }
    }
  }
}
