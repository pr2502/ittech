use std::fmt::{self, Display};


/// Impulse Tracker file types
#[derive(Clone, Copy, Debug)]
pub enum FileType {
    /// Module file
    ///
    /// Contains all information to play a track instruments, samples, patterns, etc.
    Module,

    /// Instrument file
    ///
    /// Contains an instrument description and its samples.
    Instrument,

    /// Sample file
    ///
    /// Contains one audio sample and its looping and frequency parameters.
    Sample,
}

/// Error returned by [`scan`](crate::parser::scan::scan) function if no magic number is recognized
#[derive(Clone, Copy, Debug)]
pub struct ScanError(());


/// Guess what Impulse Tracker file type is in the input buffer
///
/// Checks magic numbers at the beginning of the buffer in order to estimate what file type the
/// buffer contains. This method only does a trivial analysis and doesn't guarantee that a file is
/// actually the type it reports, however it is guaranteed that a different file type than was
/// reported canot be parsed.
///
/// In other words this function can return a false positive but never a false negative.
pub fn scan(input: &[u8]) -> Result<FileType, ScanError> {
    match input {
        [b'I', b'M', b'P', b'M', ..] => Ok(FileType::Module),
        [b'I', b'M', b'P', b'I', ..] => Ok(FileType::Instrument),
        [b'I', b'M', b'P', b'S', ..] => Ok(FileType::Sample),
        _ => Err(ScanError(())),
    }
}


impl Display for ScanError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("no Impulse Tracker magic number was recognized")
    }
}

impl std::error::Error for ScanError {}
