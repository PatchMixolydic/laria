use std::str::Utf8Error;
use thiserror::Error;

#[derive(Clone, Copy, Debug, Error)]
pub enum FromBytesError {
    #[error("Byte slice not big enough for a subroutine (expected {expected}, got {actual})")]
    NotEnoughBytes { expected: usize, actual: usize },
    #[error("Error while parsing a string: {0}")]
    Utf8Error(#[source] Utf8Error)
}

pub struct Subroutine {
    name: String,
    num_arguments: u8,
    start_address: usize
}

impl<'name> Subroutine {
    pub const fn new(name: String, num_arguments: u8, start_address: usize) -> Self {
        Self {
            name,
            num_arguments,
            start_address
        }
    }

    pub fn from_bytes(bytes: &'name [u8]) -> Result<Self, FromBytesError> {
        if bytes.is_empty() {
            return Err(FromBytesError::NotEnoughBytes {
                expected: 1,
                actual: bytes.len()
            });
        }

        let name_length = bytes[0] as usize;
        let expected_bytes_len = 1 + name_length + 1 + 4;

        if bytes.len() < expected_bytes_len {
            return Err(FromBytesError::NotEnoughBytes {
                expected: expected_bytes_len,
                actual: bytes.len()
            });
        }

        let name = match std::str::from_utf8(&bytes[1..=name_length]) {
            Ok(res) => res.to_string(),
            Err(err) => return Err(FromBytesError::Utf8Error(err))
        };

        let num_arguments = bytes[name_length + 1];

        let start_address = u32::from_le_bytes([
            bytes[name_length + 2],
            bytes[name_length + 3],
            bytes[name_length + 4],
            bytes[name_length + 5]
        ]);

        Ok(Self::new(name, num_arguments, start_address as usize))
    }
}
