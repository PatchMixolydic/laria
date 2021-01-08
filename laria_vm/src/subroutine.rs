use crate::value::FromBytesError;

#[derive(Clone, Debug)]
pub struct Subroutine {
    name: String,
    num_arguments: u8,
    start_address: usize,
}

impl Subroutine {
    pub const fn new(name: String, num_arguments: u8, start_address: usize) -> Self {
        Self {
            name,
            num_arguments,
            start_address,
        }
    }

    /// Tries to read a `Subroutine` from a byte slice.
    /// On success, returns the `Subroutine` read and the number of bytes
    /// that were read (in other words, the number of bytes to advance the
    /// program counter by).
    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), FromBytesError> {
        if bytes.is_empty() {
            return Err(FromBytesError::NotEnoughBytes {
                expected: 1,
                actual: bytes.len(),
            });
        }

        let name_length = bytes[0] as usize;
        let expected_bytes_len = 1 + name_length + 1 + 4;

        if bytes.len() < expected_bytes_len {
            return Err(FromBytesError::NotEnoughBytes {
                expected: expected_bytes_len,
                actual: bytes.len(),
            });
        }

        let name = match std::str::from_utf8(&bytes[1..=name_length]) {
            Ok(res) => res.to_string(),
            Err(err) => return Err(FromBytesError::Utf8Error(err)),
        };

        let num_arguments = bytes[name_length + 1];

        let start_address = u32::from_le_bytes([
            bytes[name_length + 2],
            bytes[name_length + 3],
            bytes[name_length + 4],
            bytes[name_length + 5],
        ]);

        Ok((
            Self::new(name, num_arguments, start_address as usize),
            expected_bytes_len,
        ))
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn num_arguments(&self) -> u8 {
        self.num_arguments
    }

    pub fn start_address(&self) -> usize {
        self.start_address
    }
}
