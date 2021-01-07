use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{fmt, str::Utf8Error};
use thiserror::Error;

// Using a `Box<dyn FnMut>` here causes issues
// when trying to call native functions.
pub type NativeFn = fn(&mut Vec<Value>);

#[derive(Clone, Copy, Debug, Error)]
pub enum FromBytesError {
    #[error("Byte slice not big enough (expected {expected} bytes, got {actual})")]
    NotEnoughBytes { expected: usize, actual: usize },

    #[error("Couldn't parse string value: {0}")]
    Utf8Error(#[source] Utf8Error),
    #[error("Unknown type tag {0}")]
    UnknownTypeTag(u8),
}

#[derive(Clone, Copy, Debug, Error)]
pub enum ToBytesError {
    #[error("{0} value too large (expected at most {1} bytes, got {2} bytes)")]
    ValueTooLarge(ValueKind, usize, usize),
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Eq, FromPrimitive, PartialEq)]
pub enum ValueKind {
    Subroutine,
    NativeFn,
    Integer,
    UnsignedInt,
    Float,
    String,
    Unit,
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueKind::Subroutine => write!(f, "subroutine"),
            ValueKind::NativeFn => write!(f, "native function"),
            ValueKind::Integer => write!(f, "integer"),
            ValueKind::UnsignedInt => write!(f, "unsigned integer"),
            ValueKind::Float => write!(f, "float"),
            ValueKind::String => write!(f, "string"),
            ValueKind::Unit => write!(f, "unit"),
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Subroutine(usize),
    NativeFn(NativeFn),
    Integer(i64),
    UnsignedInt(u64),
    Float(f64),
    String(String),
    Unit,
}

impl Value {
    /// Tries to read a `Value` from a byte slice.
    /// On success, returns the `Value` read and the number of bytes
    /// that were read (in other words, the number of bytes to advance the
    /// program counter by).
    pub fn from_bytes(bytes: &[u8]) -> Result<(Self, usize), FromBytesError> {
        if bytes.is_empty() {
            return Err(FromBytesError::NotEnoughBytes {
                expected: 1,
                actual: 0,
            });
        }

        let kind = match ValueKind::from_u8(bytes[0]) {
            Some(res) => res,
            None => return Err(FromBytesError::UnknownTypeTag(bytes[0])),
        };

        match kind {
            ValueKind::Subroutine => todo!(),
            ValueKind::NativeFn => todo!(),

            ValueKind::Integer => {
                // 9 bytes = 1 byte tag + 8 byte value
                if bytes.len() >= 9 {
                    let res = Self::Integer(i64::from_le_bytes([
                        bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                        bytes[8],
                    ]));

                    Ok((res, 9))
                } else {
                    Err(FromBytesError::NotEnoughBytes {
                        expected: 9,
                        actual: bytes.len(),
                    })
                }
            },

            ValueKind::UnsignedInt => {
                if bytes.len() >= 9 {
                    let res = Self::UnsignedInt(u64::from_le_bytes([
                        bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                        bytes[8],
                    ]));

                    Ok((res, 9))
                } else {
                    Err(FromBytesError::NotEnoughBytes {
                        expected: 9,
                        actual: bytes.len(),
                    })
                }
            },

            ValueKind::Float => {
                if bytes.len() >= 9 {
                    let res = Self::Float(f64::from_le_bytes([
                        bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                        bytes[8],
                    ]));

                    Ok((res, 9))
                } else {
                    Err(FromBytesError::NotEnoughBytes {
                        expected: 9,
                        actual: bytes.len(),
                    })
                }
            },

            ValueKind::String => {
                if bytes.len() == 1 {
                    return Err(FromBytesError::NotEnoughBytes {
                        expected: 1,
                        actual: bytes.len(),
                    });
                }

                let str_length = bytes[1] as usize;
                let expected_bytes_length = str_length + 2;
                // The slice includes two extra bytes,
                // one for the tag and one for the length
                if bytes.len() < expected_bytes_length {
                    Err(FromBytesError::NotEnoughBytes {
                        expected: expected_bytes_length,
                        actual: bytes.len(),
                    })
                } else {
                    match std::str::from_utf8(&bytes[2..str_length + 2]) {
                        Ok(res) => Ok((Value::String(res.to_string()), expected_bytes_length)),
                        Err(err) => Err(FromBytesError::Utf8Error(err)),
                    }
                }
            },

            ValueKind::Unit => Ok((Value::Unit, 1)),
        }
    }

    pub fn into_bytes(self) -> Result<Vec<u8>, ToBytesError> {
        let mut res = vec![self.kind() as u8];

        match self {
            Value::Subroutine(_) => todo!("subroutine to_bytes"),
            Value::NativeFn(_) => todo!("native fn to_bytes"),

            Value::Integer(i) => {
                res.extend_from_slice(&i.to_le_bytes());
                Ok(res)
            },

            Value::UnsignedInt(i) => {
                res.extend_from_slice(&i.to_le_bytes());
                Ok(res)
            },

            Value::Float(f) => {
                res.extend_from_slice(&f.to_le_bytes());
                Ok(res)
            },

            Value::String(ref s) => {
                if s.len() > u8::MAX as usize {
                    return Err(ToBytesError::ValueTooLarge(
                        self.kind(),
                        u8::MAX as usize,
                        s.len(),
                    ));
                }

                res.push(s.len() as u8);
                res.reserve(s.len());
                res.extend_from_slice(s.as_bytes());
                Ok(res)
            },

            Value::Unit => Ok(res),
        }
    }

    pub const fn kind(&self) -> ValueKind {
        match self {
            Value::Subroutine(_) => ValueKind::Subroutine,
            Value::NativeFn(_) => ValueKind::NativeFn,
            Value::Integer(_) => ValueKind::Integer,
            Value::UnsignedInt(_) => ValueKind::UnsignedInt,
            Value::Float(_) => ValueKind::Float,
            Value::String(_) => ValueKind::String,
            Value::Unit => ValueKind::Unit,
        }
    }
}
