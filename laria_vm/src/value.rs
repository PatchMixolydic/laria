use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{fmt, str::Utf8Error};
use thiserror::Error;

use crate::{subroutine::Subroutine, vm::VM};

// Using a `Box<dyn FnMut>` here causes issues
// when trying to call native functions.
pub type NativeFn = fn(&mut VM) -> Value;

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
    Byte,
    Tuple,
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
            ValueKind::Byte => write!(f, "byte"),
            ValueKind::Tuple => write!(f, "tuple"),
        }
    }
}

// TODO: is this needed?
#[derive(Clone)]
pub enum Value {
    Subroutine(Subroutine),
    NativeFn(NativeFn),
    Integer(i64),
    UnsignedInt(u64),
    Float(f64),
    String(String),
    Unit,
    Byte(u8),
    Tuple(Vec<Value>),
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
            ValueKind::Subroutine => Subroutine::from_bytes(&bytes[1..])
                .map(|(sub, advance)| (Value::Subroutine(sub), advance)),

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

            ValueKind::Byte => {
                if bytes.len() < 2 {
                    return Err(FromBytesError::NotEnoughBytes {
                        expected: 2,
                        actual: 1,
                    });
                }

                Ok((Value::Byte(bytes[1]), 2))
            },

            ValueKind::Tuple => {
                if bytes.len() < 2 {
                    return Err(FromBytesError::NotEnoughBytes {
                        expected: 2,
                        actual: 1,
                    });
                }

                let num_items = bytes[1];
                let mut contents = Vec::new();
                let mut num_bytes_read = 2;

                for _ in 0..num_items {
                    let (value, bytes) = Self::from_bytes(&bytes[num_bytes_read..])?;
                    contents.push(value);
                    num_bytes_read += bytes;
                }

                Ok((Value::Tuple(contents), num_bytes_read))
            },
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

            Value::Byte(b) => {
                res.push(b);
                Ok(res)
            },

            Value::Tuple(contents) => {
                for item in contents {
                    res.extend_from_slice(&item.into_bytes()?);
                }

                Ok(res)
            },
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
            Value::Byte(_) => ValueKind::Byte,
            Value::Tuple(_) => ValueKind::Tuple,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Subroutine(ref sub) => f.debug_tuple("Subroutine").field(sub).finish(),

            // This is why we need this `Debug` impl...
            Value::NativeFn(func) => f
                .debug_tuple("NativeFn")
                .field(&(func as *const _))
                .finish(),

            Value::Float(x) => f.debug_tuple("Float").field(x).finish(),
            Value::Integer(x) => f.debug_tuple("Integer").field(x).finish(),
            Value::UnsignedInt(x) => f.debug_tuple("UnsignedInt").field(x).finish(),
            Value::String(ref s) => f.debug_tuple("String").field(s).finish(),
            Value::Unit => write!(f, "Unit"),
            Value::Byte(b) => f.debug_tuple("Byte").field(b).finish(),
            Value::Tuple(contents) => f.debug_tuple("Tuple").field(contents).finish(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Subroutine(ref sub) => write!(f, "subroutine `{}`", sub.name()),
            Value::NativeFn(func) => write!(f, "native fn at {:#x}", func as *const _ as usize),
            Value::Float(x) => write!(f, "{}", x),
            Value::Integer(x) => write!(f, "{}", x),
            Value::UnsignedInt(x) => write!(f, "{}", x),
            Value::String(ref s) => write!(f, "{}", s),
            Value::Unit => write!(f, "()"),
            Value::Byte(b) => write!(f, "{}", b),

            Value::Tuple(contents) => {
                write!(f, "(")?;

                contents
                    .iter()
                    .map(ToString::to_string)
                    .intersperse(", ".into())
                    .try_for_each(|item| write!(f, "{}", item))?;

                write!(f, ")")
            },
        }
    }
}
