//! Unstable features for the compiler. These settings affect
//! compilation, not the language itself.

use std::convert::TryFrom;
use thiserror::Error;

// TODO: should this just be a tuple struct?
#[derive(Clone, Debug, Error)]
pub enum UnstableFeaturesError {
    #[error("unknown unstable compiler feature `{0}`")]
    UnknownFeature(String),
}

// TODO: should the below definitions be constructed with a macro?

/// A struct that controls which unstable compiler features
/// should be used. If you want no unstable features, the
/// [`Default`] implementation will return an `UnstableFeatures`
/// with all features disabled.
// nb. bool::default returns false
#[derive(Clone, Copy, Debug, Default)]
pub struct UnstableFeatures {
    typecheck: bool,
}

impl UnstableFeatures {
    pub const fn new(typecheck: bool) -> Self {
        Self { typecheck }
    }
}

/// Goes from a list of unstable flags to an `UnstableFeatures`
/// struct. This is useful for command line applications, such as the
/// interpreter/parser.
impl TryFrom<Vec<String>> for UnstableFeatures {
    type Error = UnstableFeaturesError;
    fn try_from(features: Vec<String>) -> Result<Self, Self::Error> {
        let mut res = UnstableFeatures::default();

        for feature in features {
            match feature.as_str() {
                "typecheck" => res.typecheck = true,
                _ => return Err(UnstableFeaturesError::UnknownFeature(feature)),
            }
        }

        Ok(res)
    }
}
