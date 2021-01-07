#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Keyword {
    Fn,
    Loop,
    Spritesheet
}

impl Keyword {
    pub fn from_str(s: impl AsRef<str>) -> Option<Self> {
        match s.as_ref() {
            "fn" => Some(Self::Fn),
            "loop" => Some(Self::Loop),
            "spritesheet" => Some(Self::Spritesheet),
            _ => None
        }
    }

    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::Fn => "fn",
            Keyword::Loop => "loop",
            Keyword::Spritesheet => "spritesheet"
        }
    }
}
