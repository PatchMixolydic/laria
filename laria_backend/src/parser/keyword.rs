#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Keyword {
    Fn,
    Loop,
    While,
    For,
    Return,
    Let,
    If,
    Else,
    As,
    Macro,
    Pub,
    Mod,
    Extern,
    Const,
    In,
    Struct,
    Enum,
    Yield,
    Match,
}

impl Keyword {
    pub fn from_str(s: impl AsRef<str>) -> Option<Self> {
        match s.as_ref() {
            "fn" => Some(Self::Fn),
            "loop" => Some(Self::Loop),
            "while" => Some(Self::While),
            "for" => Some(Self::For),
            "return" => Some(Self::Return),
            "let" => Some(Self::Let),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "as" => Some(Self::As),
            "macro" => Some(Self::Macro),
            "pub" => Some(Self::Pub),
            "mod" => Some(Self::Mod),
            "extern" => Some(Self::Extern),
            "const" => Some(Self::Const),
            "in" => Some(Self::In),
            "struct" => Some(Self::Struct),
            "enum" => Some(Self::Enum),
            "yield" => Some(Self::Yield),
            "match" => Some(Self::Match),
            _ => None,
        }
    }

    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::Fn => "fn",
            Keyword::Loop => "loop",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Return => "return",
            Keyword::Let => "let",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::As => "as",
            Keyword::Macro => "macro",
            Keyword::Pub => "pub",
            Keyword::Mod => "mod",
            Keyword::Extern => "extern",
            Keyword::Const => "const",
            Keyword::In => "in",
            Keyword::Struct => "struct",
            Keyword::Enum => "enum",
            Keyword::Yield => "yield",
            Keyword::Match => "match",
        }
    }
}
