use crate::errors::Span;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Spritesheet {
    pub name: String,
    pub width: u16,
    pub height: u16,
    pub sprites: Vec<Sprite>,
    pub span: Span
}

impl Spritesheet {
    pub fn new(name: String, width: u16, height: u16, sprites: Vec<Sprite>, span: Span) -> Self {
        Self {
            name,
            width,
            height,
            sprites,
            span
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Sprite {
    pub name: String,
    pub top: u32,
    pub left: u32,
    pub width: u32,
    pub height: u32,
    pub span: Span
}

impl Sprite {
    pub fn new(name: String, top: u32, left: u32, width: u32, height: u32, span: Span) -> Self {
        Self {
            name,
            top,
            left,
            width,
            height,
            span
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Spriteset {
    pub spritesheets: Vec<Spritesheet>,
    pub scripts: Vec<()>,
    pub span: Span
}

impl Spriteset {
    pub const fn new() -> Self {
        Self {
            spritesheets: Vec::new(),
            scripts: Vec::new(),
            span: Span::empty()
        }
    }
}
