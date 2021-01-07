//! This module holds functions that lower spritesets from an AST
//! to a [mountain_river `Spriteset`][mountain_river::spriteset::Spriteset].

use mountain_river::spriteset::{
    Sprite as MRSprite, Spriteset as MRSpriteset, Spritesheet as MRSpritesheet
};

use crate::spriteset::ast::{Sprite, Spriteset, Spritesheet};

fn lower_sprite(sprite: Sprite) -> MRSprite {
    MRSprite::new(sprite.top, sprite.left, sprite.width, sprite.height)
}

fn lower_spritesheet(spritesheet: Spritesheet) -> MRSpritesheet {
    let sprites = spritesheet.sprites.into_iter().map(lower_sprite).collect();

    MRSpritesheet::new(
        spritesheet.name,
        spritesheet.width,
        spritesheet.height,
        sprites
    )
}

/// The entry point for lowering.
pub fn lower_spriteset(spriteset: Spriteset) -> MRSpriteset {
    let lowered_spritesheets = spriteset
        .spritesheets
        .into_iter()
        .map(lower_spritesheet)
        .collect();

    MRSpriteset::new(lowered_spritesheets)
}
