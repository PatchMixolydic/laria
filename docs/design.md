# Laria

Laria is a statically typed, embeddable scripting language inspired by Rust.

## Bytecode files

Version 1

Little endian

* Magic number - u32 - lari
* Version - u8
* Length of instruction section (in bytes) - u32
* Length of constants section (in bytes) - u16
* Length of globals section (in bytes) - u16
* Instructions section
    * Instructions/operands - u8
* Constants section
    * Name length - u8
    * Name - string
    * Type tag - u8
    * Data - variable
* Globals section
    * Name length - u8
    * Name - string
    * Type tag - u8
    * Data - variable

## Source files

A source file for a simple script might look something like this:

```rust
fn main() {
    panic("not enough bullets: expected 2**64 + 1, got {}", random());
}
```

The Laria scripting language is a statically typed language, heavily inspired by Rust.
The language comes with these primitive types:
* `i8`, `i16`, `i32`, `i64` - integers of various sizes
* `u8`, `u16`, `u32`, `u64` - unsigned integers
    * Collectively, the signed and unsigned integers are {integer}.
      That is to say, these types effectively subtype {integer} for
      type inference. {integer} is the unnameable type of integer literals.
        * Basically, integer literals can be inferred to be any of these types.
* `f32`, `f64` - single and double precision floats
    * These both subtype {float}, which is the unnameable type of float literals.
* `bool` - boolean
* `string` - String (not str)
* `()` - unit type
* `!` - never (probably only used for inference?)

In addition, there are:
* Tuples: `(T, U, ...)`
* References: `&T`, `&mut T`

Here's a quick tour of the basic language:

```rust
struct Vec2 {
    x: f32;
    y: f32;

    fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }

    fn zero() -> Self {
        Self { x: 0.0, y: 0.0 }
    }

    fn length(&self) -> f32 {
        f32::sqrt(self.x * self.x + self.y * self.y)
    }

    fn normalize(&mut self) {
        let length = self.length();
        self.x /= length;
        self.y /= length;
    }
}

enum Either[T, U] {
    Left(T),
    Right(U)
}

fn naive_pow(base: i32, exp: i32) -> i32 {
    let res = 1;

    for x in 0..exp {
        res *= base;
    }

    res
}

fn main() {
    let my_vec = Vec2::new(4, 0);
    let my_other_vec = Vec2::zero();
    my_other_vec.y = 3;

    my_vec += my_other_vec;
    my_vec.normalize();
    assert_eq!(my_vec.x, 0.8);
    assert_eq!(my_vec.y, 0.6);

    // This is a partial application.
    // It is equivalent to a lambda:
    // let square = |base| naive_pow(base, 2);
    let square = naive_pow(?, 2);
    assert_eq!(square(4), 16);

    // T can't be inferred here,
    // so the type parameters
    // must be explicitly stated
    let my_either = Either[i32, bool]::Right(true);

    match my_either {
        Left(val) => println!("An i32 was inside my_either: {}", val),
        Right(val) => println!("A boolean was inside my_either: {}", val),
    }
}
```

Wonderful! But say we're making a game. How can we make it more amenable to
easily creating content? For example, what if we're developing a bullet
Hell game?

```rust
// Let's start with a simple enemy.
enemy! Fairy {
    // She has 20 health and awards 1000 points when defeated
    hp: 20,
    points: 1000,
    // spriteset! pulls in a spriteset, picking out a spritesheet
    spritesheet: spritesheet!("enemies.tspr", "enemies/common.png"),
    // Default animation scripts to use.
    // { forward, left, right }
    anim_scripts: { fairy_blue, fairy_blue_left, fairy_blue_right },

    // This behaviour is a function that acts on a given Enemy.
    // Here it recieves an implicit `self` paramter.
    // This creates a function `Fairy::behaviour` with the signature
    // `fn(Enemy) -> ()`.
    behaviour {
        self.move_y_over_time(1.0, 64.0, InterpMode::EaseOut);
        wait(1.0);

        loop 3 {
            let angle = 225.0;
            loop 5 {
                // fire_bullet(sprite, colour, speed, angle, aim_for_player)
                self.fire_bullet(sprites::Bullet, colours::Blue, 5.0, angle, false);
                self.fire_bullet(sprites::Bullet, colours::Blue, 3.0, angle, false);
                angle += 22.5;
                wait(0.075);
            }
            wait(0.5);
        }

        wait(1.0);
        self.move_over_time(1.0, 128, -1, InterpMode::EaseIn);
        wait(1.0);
    }
}

// Let's give her a friend!
enemy! RedFairy {
    hp: 40,
    points: 1100,
    // Some predefined spritesheets, for your convenience
    spritesheet: spritesheets::CommonEnemies,
    // ...as well as anim scripts.
    anim_scripts: anim_scripts::RedFairy,

    // Any old function with the signature
    // `fn(Enemy) -> ()` can be used here.
    // Because of this, behaviours can be reused.
    behaviour: Fairy::behaviour
}

// How about a midboss?
boss! Midboss {
    points: 5000,
    spritesheet: spritesheet!("stage1.tspr", "enemies/stage1.png"),
    // Note that this line ends with a semicolon.
    // This is used to seperate the configuration fields
    // from the boss sequence definition.
    anim_scripts: { miniboss, miniboss_left, miniboss_right };

    // `init` is meant for things like moving the boss onscreen, waiting for dialogue, etc.
    // After the `init` block finishes, the boss moves on to their first attack pattern.
    // `init` and the boss's attack patterns recieve a parameter, `self`,
    // which refers to the boss.
    init {
        self.move_over_time(1.0, 96.0, 64.0, InterpMode::EaseOut);
        // Don't wait for the movement to finish
    }

    // Without further ado, let's define this boss's attack sequence.
    // Her healthbar:
    healthbar 3000 {
        // Healthbars are divided into attack patterns by HP and timeouts.
        // `hp x` indicates that the boss will start this pattern when they
        // have x hp, and `timeout y` indicates that this pattern will timeout
        // after y seconds (nb. x: {integer}, y: {float}).
        // Attack patterns loop automatically.
        // `start` is synonymous with `hp [max hp]`
        start, timeout 30.0 {
            // fire_bullet_ring(sprite, colour, num_bullets, speed, aim_for_player)
            self.fire_bullet_ring(sprites::Bullet, colours::Blue, 10, 5.0, false);
            wait(0.075);
            self.fire_bullet_ring(sprites::Bullet, colours::Red, 12, 6.0, false);
            wait(0.1);
            self.fire_bullet_ring(sprites::Bullet, colours::Green, 15, 6.5, false);
            wait(0.5);

            // move_with_rand_angle(time, speed, interp_mode)
            self.move_with_rand_angle(1.0, 5.0, InterpMode::EaseInOut);
            wait(1.0);
        }

        // Once the boss hits 1500 HP or the above attack pattern is timed out,
        // this pattern starts.
        hp 1500, timeout 30.0 {
            begin_spell("Pragmatist \"Let Someone Else Handle It\"");

            loop {
                spawn_enemy(YellowFairy, 64.0, -1.0);
                wait(0.5);
                spawn_enemy(YellowFairy, 96.0, -1.0);
                wait(0.5);
                spawn_enemy(YellowFairy, 32.0, -1.0);
                wait(1.0);
            }
        }
    }

    // This miniboss only has one healthbar. She is now defeated.
    // However, if the boss timed out, we want her to run away instead of
    // exploding.
    end {
        if self.last_attack_timed_out {
            // Timed out, run away
            wait(0.5);
            self.move_over_time(1.0, -1, 128.0, InterpMode::EaseIn);
            wait(1.0);
        } else {
            // She lost :(
            self.boss_explosion();
        }
    }
}
```

But what if we're not making such a game? Covering everyone's desires with one
specialized langauge would be rather difficult. This could be alleviated with
macros.

### macros_rule!

**Note:** Like the rest of this document, this is a work in progress.

There would be two kinds of macros: declarative macros_rules! by example 1.0 and
syntax extensions (placeholder names). Syntax extensions would be defined
by the host application, while declarative macros could be defined in-language.

```rust
// Let's define a declarative macro that counts exclamation marks
pub macro count_excls! {
    () => {
        0
    },

    // Matcher syntax isn't final
    (! $($rest:tt)*) => {
        1 + count_excls!($($rest)*)
    }
}

fn main() {
    let x = count_excls!(!!!!!);
    let y = count_excls!();

    // x = 5, y = 0
    println!("x = {}, y = {}", x, y);
}
```

Macros are hygenic by default, in that they don't capture values that aren't
provided in their arguments (with the exception of top-level modules and
`root`), and they don't leak identifiers that are created within the macro.
It's as if macros are their own modules, but they have no access to `super`.

```rust
struct MyInterest;

macro cpp_template_error! {
    () => {
        // error: `MyInterest` cannot be accessed from this macro
        // help: try importing the struct
        // pique(MyInterest)
    }
}

macro osana_reimu! {
    () => {
        use root::MyInterest;

        // only about half as sad as the above,
        // so it's still within that comfortable range
        pique(MyInterest)
    }
}
```

You can, however, allow identifiers to escape the macro:
```rust
// Have I mentioned that this is all a work in progress?

macro doesnt_make_unit_struct! {
    ($($visibility:vis)? $name:ident) => {
        // $name is only visible to the macro.
        $($visibility)? struct $name;
    }
}

pub macro make_unit_struct! {
    ($($visibility:vis)? $name:ident) => {
        // $name escapes into the invocation site.
        $($visibility)? struct ~$name;
    }
}

make_unit_struct!(Foo);
make_unit_struct!(Bar);
// error: `Foo` is defined multiple times
// make_unit_struct!(Foo);

// no problem
doesnt_make_unit_struct!(Foo);
```

Whereas declarative macros are defined within the language,
it's likely that syntax extensions will be restricted to the host application.
I have no idea what the API for defining a syntax extension will look like,
so please stay tuned.

Regardless, syntax extensions would be fairly freeform. A syntax extension used
to describe spritesheets in a video game might end up looking like this:
```rust
spriteset! "resources/sprites/enemy.tspr", 512, 512 {
    fairy = (0, 0, 16, 16),
    dynamic_typing = (16, 0, 16, 16),
    seija = (32, 0, 32, 32),
}
```

You could also have something like this:

```rust
// what!
c! {
    void foobar(int x, unsigned char y) {
        for (int i = 0; i < x; i++) {
            printf("%c", y);
        }

        printf("\n");
    }
}

fn main() {
    // prints "aaaaa\n"
    foobar(5, 'a' as u8);
}
```

The implementation of this syntax extension is, of course, left as an
exercise to the reader.
<sup id="ref-1" aria-label="Footnote 1" title="Footnote 1">[1](#footnote-1)</sup>

----

<small id="footnote-1">1.
    <a href="#ref-1" aria-label="Go back" title="Go back">^</a>
    Hint: the optimal solution is to leave it as an exercise for someone else.
    For more information, see most other creepy stories.
</small>
