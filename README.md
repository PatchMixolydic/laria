# Laria
This is the repository for Laria, a (someday) statically typed (eventually)
embeddable scripting language.

Currently, Laria is a very early work in progress, so it is almost definitely
not ready for production use. The codebase may be quite messy, as the project
is currently in the prototyping phase. Many oddities are likely placeholders or
prototypes.

The \[projected] syntax of Laria currently borrows (:P) heavily from Rust.
It is, however, subject to change (and is expected to change!).
Here is an example (note that due to the development state of Laria, this may
be outdated):
<!--
    Rust is close enough to Laria for highlighting.
    TODO: Switch this over if Linguist gets support for Laria
-->
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

    // TODO: figure out a design for operator overloading
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
