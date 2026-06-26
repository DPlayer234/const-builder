#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::style,
    clippy::suspicious
)]
#![allow(non_camel_case_types)]
#![allow(dead_code, unused_macros)]
#![allow(clippy::derive_partial_eq_without_eq)]
// the `unsafe` in the macro expansion should be allowed
#![forbid(unsafe_code)]
// disable the prelude to avoid default trait imports
// it still imports some macros (like assert and derive)
#![no_implicit_prelude]

// shadow core and std imports
mod core {}
mod std {}

// shadow primitives that the macro uses
struct u32;
struct usize;
struct bool;

// shadow macros that are somehow imported no matter what?
macro_rules! assert {
    () => {
        compiler_error!("this macro should go unused");
    };
}

// ensures that the macro can see types in scope
struct Byte(::core::primitive::u8);

#[derive(::const_builder::ConstBuilder)]
struct ShadowSmall {
    #[builder(default = Byte(0))]
    _00: Byte,
    #[builder(default = Byte(1))]
    _01: Byte,
    _02: Byte,
    _03: Byte,
    _04: Byte,
    _05: Byte,
    _06: Byte,
    _07: Byte,
}

#[derive(::const_builder::ConstBuilder)]
struct ShadowLarge {
    #[builder(default = Byte(0))]
    _00: Byte,
    #[builder(default = Byte(1))]
    _01: Byte,
    _02: Byte,
    _03: Byte,
    _04: Byte,
    _05: Byte,
    _06: Byte,
    _07: Byte,
    _08: Byte,
    _09: Byte,
    _10: Byte,
    _11: Byte,
    _12: Byte,
    _13: Byte,
    _14: Byte,
    _15: Byte,
    _16: Byte,
    _17: Byte,
    _18: Byte,
    _19: Byte,
    _20: Byte,
    _21: Byte,
    _22: Byte,
    _23: Byte,
    _24: Byte,
    _25: Byte,
    _26: Byte,
    _27: Byte,
    _28: Byte,
    _29: Byte,
    _30: Byte,
    _31: Byte,
    _32: Byte,
    _33: Byte,
    _34: Byte,
    _35: Byte,
    _36: Byte,
    _37: Byte,
    _38: Byte,
    _39: Byte,
    _40: Byte,
    _41: Byte,
    _42: Byte,
    _43: Byte,
    _44: Byte,
    _45: Byte,
    _46: Byte,
    _47: Byte,
    _48: Byte,
    _49: Byte,
    _50: Byte,
    _51: Byte,
    _52: Byte,
    _53: Byte,
    _54: Byte,
    _55: Byte,
    _56: Byte,
    _57: Byte,
    _58: Byte,
    _59: Byte,
    _60: Byte,
    _61: Byte,
    _62: Byte,
    _63: Byte,
    _64: Byte,
    _65: Byte,
    _66: Byte,
    _67: Byte,
    _68: Byte,
    _69: Byte,
    _70: Byte,
    _71: Byte,
    _72: Byte,
    _73: Byte,
    _74: Byte,
    _75: Byte,
    _76: Byte,
    _77: Byte,
    _78: Byte,
    _79: Byte,
}

#[derive(::const_builder::ConstBuilder)]
#[repr(Rust, packed)]
struct ShadowUnsized<T: ?::core::marker::Sized> {
    #[builder(default = Byte(0))]
    _00: Byte,
    #[builder(default = Byte(1))]
    _01: Byte,
    #[builder(unsized_tail)]
    tail: ::std::mem::ManuallyDrop<T>,
}

#[derive(::const_builder::ConstBuilder)]
#[builder(default)]
struct ShadowDefault {
    #[builder(default = Byte(0))]
    _00: Byte,
    #[builder(default = Byte(1))]
    _01: Byte,
}

#[test]
fn sanity_check() {
    _ = ShadowSmall::builder();
    _ = ShadowLarge::builder();
    _ = ShadowUnsized::<u8>::builder();
    _ = ShadowDefault::builder().build();
}
