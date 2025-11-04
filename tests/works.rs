// enable basically all clippy lints so we can see unexpected
// ones triggering while testing and debugging.
#![warn(
    clippy::complexity,
    clippy::correctness,
    clippy::nursery,
    clippy::pedantic,
    clippy::perf,
    clippy::style,
    clippy::suspicious
)]
#![allow(clippy::derive_partial_eq_without_eq)]

use std::borrow::Cow;
use std::marker::PhantomData;

use const_builder::ConstBuilder;

macro_rules! age_doc {
    () => {
        "The age in years.\n\
         \n\
         Probably shouldn't be set to something absurd."
    };
}

//#[derive(Debug, PartialEq)]
#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(
    vis = "",
    rename = CreatePerson,
    rename_fn = new,
    unchecked(vis = "pub(crate)", rename = UncheckedCreatePerson)
)]
pub struct Person<'a, T: ?Sized + PartialEq, const VERSION: usize> {
    /// The person's first name.
    #[builder(rename = set_first_name)]
    pub first_name: Cow<'a, str>,
    /// The person's last name.
    ///
    /// May include compound names.
    pub last_name: Cow<'a, str>,
    #[doc = age_doc!()]
    #[doc(hidden)]
    #[builder(default = 0)]
    pub age: u32,
    #[builder(default = None, setter(strip_option))]
    pub awake_since: Option<u32>,
    #[builder(vis = "" /* priv */)]
    pub unique: T,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct MutRef<'a> {
    key: u32,
    #[builder(default = &mut [])]
    buf: &'a mut [u8],
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(default)]
struct Defaultable {
    #[builder(default = 0, leak_on_drop)]
    key: u32,
    #[builder(default = Some(0))]
    value: Option<u32>,
    #[builder(default = Cow::Borrowed("unlabelled"))]
    label: Cow<'static, str>,
}

#[derive(ConstBuilder)]
#[allow(dead_code)]
struct LeakAll {
    #[builder(leak_on_drop)]
    key: String,
    #[builder(leak_on_drop)]
    value: String,
}

#[derive(ConstBuilder)]
#[builder(default)]
struct OddDefaults {
    #[builder(default = || 0)]
    f: fn() -> u32,
    #[builder(default = |a| a + 1)]
    g: fn(u32) -> u32,
    #[builder(default = |a, b| a * b + 1)]
    h: fn(u32, u32) -> u32,
    #[builder(default = Cow::Borrowed(&[1, 2, 3]))]
    c: Cow<'static, [u32]>,
    #[builder(default = Some(|a, b| a || b))]
    s: Option<fn(bool, bool) -> bool>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct OddSetters {
    #[builder(setter(transform = |Wrap(v): Wrap<u32>| v))]
    value: u32,
}

struct Wrap<T>(T);

struct TrueOnDrop<'a>(&'a mut bool);

impl Drop for TrueOnDrop<'_> {
    fn drop(&mut self) {
        *self.0 = true;
    }
}

#[derive(ConstBuilder)]
struct EnsureDropsSmall<'a> {
    _00: TrueOnDrop<'a>,
    _01: TrueOnDrop<'a>,
    _02: TrueOnDrop<'a>,
    _03: TrueOnDrop<'a>,
    _04: TrueOnDrop<'a>,
    _05: TrueOnDrop<'a>,
    _06: TrueOnDrop<'a>,
    _07: TrueOnDrop<'a>,
}

#[derive(ConstBuilder)]
struct EnsureDropsLarge<'a> {
    _00: TrueOnDrop<'a>,
    _01: TrueOnDrop<'a>,
    _02: TrueOnDrop<'a>,
    _03: TrueOnDrop<'a>,
    _04: TrueOnDrop<'a>,
    _05: TrueOnDrop<'a>,
    _06: TrueOnDrop<'a>,
    _07: TrueOnDrop<'a>,
    _08: TrueOnDrop<'a>,
    _09: TrueOnDrop<'a>,
    _10: TrueOnDrop<'a>,
    _11: TrueOnDrop<'a>,
    _12: TrueOnDrop<'a>,
    _13: TrueOnDrop<'a>,
    _14: TrueOnDrop<'a>,
    _15: TrueOnDrop<'a>,
    _16: TrueOnDrop<'a>,
    _17: TrueOnDrop<'a>,
    _18: TrueOnDrop<'a>,
    _19: TrueOnDrop<'a>,
    _20: TrueOnDrop<'a>,
    _21: TrueOnDrop<'a>,
    _22: TrueOnDrop<'a>,
    _23: TrueOnDrop<'a>,
    _24: TrueOnDrop<'a>,
    _25: TrueOnDrop<'a>,
    _26: TrueOnDrop<'a>,
    _27: TrueOnDrop<'a>,
    _28: TrueOnDrop<'a>,
    _29: TrueOnDrop<'a>,
    _30: TrueOnDrop<'a>,
    _31: TrueOnDrop<'a>,
    _32: TrueOnDrop<'a>,
    _33: TrueOnDrop<'a>,
    _34: TrueOnDrop<'a>,
    _35: TrueOnDrop<'a>,
    _36: TrueOnDrop<'a>,
    _37: TrueOnDrop<'a>,
    _38: TrueOnDrop<'a>,
    _39: TrueOnDrop<'a>,
    _40: TrueOnDrop<'a>,
    _41: TrueOnDrop<'a>,
    _42: TrueOnDrop<'a>,
    _43: TrueOnDrop<'a>,
    _44: TrueOnDrop<'a>,
    _45: TrueOnDrop<'a>,
    _46: TrueOnDrop<'a>,
    _47: TrueOnDrop<'a>,
    _48: TrueOnDrop<'a>,
    _49: TrueOnDrop<'a>,
    _50: TrueOnDrop<'a>,
    _51: TrueOnDrop<'a>,
    _52: TrueOnDrop<'a>,
    _53: TrueOnDrop<'a>,
    _54: TrueOnDrop<'a>,
    _55: TrueOnDrop<'a>,
    _56: TrueOnDrop<'a>,
    _57: TrueOnDrop<'a>,
    _58: TrueOnDrop<'a>,
    _59: TrueOnDrop<'a>,
    _60: TrueOnDrop<'a>,
    _61: TrueOnDrop<'a>,
    _62: TrueOnDrop<'a>,
    _63: TrueOnDrop<'a>,
    _64: TrueOnDrop<'a>,
    _65: TrueOnDrop<'a>,
    _66: TrueOnDrop<'a>,
    _67: TrueOnDrop<'a>,
    _68: TrueOnDrop<'a>,
    _69: TrueOnDrop<'a>,
    _70: TrueOnDrop<'a>,
    _71: TrueOnDrop<'a>,
    _72: TrueOnDrop<'a>,
    _73: TrueOnDrop<'a>,
    _74: TrueOnDrop<'a>,
    _75: TrueOnDrop<'a>,
    _76: TrueOnDrop<'a>,
    _77: TrueOnDrop<'a>,
    _78: TrueOnDrop<'a>,
    _79: TrueOnDrop<'a>,
}

#[derive(ConstBuilder)]
struct EnsureDropsTail<'a> {
    #[builder(unsized_tail)]
    tail: TrueOnDrop<'a>,
}

#[derive(ConstBuilder)]
struct EnsureLeak<'a> {
    #[builder(leak_on_drop)]
    leak: TrueOnDrop<'a>,
    drop: TrueOnDrop<'a>,
}

#[derive(ConstBuilder)]
#[repr(Rust, packed)]
struct EnsureDropPacked<'a> {
    #[builder(leak_on_drop)]
    leak: TrueOnDrop<'a>,
    drop: TrueOnDrop<'a>,
    // invalid:
    //#[builder(unsized_tail)]
    //tail: TrueOnDrop<'a>,
}

#[test]
fn person() {
    let person = const {
        Person::<'_, _, 2>::new()
            .set_first_name(Cow::Borrowed("steve"))
            .last_name(Cow::Borrowed("smith"))
            .age(32)
            .unique(PhantomData::<()>)
            .build()
    };

    assert_eq!(
        person,
        Person {
            first_name: Cow::Borrowed("steve"),
            last_name: Cow::Borrowed("smith"),
            age: 32,
            awake_since: None,
            unique: PhantomData::<()>,
        }
    );
}

#[test]
fn mut_ref() {
    let mut buf = [0, 1, 2, 3];
    let value = MutRef::builder().key(1).buf(&mut buf).build();

    // let x = &mut buf;
    assert_eq!(
        value,
        MutRef {
            key: 1,
            buf: &mut [0, 1, 2, 3]
        }
    );
}

#[test]
fn defaultable() {
    let default = const { Defaultable::default() };
    assert_eq!(default, <Defaultable as Default>::default());
    assert_eq!(
        default,
        Defaultable {
            key: 0,
            value: Some(0),
            label: "unlabelled".into()
        }
    );
}

#[test]
fn unused_builder() {
    #[expect(unused_must_use)]
    Defaultable::builder();
    #[expect(unused_must_use)]
    Defaultable::builder().key(42);
    #[expect(unused_must_use)]
    Defaultable::builder().build();

    #[expect(unused_must_use)]
    DefaultableUncheckedBuilder::new();
    #[expect(unused_must_use)]
    DefaultableUncheckedBuilder::new().key(42);
    unsafe {
        #[expect(unused_must_use)]
        DefaultableUncheckedBuilder::new().build();
    }
}

#[test]
fn odd_defaults() {
    let odd = OddDefaults::default();

    assert_eq!((odd.f)(), 0);
    assert_eq!((odd.g)(42), 43);
    assert_eq!((odd.h)(4, 8), 33);
    assert_eq!(*odd.c, [1, 2, 3]);
    assert_eq!(odd.s.map(|s| s(true, false)), Some(true));
}

#[test]
fn odd_setter() {
    let odd = const { OddSetters::builder().value(Wrap(42)).build() };

    assert_eq!(odd, OddSetters { value: 42 });
}

#[test]
fn ensure_drops_small() {
    let mut a = false;
    let mut b = false;
    let mut c = false;

    _ = EnsureDropsSmall::builder()
        ._01(TrueOnDrop(&mut a))
        ._03(TrueOnDrop(&mut b))
        ._07(TrueOnDrop(&mut c));

    assert!(a && b && c);
}

#[test]
fn ensure_drops_large() {
    let mut a = false;
    let mut b = false;
    let mut c = false;

    _ = EnsureDropsLarge::builder()
        ._01(TrueOnDrop(&mut a))
        ._34(TrueOnDrop(&mut b))
        ._76(TrueOnDrop(&mut c));

    assert!(a && b && c);
}

#[test]
fn ensure_drops_tail() {
    let mut a = false;

    _ = EnsureDropsTail::builder().tail(TrueOnDrop(&mut a));

    assert!(a);
}

#[test]
fn ensure_leak() {
    let mut a = false;
    let mut b = false;

    _ = EnsureLeak::builder()
        .leak(TrueOnDrop(&mut a))
        .drop(TrueOnDrop(&mut b));

    assert!(!a && b);
}

#[test]
fn ensure_drop_packed() {
    let mut a = false;
    let mut b = false;

    _ = EnsureDropPacked::builder()
        .leak(TrueOnDrop(&mut a))
        .drop(TrueOnDrop(&mut b));

    assert!(!a && b);
}

#[allow(non_camel_case_types)]
#[allow(dead_code, unused_macros)]
mod compile_shadowed {
    use ::std::mem::ManuallyDrop;
    use const_builder::ConstBuilder;

    mod core {}
    mod std {}

    struct usize;
    struct bool;
    trait Sized {}

    macro_rules! assert {
        () => {
            compiler_error!("this macro should go unused");
        };
    }

    #[derive(ConstBuilder)]
    struct ShadowSmall {
        _00: u8,
        _01: u8,
        _02: u8,
        _03: u8,
        _04: u8,
        _05: u8,
        _06: u8,
        _07: u8,
    }

    #[derive(ConstBuilder)]
    struct ShadowLarge {
        _00: u8,
        _01: u8,
        _02: u8,
        _03: u8,
        _04: u8,
        _05: u8,
        _06: u8,
        _07: u8,
        _08: u8,
        _09: u8,
        _10: u8,
        _11: u8,
        _12: u8,
        _13: u8,
        _14: u8,
        _15: u8,
        _16: u8,
        _17: u8,
        _18: u8,
        _19: u8,
        _20: u8,
        _21: u8,
        _22: u8,
        _23: u8,
        _24: u8,
        _25: u8,
        _26: u8,
        _27: u8,
        _28: u8,
        _29: u8,
        _30: u8,
        _31: u8,
        _32: u8,
        _33: u8,
        _34: u8,
        _35: u8,
        _36: u8,
        _37: u8,
        _38: u8,
        _39: u8,
        _40: u8,
        _41: u8,
        _42: u8,
        _43: u8,
        _44: u8,
        _45: u8,
        _46: u8,
        _47: u8,
        _48: u8,
        _49: u8,
        _50: u8,
        _51: u8,
        _52: u8,
        _53: u8,
        _54: u8,
        _55: u8,
        _56: u8,
        _57: u8,
        _58: u8,
        _59: u8,
        _60: u8,
        _61: u8,
        _62: u8,
        _63: u8,
        _64: u8,
        _65: u8,
        _66: u8,
        _67: u8,
        _68: u8,
        _69: u8,
        _70: u8,
        _71: u8,
        _72: u8,
        _73: u8,
        _74: u8,
        _75: u8,
        _76: u8,
        _77: u8,
        _78: u8,
        _79: u8,
    }

    #[derive(ConstBuilder)]
    #[repr(Rust, packed)]
    struct ShadowUnsized<T: ?::core::marker::Sized> {
        #[builder(unsized_tail)]
        tail: ManuallyDrop<T>,
    }
}
