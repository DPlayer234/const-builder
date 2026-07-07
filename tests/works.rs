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
use std::sync::Mutex;

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

#[derive(ConstBuilder)]
struct SkippedDrop<'a> {
    #[builder(skip, default = None)]
    skipped: Option<TrueOnDrop<'a>>,
}

#[derive(ConstBuilder, PartialEq, Debug)]
struct RenameSkip {
    #[builder(skip, default = 0, rename = unchecked_private)]
    private: u32,
}

struct NonCopy;

// clippy lints on `drop(value_non_drop)`, but only if the it's not also `Copy`
// for some reason. we only use that pattern for `repr(packed)`, so ensure we
// don't trigger the lint.
#[allow(dead_code)]
#[derive(ConstBuilder)]
#[repr(Rust, packed)]
struct NoClippyDropNonDropWarn {
    field: NonCopy,
}

#[derive(ConstBuilder)]
struct CellRefDefault<'a> {
    #[builder(default = {
        static M: Mutex<usize> = Mutex::new(0);
        &M
    })]
    mutex: &'a Mutex<usize>,
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

#[test]
fn skipped_drop() {
    let mut a = false;

    // SAFETY: equivalent to what the setter for `skipped` would've been
    let _: SkippedDropBuilder<'_> = unsafe {
        SkippedDrop::builder()
            .into_unchecked()
            .skipped(Some(TrueOnDrop(&mut a)))
            .assert_init()
    };

    assert!(!a);
}

#[test]
fn rename_skip() {
    // SAFETY: equivalent to what the setter for `private` would've been
    let value = unsafe {
        RenameSkip::builder()
            .into_unchecked()
            .unchecked_private(42)
            .build()
    };

    assert_eq!(value, RenameSkip { private: 42 });
}

#[test]
fn cell_ref_default() {
    let obj1 = CellRefDefault::builder().build();
    *obj1.mutex.lock().unwrap() = 16;

    let obj2 = CellRefDefault::builder().build();
    assert_eq!(*obj2.mutex.lock().unwrap(), 16);
    *obj2.mutex.lock().unwrap() = 42;

    let obj3 = CellRefDefault::builder().build();
    assert_eq!(*obj3.mutex.lock().unwrap(), 42);

    assert_eq!(&raw const *obj1.mutex, &raw const *obj2.mutex);
    assert_eq!(&raw const *obj1.mutex, &raw const *obj3.mutex);
}

#[test]
fn cell_ref_override() {
    let local_mutex = Mutex::new(16);

    let obj1 = CellRefDefault::builder().mutex(&local_mutex).build();
    *obj1.mutex.lock().unwrap() = 16;

    let obj2 = CellRefDefault::builder().mutex(&local_mutex).build();
    assert_eq!(*obj2.mutex.lock().unwrap(), 16);
    *obj2.mutex.lock().unwrap() = 42;

    let obj3 = CellRefDefault::builder().mutex(&local_mutex).build();
    assert_eq!(*obj3.mutex.lock().unwrap(), 42);

    let obj_global = CellRefDefault::builder().build();

    assert_eq!(&raw const *obj1.mutex, &raw const *obj2.mutex);
    assert_eq!(&raw const *obj1.mutex, &raw const *obj3.mutex);
    assert_ne!(&raw const *obj1.mutex, &raw const *obj_global.mutex);
}
