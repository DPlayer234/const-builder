use std::borrow::Cow;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;

use const_builder::ConstBuilder;

//#[derive(Debug, PartialEq)]
#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(
    vis = "",
    rename = CreatePerson,
    rename_fn = new,
    unchecked(vis = "pub(crate)", rename = UncheckedCreatePerson)
)]
pub struct Person<'a, T: ?Sized + PartialEq, const VERSION: usize> {
    #[builder(rename = set_first_name)]
    pub first_name: Cow<'a, str>,
    pub last_name: Cow<'a, str>,
    #[builder(default = 0)]
    pub age: u32,
    #[builder(default = None)]
    pub awake_since: Option<u32>,
    #[builder(vis = "" /* priv */)]
    pub unique: T,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(default)]
struct Defaultable {
    #[builder(default = 0)]
    key: u32,
    #[builder(default = Some(0))]
    value: Option<u32>,
    #[builder(default = Cow::Borrowed("unlabelled"))]
    label: Cow<'static, str>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[repr(Rust, packed)]
struct PackedUnsize<T: ?Sized> {
    #[builder(leak_on_drop, default = r#""hello world""#)]
    id: &'static str,
    #[builder(unsized_tail)]
    field: ManuallyDrop<T>,
}

#[derive(ConstBuilder)]
#[expect(dead_code)]
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
fn defaultable() {
    let default = const { Defaultable::default() };
    assert_eq!(default, <Defaultable as Default>::default());
}

#[test]
fn packed_unsize() {
    let _drop_me = PackedUnsize::builder()
        .id("1024")
        .field(ManuallyDrop::new(String::new()));

    let packed = PackedUnsize::builder()
        .id("16")
        .field(ManuallyDrop::new([1u8, 2, 3, 4]))
        .build();

    let _unsized: &PackedUnsize<[u8]> = &packed;
}
