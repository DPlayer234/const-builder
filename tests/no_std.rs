#![no_std]

use core::mem::ManuallyDrop;

use const_builder::ConstBuilder;

#[derive(Debug, PartialEq, ConstBuilder)]
pub struct Person<'a> {
    pub name: &'a str,
    #[builder(default = 0)]
    pub age: u32,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(default)]
struct Defaultable {
    #[builder(default = 0, leak_on_drop)]
    key: u32,
    #[builder(default = Some(0))]
    value: Option<u32>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[repr(Rust, packed)]
struct PackedUnsize<T: ?Sized> {
    #[builder(default = r#""hello world""#)]
    id: &'static str,
    #[builder(unsized_tail)]
    tail: ManuallyDrop<T>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct StripOption {
    #[builder(setter(strip_option))]
    value: Option<u32>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct Sum {
    #[builder(setter(transform = |a: u32, b: u32| a + b))]
    value: u32,
}

struct Droppable;
impl Drop for Droppable {
    fn drop(&mut self) {}
}

#[expect(
    non_camel_case_types,
    reason = "struct builder types may start with lowercase letter"
)]
#[expect(
    non_upper_case_globals,
    reason = "generic `r#const` should be uppercase"
)]
mod raw {
    // ensure raw idents compile in every possible position
    use const_builder::ConstBuilder;

    #[derive(Debug, PartialEq, ConstBuilder)]
    pub struct r#struct {
        pub r#fn: r#bool,
        pub r#type: &'r#static r#str,
        #[builder(
            default = None,
            setter(strip_option),
            rename = r#pub,
            rename_generic = r#const,
        )]
        pub r#next: r#Option<&'r#static r#struct>,
    }

    #[allow(dead_code)]
    #[derive(Debug, PartialEq, ConstBuilder)]
    #[builder(rename = r#use, rename_fn = r#struct, unchecked(rename = r#let))]
    struct r#mod {
        _f: (),
    }
}

#[test]
fn no_std_person() {
    const STEVE: Person<'_> = const {
        Person::builder()
            .name("steve smith")
            // keep the default for age
            .build()
    };

    assert_eq!(
        STEVE,
        Person {
            name: "steve smith",
            age: 0
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
        }
    );
}

#[test]
fn packed_unsize() {
    let _drop_me = PackedUnsize::builder()
        .id("1024")
        .tail(ManuallyDrop::new(Droppable));

    let packed = PackedUnsize::builder()
        .id("16")
        .tail(ManuallyDrop::new([1u8, 2, 3, 4]))
        .build();

    let _unsized: &PackedUnsize<[u8]> = &packed;
}

#[test]
fn strip_option() {
    let strip = StripOption::builder().value(16).build();
    assert_eq!(strip, StripOption { value: Some(16) });
}

#[test]
fn transform_sum() {
    let sum = Sum::builder().value(4, 6).build();
    assert_eq!(sum, Sum { value: 10 });
}

#[test]
fn raw_idents() {
    const ROOT: raw::r#struct = raw::r#struct::builder().r#fn(false).r#type("root").build();

    let raw = raw::r#struct::builder()
        .r#fn(true)
        .r#type("name")
        .r#pub(&ROOT)
        .build();

    assert_eq!(
        raw,
        raw::r#struct {
            r#fn: true,
            r#type: "name",
            r#next: Some(&ROOT),
        }
    );
}
