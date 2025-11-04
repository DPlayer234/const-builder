#![no_std]
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

use core::any::TypeId;
use core::marker::PhantomData;
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
#[repr(C, packed(4))]
struct Packed4 {
    // the macro doesn't actually care about field alignment.
    // it will either use all aligned or all unaligned operations.
    aligned: u32,
    unaligned: u64,
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

#[derive(Debug, PartialEq, ConstBuilder)]
struct OddButValidTransforms {
    #[builder(setter(transform = ((|a: i32| a * 2))))]
    wrapped: i32,
    #[builder(setter(transform = "|a: u32| a + 1"))]
    in_literal: u32,
    #[builder(setter(transform = for<'a> |a: &'a u32| *a))]
    with_lifetime: u32,
    // so this wasn't intentional but it parses and emits correctly.
    // may be useful when const traits are stabilized.
    #[builder(setter(transform = for<I: Copy> |a: I| size_of_val(&a)))]
    with_generic: usize,
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

trait WeirdGatTrait {
    type Opt<T, U>;
}

impl<V> WeirdGatTrait for PhantomData<V> {
    type Opt<T, U> = Option<T>;
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct WeirdGatUse<T, U, V> {
    // resolves to `Option<T>`, tests that the behavior for which generic is picked stays
    // consistent across releases and that it keeps compiling. the derive can't actually see the
    // real type, so it can only _guess_ what generic it needs to extract to get the `Option`'s
    // inner type for `strip_option`.
    // the current behavior is "first argument to last path segment", which is `T` here.
    #[builder(setter(strip_option))]
    opt: <PhantomData<U> as WeirdGatTrait>::Opt<T, V>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct DeprecatedFields {
    #[deprecated = "outdated, use `new_field`"]
    #[builder(default = 0)]
    old_field: u32,
    new_field: u64,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct DefaultedGenerics<T, U: Default = u32> {
    value: T,
    #[builder(default = PhantomData)]
    marker: PhantomData<U>,
}

impl<T, U: 'static + Default> DefaultedGenerics<T, U> {
    #[expect(clippy::unused_self)]
    fn u_type_id(&self) -> TypeId {
        TypeId::of::<U>()
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

    let unsized_packed: &PackedUnsize<[u8]> = &packed;

    assert_eq!({ unsized_packed.id }, "16");
    assert_eq!(*unsized_packed.tail, [1u8, 2, 3, 4]);
}

#[test]
fn packed4() {
    let packed = const { Packed4::builder().aligned(42).unaligned(600).build() };

    assert_eq!(
        packed,
        Packed4 {
            aligned: 42,
            unaligned: 600
        }
    );
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

#[test]
fn odd_but_valid_transforms() {
    let value = OddButValidTransforms::builder()
        .wrapped(8)
        .in_literal(6)
        .with_lifetime(&52)
        .with_generic([0u8; 23])
        .build();

    assert_eq!(
        value,
        OddButValidTransforms {
            wrapped: 16,
            in_literal: 7,
            with_lifetime: 52,
            with_generic: 23,
        }
    );
}

#[test]
fn weird_gat_use() {
    // unique target type used for the generic type used by the `strip_option` field
    // to be sure the derive doesn't get the wrong generic
    #[derive(Debug, PartialEq)]
    struct StripTarget;

    let weird_gat: WeirdGatUse<StripTarget, &str, ()> =
        const { WeirdGatUse::builder().opt(StripTarget).build() };

    assert_eq!(
        weird_gat,
        WeirdGatUse {
            opt: Some(StripTarget)
        }
    );
}

#[test]
fn deprecated_fields_new() {
    let value = DeprecatedFields::builder().new_field(48).build();

    assert_eq!(
        value,
        DeprecatedFields {
            #[expect(deprecated)]
            old_field: 0,
            new_field: 48
        }
    );
}

#[test]
fn deprecated_fields_old() {
    #[expect(deprecated)]
    let value = DeprecatedFields::builder()
        .old_field(32)
        .new_field(0)
        .build();

    assert_eq!(
        value,
        DeprecatedFields {
            #[expect(deprecated)]
            old_field: 32,
            new_field: 0
        }
    );
}

#[test]
fn defaulted_generics_use_default() {
    // annotate the type here to enforce using the default on the 2nd generic
    let builder: DefaultedGenericsBuilder<&str> = DefaultedGenerics::builder();
    let value = builder.value("hello world").build();

    assert_eq!(
        value,
        DefaultedGenerics {
            value: "hello world",
            marker: PhantomData
        }
    );
    assert_eq!(value.u_type_id(), TypeId::of::<u32>());
}

#[test]
fn defaulted_generics_imply() {
    // infer the generic by the surrounding context
    // i.e. the default is irrelevant unless we actually annotate types
    let value = DefaultedGenerics::builder().value("goodbye").build();

    assert_eq!(
        value,
        DefaultedGenerics {
            value: "goodbye",
            // does not compile without this turbofish
            marker: PhantomData::<()>
        }
    );
    assert_eq!(value.u_type_id(), TypeId::of::<()>());
}
