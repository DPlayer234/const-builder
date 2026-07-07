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

/// Helper trait for `dyn` with reference-identity equality.
trait Object: core::fmt::Debug {}
impl<T: core::fmt::Debug> Object for T {}

impl PartialEq for dyn Object + '_ {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self, other)
    }
}

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

const CONST_STR: &str = "hello world";

#[derive(Debug, PartialEq, ConstBuilder)]
struct DefaultStr {
    #[builder(default = r#""hello world""#)]
    double_quote: &'static str,
    #[builder(default = ("hello world"))]
    parens: &'static str,
    #[builder(default = CONST_STR)]
    from_const: &'static str,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[repr(Rust, packed)]
struct PackedUnsize<T: ?Sized> {
    #[builder(default = ("hello world"))]
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
struct StripOptionSpecial<'a, T> {
    #[builder(setter(strip_option))]
    dyn_trait: Option<&'a dyn Object>,
    #[builder(setter(strip_option))]
    gen_value: Option<T>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct Sum {
    #[builder(setter(transform = |a: u32, b: u32| a + b))]
    value: u32,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct OddSetters {
    #[builder(setter(transform = |Wrap(v): Wrap<u32>| v))]
    value: u32,
}

struct Wrap<T>(T);

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
#[expect(clippy::use_self)]
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

// since the emitted code necessarily uses the deprecated struct, it will emit
// deprecation warnings. use a module and suppress warnings for that whole
// module.
#[expect(deprecated)]
mod deprecated_struct {
    #[derive(Debug, PartialEq, super::ConstBuilder)]
    #[builder(unchecked(vis = "pub(crate)"))]
    #[deprecated = "outdated, use literally anything else"]
    pub struct DeprecatedStruct {
        pub field: u32,
    }
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

#[derive(Debug, PartialEq)]
struct PanicDrop(usize);
impl Drop for PanicDrop {
    fn drop(&mut self) {
        panic!("this value must not be dropped");
    }
}

// used to assert leak behavior
#[derive(Debug, PartialEq, ConstBuilder)]
struct HasPanicDropField {
    #[builder(default = PanicDrop(0))]
    field: PanicDrop,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct HasSkippedFields {
    #[builder(skip, default = false)]
    marker: bool,
    key: i16,
    value: u32,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[repr(Rust, packed)]
struct PackedHasSkippedFields {
    #[builder(skip, default = false)]
    marker: bool,
    key: i16,
    value: u32,
}

// assert some complex edge case behavior
#[derive(Debug, PartialEq, ConstBuilder)]
struct ComplexEdgeCases {
    // struct name, field types and field names are duplicated,
    // so those tokens must be equivalent when duplicated
    #[builder(default = {
        // this will error if the definition is duplicated
        #[expect(non_local_definitions, dead_code)]
        impl ComplexEdgeCases {
            const fn single_emit_default() {}
        }
    })]
    single_emit_default: (),
    #[builder(setter(transform = |/* types here are duplicated */| {
        // this will error if the definition is duplicated
        #[expect(non_local_definitions, dead_code)]
        impl ComplexEdgeCases {
            const fn single_emit_transform() {}
        }
    }))]
    single_emit_transform: (),
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
fn default_str() {
    let default = DefaultStr::builder().build();

    assert_eq!(
        default,
        DefaultStr {
            double_quote: "hello world",
            parens: "hello world",
            from_const: "hello world",
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
fn strip_option_special() {
    // miri later considers the `dyn Object` refs different if they are created
    // separately on both sides of the comparison.
    let dyn_value = 42u32;
    let dyn_value: &dyn Object = &dyn_value;

    let strip = StripOptionSpecial::builder()
        .dyn_trait(dyn_value)
        .gen_value(60usize)
        .build();

    assert_eq!(
        strip,
        StripOptionSpecial {
            dyn_trait: Some(dyn_value),
            gen_value: Some(60usize)
        }
    );
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
fn odd_setter() {
    let odd = const { OddSetters::builder().value(Wrap(42)).build() };

    assert_eq!(odd, OddSetters { value: 42 });
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
    let _ = <DeprecatedFieldsBuilder>::old_field;
    #[expect(deprecated)]
    let _ = DeprecatedFieldsUncheckedBuilder::old_field;

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
fn deprecated_struct() {
    use deprecated_struct::*;

    #[expect(deprecated)]
    let _: DeprecatedStructBuilder;
    #[expect(deprecated)]
    let _: DeprecatedStructUncheckedBuilder;

    #[expect(deprecated)]
    let value = DeprecatedStructBuilder::new().field(42).build();
    #[expect(deprecated)]
    let compare = DeprecatedStruct { field: 42 };

    assert_eq!(value, compare);
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

#[test]
fn defaulted_panic_drop_field() {
    // since replacing the default value is likely never going to be changed to drop
    // the default (see comment in `replace_panic_drop_field`), that value should
    // also not be dropped if the builder is dropped

    // defaulted field is always leaked
    let _panic_drop = HasPanicDropField::builder();
}

#[test]
fn replace_panic_drop_field() {
    // while `[const] Destruct` may be used once it's stable, using such a bound
    // here would prevent setting fields of types such as `Cow<'_, B>` in const

    // replaced defaulted field is leaked
    let panic_drop = HasPanicDropField::builder().field(PanicDrop(42)).build();

    // wrap the struct so it doesn't panic on drop
    let no_drop = ManuallyDrop::new(panic_drop);
    assert_eq!(
        no_drop,
        ManuallyDrop::new(HasPanicDropField {
            field: PanicDrop(42),
        })
    );
}

#[test]
fn skipped_fields() {
    let has_skipped = const { HasSkippedFields::builder().key(32).value(16).build() };

    assert_eq!(
        has_skipped,
        HasSkippedFields {
            marker: false,
            key: 32,
            value: 16,
        }
    );
}

#[test]
fn skipped_fields_unchecked() {
    // `marker` is available on the unchecked variant
    let builder = HasSkippedFieldsUncheckedBuilder::new()
        .marker(true)
        .key(1)
        .value(2);

    // SAFETY: all fields are initialized
    let builder = unsafe {
        // no const-generic corresponds to `marker`
        builder.assert_init::</* _KEY */ true, /* _VALUE */ true>()
    };

    let has_skipped = builder.build();

    assert_eq!(
        has_skipped,
        HasSkippedFields {
            marker: true,
            key: 1,
            value: 2,
        }
    );
}

#[test]
fn packed_skipped_fields() {
    let has_skipped = PackedHasSkippedFields::builder().key(32).value(16).build();

    assert_eq!(
        has_skipped,
        PackedHasSkippedFields {
            marker: false,
            key: 32,
            value: 16,
        }
    );
}

#[test]
fn complex_edge_cases() {
    _ = ComplexEdgeCases::builder()
        .single_emit_default(())
        .single_emit_transform()
        .build();
}
