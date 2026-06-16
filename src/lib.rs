//! Provides a [`ConstBuilder`] derive macro that generates a `*Builder` type
//! that can be used to create a value with the builder pattern, even in a const
//! context.
//!
//! The attributed type will gain an associated `builder` method, which can then
//! be chained with function calls until all required fields are set, at which
//! point you can call `build` to get the final value.
//!
//! Compile-time checks prevent setting the same field twice or calling `build`
//! before all required fields are set.
//!
//! By default, the `*Builder` type and `*::builder` method have the same
//! visibility as the type, and every field setter is `pub`, regardless of field
//! visibility.
//!
//! The builder isn't clonable and its methods are called by-value, returning
//! the updated builder value.
//!
//! The generated code is supported in `#![no_std]` crates.
//!
//! # Unsafety
//!
//! This derive macro generates `unsafe` code using
//! [`MaybeUninit`](std::mem::MaybeUninit) to facilitate field-wise
//! initialization of a struct, tracking initialized fields via const-generics.
//! This broadly follows the guidance in the [nomicon section on unchecked
//! uninitialized memory], and is, for now, required to get const-compatible
//! builders for arbitrary types.
//!
//! # Struct Requirements
//!
//! All struct fields must be [`Sized`]. Fields using generic parameters may be
//! [`?Sized`](Sized) for some parameters, as long as the actual instantiation
//! of the builder only has [`Sized`] fields.
//!
//! When the struct is `#[repr(packed)]` and the last field may be
//! [`?Sized`](Sized), the field must be attributed with
//! `#[builder(unsized_tail)]` to replace the field drop code with an assert
//! that the field cannot be dropped. Functionally, this combination of packed,
//! unsized tails, and with the builder requirements means that this field has
//! to be [`ManuallyDrop<T: ?Sized>`](std::mem::ManuallyDrop) or a wrapper
//! around it.
//!
//! `enum` and `union` types are unsupported.
//!
//! # Default Values
//!
//! Fields can be attributed with `#[builder(default = None)]` or similar to
//! be made optional by providing a default value.
//!
//! This default value will _not_ be dropped if it is overridden or the builder
//! is dropped. Consequently, the value should be something that does not need
//! to be dropped, such as a primitive, [`None`], [`String::new()`],
//! [`Vec::new()`], [`Cow::Borrowed`](std::borrow::Cow), or similar.
//!
//! # API Stability
//!
//! The API of the emitted builder is considered forward-compatible as long as
//! care is taken to ensure the surface area to consumers stays the same and the
//! changes don't already constitute as breaking to consumers of the original
//! struct.
//!
//! Because each field gets an associated const-generic parameter on the builder
//! struct, even private fields with private setters will show up in the public
//! API of the builder. To avoid exposing a field in the public API, use the
//! `skip` attribute.
//!
//! Adding the `default` attribute to the struct or a field is
//! forward-compatible. Removing the attribute is a breaking change.
//!
//! Additionally, changes to builder attributes that lead to reduction in
//! visibility, renames, removal, or changes in signature of functions in the
//! emitted code are also breaking changes. This includes attributes such `vis`,
//! `rename`, `rename_fn`, `setter`, and `skip`.
//!
//! Major versions of this crate may also introduce breaking changes to the
//! emitted structs. Minor versions will ensure to emit forward-compatible code.
//!
//! # Unchecked Builder
//!
//! There is also an `*UncheckedBuilder` without safety checks, which is private
//! by default. While similar to the checked builder at a glance, not every
//! attribute applies to it in the same way (f.e. the `setter` attribute has no
//! effect), and its API isn't considered stable across source struct
//! modifications, so it should not be exposed in stable public interfaces.
//!
//! This struct is used to simplify the implementation of the checked builder
//! and it is exposed for users that want additional control.
//!
//! This builder works broadly in the same way as the checked builder, however:
//!
//! - initialized fields aren't tracked,
//! - setting fields that were already set will forget the old value,
//! - calling `build` is unsafe due to the lack of tracking, and
//! - dropping it will forget all field values that were already set.
//!
//! You can convert between the checked and unchecked builder with
//! `*Builder::into_unchecked` and `*UncheckedBuilder::assert_init`.
//!
//! # Example
//!
//! ```
//! use const_builder::ConstBuilder;
//!
//! #[derive(ConstBuilder)]
//! # #[derive(Debug, PartialEq)]
//! pub struct Person<'a> {
//!     // fields are required by default
//!     pub name: &'a str,
//!     // optional fields have a default specified
//!     // the value is required even when the type implements `Default`!
//!     #[builder(default = 0)]
//!     pub age: u32,
//! }
//!
//! let steve = const {
//!     Person::builder()
//!         .name("steve smith")
//!         .build()
//! };
//! # assert_eq!(
//! #     steve,
//! #     Person {
//! #         name: "steve smith",
//! #         age: 0,
//! #     }
//! # );
//! ```
//!
//! # Generated Interface
//!
//! The example above would generate an interface similar to the following. The
//! actual generated code is more complex because it includes bounds to ensure
//! fields are only written once and that the struct is fully initialized when
//! calling `build`.
//!
//! ```
//! # // This isn't an example to run, just an example "shape".
//! # _ = stringify!(
//! /// A builder type for [`Person`].
//! pub struct PersonBuilder<'a, const _NAME: bool = false, const _AGE: bool = false> { ... }
//!
//! impl<'a, ...> PersonBuilder<'a, ...> {
//!     /// Creates a new builder.
//!     pub const fn new() -> Self;
//!
//!     /// Returns the finished value.
//!     ///
//!     /// This function can only be called when all required fields have been set.
//!     pub const fn build(self) -> Person<'a>;
//!
//!     // one setter function per field
//!     pub const fn name(self, value: &'a str) -> PersonBuilder<'a, ...>;
//!     pub const fn age(self, value: u32) -> PersonBuilder<'a, ...>;
//!
//!     /// Unwraps this builder into its unsafe counterpart.
//!     ///
//!     /// This isn't unsafe in itself, however using it carelessly may lead to
//!     /// leaking objects and not dropping initialized values.
//!     const fn into_unchecked(self) -> PersonUncheckedBuilder<'a>;
//! }
//!
//! impl<'a> Person<'a> {
//!     /// Creates a new builder for this type.
//!     pub const fn builder() -> PersonBuilder<'a>;
//! }
//!
//! /// An _unchecked_ builder type for [`Person`].
//! ///
//! /// This version being _unchecked_ means it has less safety guarantees:
//! /// - No tracking is done whether fields are initialized, so [`Self::build`] is `unsafe`.
//! /// - If dropped, already initialized fields will be leaked.
//! /// - The same field can be set multiple times. If done, the old value will be leaked.
//! struct PersonUncheckedBuilder<'a> { ... }
//!
//! impl<'a> PersonUncheckedBuilder<'a> {
//!    /// Creates a new unchecked builder.
//!    pub const fn new() -> Self;
//!
//!    /// Asserts that the fields specified by the const generics as well as all optional
//!    /// fields are initialized and promotes this value into a checked builder.
//!    ///
//!    /// # Safety
//!    ///
//!    /// The fields whose const generic are `true` and all optional fields have to be
//!    /// initialized.
//!    ///
//!    /// Optional fields are initialized by [`Self::new`] by default, however using
//!    /// [`Self::as_uninit`] allows de-initializing them. This means that this function
//!    /// isn't even necessarily safe to call if all const generics are `false`.
//!    pub const unsafe fn assert_init<const _NAME: bool, const _AGE: bool>(self) -> PersonBuilder<'a, _NAME, _AGE>;
//!
//!    /// Returns the finished value.
//!    ///
//!    /// # Safety
//!    ///
//!    /// This function requires that all fields have been initialized.
//!    pub const unsafe fn build(self) -> Person<'a>;
//!
//!    // one setter function per field
//!    pub const fn name(mut self, value: &'a str) -> Self;
//!    pub const fn age(mut self, value: u32) -> Self;
//!
//!    /// Gets a mutable reference to the partially initialized data.
//!    pub const fn as_uninit(&mut self) -> &mut ::core::mem::MaybeUninit<Person<'a>>;
//! }
//! # );
//! ```
//!
//! # Struct Attributes
//!
//! These attributes can be specified within `#[builder(...)]` on the struct
//! level.
//!
//! | Attribute                   | Meaning |
//! |:--------------------------- |:------- |
//! | `default`                   | Generate a const-compatible `*::default()` function and [`Default`] derive. Requires every field to have a default value. |
//! | `vis = "$vis"`              | Change the visibility of the builder type. May be an empty string for private. Default is the same as the struct. |
//! | `rename = $name`            | Renames the builder type. Defaults to "`<Type>Builder`". |
//! | `rename_fn = $name`         | Renames the associated function that creates the builder. Defaults to `builder`. Set to `false` to disable. |
//! | `unchecked(vis = "$vis")`   | Change the visibility of the unchecked builder type. Default is private. |
//! | `unchecked(rename = $name)` | Renames the unchecked builder type. Defaults to "`<Type>UncheckedBuilder`". |
//!
//! # Field Attributes
//!
//! These attributes can be specified within `#[builder(...)]` on the struct's
//! fields.
//!
//! | Attribute                      | Meaning |
//! |:------------------------------ |:------- |
//! | `vis = "$vis"`                 | Change the visibility of the builder's field setter. May be an empty string for private. Default is `pub`. If you intend to hide the field from the public API, prefer `skip`. |
//! | `default = $value`             | Make the field optional by providing a default value. The value must be evaluatable in `const`. |
//! | `rename = $name`               | Renames the setters for this field. Defaults to the field name. |
//! | `rename_generic = $name`       | Renames the name of the associated const generic. Defaults to "`_{field:upper}`". |
//! | `leak_on_drop`                 | Instead of dropping the field when dropping the builder, do nothing. |
//! | `unsized_tail`                 | In a packed struct, marks the last field as potentially being unsized, replacing the drop code with an assert. No effect if the struct isn't packed. |
//! | `setter(transform = $closure)` | Accepts closure syntax. The setter is changed to accept its inputs and set the corresponding value to its output. Parameter types are required. The closure body must be evaluatable in `const`. |
//! | `setter(strip_option)`         | On an [`Option<T>`] field, change the setter to accept `T` and wrap it in [`Some`] itself. Equivalent to `setter(transform = \|value: T\| Some(value))`. |
//! | `skip`                         | Must be combined with `default`. Hides the field from the builder's public API by omitting its generic parameter and setter, instead forcing the default value. The unchecked builder retains a setter with the field's visibility. |
//!
//! # Attributes Example
//!
//! ```
//! use const_builder::ConstBuilder;
//!
//! #[derive(ConstBuilder)]
//! // change the builder from pub (same as Person) to crate-internal
//! // also override the name of the builder to `CreatePerson`
//! #[builder(vis = "pub(crate)", rename = "CreatePerson")]
//! // change the unchecked builder from priv also to crate-internal
//! #[builder(unchecked(vis = "pub(crate)"))]
//! # #[derive(Debug, PartialEq)]
//! pub struct Person<'a> {
//!     // required field with public setter
//!     name: &'a str,
//!     // optional field with public setter
//!     #[builder(default = 0)]
//!     age: u32,
//!     // skipped field omitted from builder interface
//!     #[builder(default = 1, skip)]
//!     version: u32,
//! }
//!
//! # assert_eq!(
//! #     const {
//! #         Person::builder()
//! #             .name("smith")
//! #             .build()
//! #     },
//! #     Person {
//! #         name: "smith",
//! #         age: 0,
//! #         version: 1,
//! #     }
//! # );
//! ```
//!
//! [nomicon section on unchecked uninitialized memory]: https://doc.rust-lang.org/nomicon/unchecked-uninit.html

#![forbid(unsafe_code)]
#![warn(clippy::doc_markdown)]

use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

mod const_builder_impl;
mod model;
mod util;

/// Generates the builder types for the attributed struct.
///
/// See the crate-level documentation for more details.
#[proc_macro_derive(ConstBuilder, attributes(builder))]
pub fn derive_const_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    const_builder_impl::entry_point(input).into()
}

/// Not public API. This is an internal helper for compile-fail tests.
///
/// This fully discards the input token stream, allowing replacing the tested
/// struct with an entirely different definition to test that safety-relevant
/// mismatches lead to compilation errors.
///
/// Related rust issue: <https://github.com/rust-lang/rust/issues/148423>
#[doc(hidden)]
#[deprecated = "do not use, this is an internal test helper"]
#[proc_macro_attribute]
pub fn __discard_input_token_stream(_args: TokenStream, _input: TokenStream) -> TokenStream {
    TokenStream::new()
}

/// I considered UI tests but since we still emit the code on almost all errors,
/// there are a bunch of rustc diagnostics mixed in (when the compile error
/// isn't literally just a rustc error). These aren't stable, so those tests
/// would just break between language versions -- or even just stable and
/// nightly.
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct TupleStruct(u32, u64);
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct UnitStruct;
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// enum Enum {
///     B { a: u32, b: u64 },
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// union Union {
///     a: u32,
///     b: u64,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// #[builder(default)]
/// struct InvalidDefault {
///     a: u32,
///     #[builder(default = 0)]
///     b: u32,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct DefaultNoValue {
///     #[builder(default)]
///     a: u32,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct WrongUnsizedTailPosition {
///     #[builder(unsized_tail)]
///     a: u32,
///     b: u32,
/// }
/// ```
///
/// ```compile_fail
/// // on stable, the macro code will not compile due to `UnsizedField: Sized`
/// // bounds. however on nightly with `trivial_bounds`, the actual output of
/// // the macro will compile, but the bounds will still prevent instantiating
/// // or otherwise using the builder.
/// // see also: https://github.com/rust-lang/rust/issues/48214
/// #[derive(const_builder::ConstBuilder)]
/// struct UnsizedField {
///     a: [u32],
/// }
///
/// // ensure instantiating fails anyways
/// _ = UnsizedFieldBuilder::new();
/// ```
///
/// ```compile_fail
/// // this test actually fails for two reasons:
/// // - rust disallowing possible-Drop unsized tails in packed structs
/// // - static assert when a packed struct's `unsized_tail` field `needs_drop`
/// #[derive(const_builder::ConstBuilder)]
/// #[repr(Rust, packed)]
/// struct PackedUnsizedDropTail<T: ?Sized> {
///     #[builder(unsized_tail)]
///     a: T,
/// }
///
/// // ensure a variant with a `needs_drop` tail is instantiated
/// _ = PackedUnsizedDropTail::<String>::builder();
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNoClosure {
///     #[builder(setter(transform))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNotAClosure1 {
///     #[builder(setter(transform = r#""hello""#))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNotAClosure2 {
///     #[builder(setter(transform = 42))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNotAClosure3 {
///     #[builder(setter(transform = Some(42)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNoType {
///     #[builder(setter(transform = |i| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNoTypePartial1 {
///     #[builder(setter(transform = |a: u32, b| a + b))]
///     value: u32,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastNoTypePartial2 {
///     #[builder(setter(transform = |a, b: u32| a + b))]
///     value: u32,
/// }
/// ```
///
/// ```compile_fail
/// // `works.rs` contains a similar case that compiles
/// #[derive(ConstBuilder)]
/// struct SetterCastPatNoType {
///     #[builder(setter(transform = |Wrap(v)| v))]
///     value: u32,
/// }
///
/// struct Wrap<T>(T);
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastAttrs {
///     #[builder(setter(transform = (#[inline] |i: u32| Some(i))))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastWrongLifetimes {
///     #[builder(setter(transform = for<'b> |i: &'a u32| Some(*i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastConst {
///     #[builder(setter(transform = const |i: u32| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastStatic {
///     #[builder(setter(transform = static |i: u32| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastAsync {
///     #[builder(setter(transform = async |i: u32| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastMove {
///     #[builder(setter(transform = move |i: u32| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastReturnType {
///     #[builder(setter(transform = |i: u32| -> Option<u32> { Some(i) }))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastAndStrip {
///     #[builder(setter(strip_option, transform = |i: u32| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastWrongType {
///     #[builder(setter(transform = |v: u32| v))]
///     value: i32,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterUnknown {
///     #[builder(setter(strip_result))]
///     value: Result<u32, u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterStripOptionNotOption {
///     #[builder(setter(strip_option))]
///     value: Result<(), ()>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterStripOptionDefault {
///     #[builder(default, setter(strip_option))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// use core::marker::PhantomData;
///
/// #[derive(const_builder::ConstBuilder)]
/// struct InvalidUse<T = u32> {
///     #[builder(default = PhantomData)]
///     marker: PhantomData<T>,
/// }
///
/// // fails because it can't infer the generic type
/// let value = InvalidUse::builder().build();
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct Incomplete1 {
///     unset: bool,
/// }
///
/// let value = Incomplete1::builder().build();
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct Incomplete2 {
///     #[builder(default = false)]
///     defaulted: bool,
///     unset: bool,
/// }
///
/// let value = Incomplete2::builder().build();
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct Incomplete3 {
///     set: bool,
///     unset: bool,
/// }
///
/// let value = Incomplete3::builder().set(true).build();
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct DuplicateSet {
///     field: bool,
/// }
///
/// DuplicateSet::builder().field(true).field(true);
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SkipNoDefault {
///     #[builder(skip)]
///     field: bool,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SkipSetter {
///     #[builder(skip, default = None, setter())]
///     field: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SkipVis {
///     #[builder(skip, default = None, vis = "pub")]
///     field: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SkipRename {
///     #[builder(skip, default = None, rename_generic = "_F")]
///     field: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SkipNoSetter {
///     #[builder(skip, default = None)]
///     field: Option<u32>,
/// }
///
/// // no setter
/// _ = SkipNoSetter::builder().field(Some(0));
/// ```
///
/// ```compile_fail
/// mod inner {
///     #[derive(const_builder::ConstBuilder)]
///     #[builder(unchecked(vis = "pub"))]
///     pub struct SkipPrivSetter {
///         #[builder(skip, default = None)]
///         field: Option<u32>,
///     }
/// }
///
/// // setter private
/// _ = inner::SkipPrivSetterUncheckedBuilder::new().field(Some(0));
/// ```
///
/// ```compile_fail
/// // !! safety-relevant !!
/// // a field not observed by the macro would allow
/// // safe code to later read uninitialized memory.
/// #[derive(const_builder::ConstBuilder)]
/// #[const_builder::__discard_input_token_stream]
/// struct AddsField {}
/// struct AddsField {
///     x: u32,
/// }
/// ```
///
/// ```compile_fail
/// // this just fails because the builder tries to access
/// // an unknown field.
/// #[derive(const_builder::ConstBuilder)]
/// #[const_builder::__discard_input_token_stream]
/// struct RemovesField {
///     x: u32,
/// }
/// struct RemovesField {}
/// ```
///
/// ```compile_fail
/// // !! safety-relevant !!
/// // unaligned access requires a different emit because
/// // writing to unaligned pointers is UB by default.
/// // the inverse of this is fine, if not optimal.
/// #[derive(const_builder::ConstBuilder)]
/// #[const_builder::__discard_input_token_stream]
/// struct ActuallyPacked {
///     x: u32,
/// }
/// #[repr(C, packed)]
/// struct ActuallyPacked {
///     x: u32,
/// }
/// ```
fn _compile_fail_test() {}
