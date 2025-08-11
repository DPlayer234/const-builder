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
//! [`?Sized`](Sized), the field needs to be attributed with
//! `#[builder(unsized_tail)]` to replace the field drop code with an assert
//! that the field cannot be dropped. Rust functionally currently requires this
//! combination of packed and unsized tails to be
//! [`ManuallyDrop`](std::mem::ManuallyDrop) or a wrapper around it.
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
//! # Unchecked Builder
//!
//! There is also an `*UncheckedBuilder` without safety checks, which is
//! private by default. This struct is used to simplify the implementation of
//! the checked builder and it is exposed for users that want additional
//! control.
//!
//! This builder works broadly in the same way as the checked builder, however:
//!
//! - initialized fields aren't tracked,
//! - setting fields that were already set will forget the old value,
//! - calling `build` is unsafe due to the lack of tracking, and
//! - dropping it will forget all field values that were already set.
//!
//! Furthermore, its API isn't stable across modifications of the source struct.
//! In particular, adding or removing a field to the struct is a breaking change
//! to the API of the unchecked builder, so it should not be exposed in stable
//! public interfaces.
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
//!    pub const fn name(mut self, value: &'a str) -> PersonUncheckedBuilder<'a>;
//!    pub const fn age(mut self, value: u32) -> PersonUncheckedBuilder<'a>;
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
//! | Attribute           | Meaning |
//! |:------------------- |:------- |
//! | `default`           | Generate a const-compatible `*::default()` function and [`Default`] derive. Requires every field to have a default value. |
//! | `vis`               | Change the visibility of the builder type. May be an empty string for private. Default is the same as the struct. |
//! | `rename`            | Renames the builder type. Defaults to `"<Type>Builder"`. |
//! | `rename_fn`         | Renames the associated function that creates the builder. Defaults to `builder`. Set to `false` to disable. |
//! | `unchecked(vis)`    | Change the visibility of the unchecked builder type. Default is private. |
//! | `unchecked(rename)` | Renames the unchecked builder type. Defaults to `"<Type>UncheckedBuilder"`. |
//!
//! # Field Attributes
//!
//! These attributes can be specified within `#[builder(...)]` on the struct's
//! fields.
//!
//! | Attribute              | Meaning |
//! |:---------------------- |:------- |
//! | `vis`                  | Change the visibility of the builder's field setter. May be an empty string for private. Default is `pub`. |
//! | `default`              | Make the field optional by providing a default value. |
//! | `rename`               | Renames the setters for this field. Defaults to the field name. |
//! | `rename_generic`       | Renames the name of the associated const generic. Defaults to `_{field:upper}`. |
//! | `leak_on_drop`         | Instead of dropping the field when dropping the builder, do nothing. |
//! | `unsized_tail`         | In a packed struct, marks the last field as potentially being unsized, replacing the drop code with an assert. No effect if the struct isn't packed. |
//! | `setter(transform)`    | Accepts closure syntax. The setter is changed to accept its inputs and set the corresponding value to its output. |
//! | `setter(strip_option)` | On an [`Option<T>`] field, change the setter to accept `T` and wrap it in [`Some`] itself. Equivalent to `setter(transform = \|value: T\| Some(value))`. |
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
//!     // optional field with private setter
//!     #[builder(default = 1, vis = "" /* priv */)]
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
    const_builder_impl::entry_point(input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

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
/// enum Enum { B { a: u32, b: u64 } };
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// union Union { a: u32, b: u64 };
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
/// struct WrongUnsizedTailPosition {
///     #[builder(unsized_tail)]
///     a: u32,
///     b: u32,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct UnsizedField {
///     a: [u32],
/// }
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
/// struct SetterCastWrong1 {
///     #[builder(setter(transform))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastWrong2 {
///     #[builder(setter(transform = |i| Some(i)))]
///     value: Option<u32>,
/// }
/// ```
///
/// ```compile_fail
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastWrong3 {
///     #[builder(setter(transform = move |i: u32| Some(i)))]
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
/// #[derive(const_builder::ConstBuilder)]
/// struct SetterCastWrongType {
///     #[builder(setter(transform = |v: u32| v))]
///     value: i32,
/// }
/// ```
fn _compile_fail_test() {}
