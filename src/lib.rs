//! Provides a [`ConstBuilder`] derive macro that generates a `*Builder` type
//! that can be used to field-by-field initialize a value, even in a const
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
//! There is also an `*UncheckedBuilder` without safety checks, which is
//! private by default. You can convert between them with
//! `*Builder::into_unchecked` and `*UncheckedBuilder::assert_init`.
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
//! # Examples
//!
//! ```
//! use const_builder::ConstBuilder;
//!
//! #[derive(ConstBuilder)]
//! # #[derive(Debug, PartialEq)]
//! struct Person<'a> {
//!     pub first_name: &'a str,
//!     pub last_name: &'a str,
//!     // note: see section below on caveats with `default`
//!     #[builder(default = "0")]
//!     pub age: u32,
//!     #[builder(default = "None")]
//!     pub awake_since: Option<u32>,
//! }
//!
//! let steve = const {
//!     Person::builder()
//!         .first_name("steve")
//!         .last_name("smith")
//!         .age(32)
//!         .build()
//! };
//! # assert_eq!(
//! #     steve,
//! #     Person {
//! #         first_name: "steve",
//! #         last_name: "smith",
//! #         age: 32,
//! #         awake_since: None,
//! #     }
//! # );
//! ```
//!
//! # Default Values
//!
//! Fields can be attributed with `#[builder(default = "None")]` or similar to
//! be made optional by providing a default value.
//!
//! This default value will _not_ be dropped if it is overridden or the builder
//! is dropped. Consequently, the value should be something that does not need
//! to be dropped, such as a primitive, [`None`], [`String::new()`],
//! [`Vec::new()`], [`Cow::Borrowed`](std::borrow::Cow), or similar.
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
//! | Attribute        | Meaning |
//! |:---------------- |:------- |
//! | `vis`            | Change the visibility of the builder's field setter. May be an empty string for private. Default is `pub`. |
//! | `default`        | Make the field optional by providing a default value. |
//! | `rename`         | Renames the setters for this field. Defaults to the field name. |
//! | `rename_generic` | Renames the name of the associated const generic. Defaults to `_{field:upper}`. |
//! | `leak_on_drop`   | Instead of dropping the field when dropping the builder, do nothing. |
//! | `unsized_tail`   | In a packed struct, marks the last field as potentially being unsized, replacing the drop code with an assert. No effect if the struct isn't packed. |
//!
//! # Attributes Example
//!
//! ```
//! use const_builder::ConstBuilder;
//!
//! #[derive(ConstBuilder)]
//! // change the builder from pub (same as Person) to crate-internal
//! #[builder(vis = "pub(crate)")]
//! // change the unchecked builder from priv also to crate-internal
//! #[builder(unchecked(vis = "pub(crate)"))]
//! # #[derive(Debug, PartialEq)]
//! pub struct Person<'a> {
//!     // required field with public setter
//!     name: &'a str,
//!     // optional field with public setter
//!     #[builder(default = "0")]
//!     age: u32,
//!     // optional field with private setter
//!     #[builder(default = "1", vis = "" /* priv */)]
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
