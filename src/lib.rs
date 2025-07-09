use proc_macro::TokenStream;
use syn::{DeriveInput, parse_macro_input};

mod const_builder_impl;

/// Derive macro that provides a `*Builder` type that can be used to
/// field-by-field initialize a value, even in a const context.
///
/// You may specify default values for fields by attributing them with
/// `#[const_context = "expr"]`.
///
/// The attributed type will gain an associated `builder` method, which can then
/// be chained with function calls until all required fields are set, at which
/// point you can call `build` to get the final value.
///
/// Compile-time checks prevent setting the same field twice or calling `build`
/// before all required fields are set.
///
/// # Examples
///
/// ```
/// # use std::borrow::Cow;
/// use const_builder::ConstBuilder;
///
/// #[derive(ConstBuilder)]
/// # #[derive(Debug, PartialEq)]
/// struct Person<'a> {
///     pub first_name: &'a str,
///     pub last_name: &'a str,
///     #[const_default = "0"]
///     pub age: u32,
///     #[const_default = "None"]
///     pub awake_since: Option<u32>,
/// }
///
/// let steve = const {
///     Person::builder()
///         .first_name("steve")
///         .last_name("smith")
///         .age(32)
///         .build()
/// };
/// # assert_eq!(
/// #     steve,
/// #     Person {
/// #         first_name: "steve",
/// #         last_name: "smith",
/// #         age: 32,
/// #         awake_since: None,
/// #     }
/// # );
/// ```
#[proc_macro_derive(ConstBuilder, attributes(const_default))]
pub fn derive_const_builder(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    const_builder_impl::entry_point(input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}
