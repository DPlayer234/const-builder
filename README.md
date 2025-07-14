Provides a `ConstBuilder` derive macro that generates a `*Builder` type
that can be used to field-by-field initialize a value, even in a const
context.

The attributed type will gain an associated `builder` method, which can then
be chained with function calls until all required fields are set, at which
point you can call `build` to get the final value.

Compile-time checks prevent setting the same field twice or calling `build`
before all required fields are set.

By default, the `*Builder` type and `*::builder` method have the same
visibility as the type, and every field setter is `pub`, regardless of field
visibility.

There is also an `*UncheckedBuilder` without safety checks, which is
private by default. You can convert between them with
`*Builder::into_unchecked` and `*UncheckedBuilder::assert_init`.

# Struct Requirements

All struct fields must be `Sized`. Fields using generic parameters may be
`?Sized` for some parameters, as long as the actual instantiation
of the builder only has `Sized` fields.

However, when the struct is `#[repr(packed)]`, every field must be `Sized`
for any combination of generics. This is due to the interaction between
field alignment and requirements for `Drop` code.

`enum` and `union` types are unsupported.

# Examples

```rust
use const_builder::ConstBuilder;

#[derive(ConstBuilder)]
struct Person<'a> {
    pub first_name: &'a str,
    pub last_name: &'a str,
    // note: see section below on caveats with `default`
    #[builder(default = "0")]
    pub age: u32,
    #[builder(default = "None")]
    pub awake_since: Option<u32>,
}

let steve = const {
    Person::builder()
        .first_name("steve")
        .last_name("smith")
        .age(32)
        .build()
};
```

# Default Values

Fields can be attributed with `#[builder(default = "None")]` or similar to
be made optional by providing a default value.

This default value will _not_ be dropped if it is overridden or the builder
is dropped. Consequently, the value should be something that does not need
to be dropped, such as a primitive, `None`, `String::new()`,
`Vec::new()`, `Cow::Borrowed`, or similar.

# Struct Attributes

These attributes can be specified within `#[builder(...)]` on the struct
level.

| Attribute           | Meaning |
|:------------------- |:------- |
| `default`           | Generate a const-compatible `*::default()` function and `Default` derive. Requires every field to have a default value. |
| `vis`               | Change the visibility of the builder type. May be an empty string for private. Default is the same as the struct. |
| `rename`            | Renames the builder type. Defaults to `"<Type>Builder"`. |
| `unchecked(vis)`    | Change the visibility of the unchecked builder type. Default is private. |
| `unchecked(rename)` | Renames the unchecked builder type. Defaults to `"<Type>UncheckedBuilder"`. |

# Field Attributes

These attributes can be specified within `#[builder(...)]` the struct's
fields.

| Attribute        | Meaning |
|:---------------- |:------- |
| `vis`            | Change the visibility of the builder's field setter. May be an empty string for private. Default is `pub`. |
| `default`        | Make the field optional by providing a default value. |
| `rename`         | Renames the setters for this field. Defaults to the field name. |
| `rename_generic` | Renames the name of the associated const generic. Defaults to `_{field:upper}`. |

# Attributes Example

```rust
use const_builder::ConstBuilder;

#[derive(ConstBuilder)]
// change the builder from pub (same as Person) to crate-internal
#[builder(vis = "pub(crate)")]
// change the unchecked builder from priv also to crate-internal
#[builder(unchecked(vis = "pub(crate)"))]
pub struct Person<'a> {
    // required field with public setter
    name: &'a str,
    // optional field with public setter
    #[builder(default = "0")]
    age: u32,
    // optional field with private setter
    #[builder(default = "1", vis = "" /* priv */)]
    version: u32,
}
```