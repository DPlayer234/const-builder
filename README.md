[![Documentation](https://img.shields.io/badge/api-docs-blue.svg)](https://docs.rs/const-builder/latest/const_builder/)
[![Latest Version](https://img.shields.io/crates/v/const-builder.svg)](https://crates.io/crates/const-builder)

# Const-Compatible Builders

Creates const-compatible builders for your structs:

```rust
use const_builder::ConstBuilder;

#[derive(ConstBuilder)]
pub struct Person<'a> {
    // fields are required by default
    pub name: &'a str,
    // optional fields can be specified with `default`
    #[builder(default = 0)]
    pub age: u32,
}

let steve = const {
    Person::builder()
        .name("steve smith")
        // keep the default for age
        .build()
};
```

You can initialize fields in any order, however compile-time checks prevent calling `build` before all required field have been set.

By default, the builder type and `*::builder` function have the same visibility as the declared struct.

The generated code is supported in `#![no_std]` crates.

This crate was inspired by [typed-builder](https://crates.io/crates/typed-builder) and created due to the need to be able to provide a stable way to create non-exhaustive structs across crates in `const` code.

# Features

- Easy to use builder pattern.
- Callable in `const` code.
- Compile-time check that all required fields have been set.
- Default values for fields via `#[builder(default = ...)]`.
- Customization for the names and visibility of the `*::builder` function, builder types, and field setters.

# Caveats

- The generated builder types use a tuple of the internal field types. You _can_ name them, but the builders aren't really meant to be passed around and are more so meant as a stable way to create structs across crates.
- The errors for missing fields on `build` and duplicate set fields aren't easy to understand and may refer to complex, obscure type bounds.
- `#[builder(default = ...)]` accepts any Rust expression, however when a string literal is provided, it is parsed again. Setting defaults for `&str` values therefore requires specifying them similar to `#[builder(default = r#""default value""#)]`.

# License

Licensed under the MIT license.
