//! Additional tests intended for MIRI's UB checks that won't do a lot in other
//! environments.
//!
//! MIRI should run the other tests as well. These ones are just for cases where
//! we are primarily concerned about soundness of the generated code (i.e. not
//! emitting aligned reads & writes for unaligned/packed fields).
//!
//! These are enabled unconditionally because there isn't a real reason not to
//! run them anyways.

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

use std::ops::Range;

use const_builder::ConstBuilder;

#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(align(512))]
struct HugeAlign<T>(T);

#[derive(Debug, PartialEq, ConstBuilder)]
#[cfg_attr(any(), repr(Rust, packed))]
struct UnconditionalNotPacked<T: ?Sized> {
    field: T,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[cfg_attr(all(), repr(Rust, packed))]
struct UnconditionalPacked<T> {
    field: T,
}

#[derive(Debug, PartialEq, ConstBuilder)]
struct UnsafeCopy {
    bytes: Box<[u8]>,
    ranges: Box<[Range<usize>]>,
}

#[test]
fn unconditional_not_packed() {
    let not_packed = UnconditionalNotPacked::builder().field(HugeAlign(16));
    assert_eq!(align_of_val(&not_packed), 512);

    let not_packed = not_packed.build();
    assert_eq!(
        not_packed,
        UnconditionalNotPacked {
            field: HugeAlign(16)
        }
    );
}

#[test]
fn unconditional_packed() {
    let packed = UnconditionalPacked::builder().field(HugeAlign(16));
    assert_eq!(align_of_val(&packed), 1);

    let packed = packed.build();
    assert_eq!(
        packed,
        UnconditionalPacked {
            field: HugeAlign(16)
        }
    );
}

#[test]
fn unsafe_copy() {
    // this test essentially just ensures that multiple copies of an unchecked
    // builder are allowed to coexist and don't cause uniqueness errors as long as
    // only one of them is actually built at the end. copying the normal builder
    // like this isn't allowed because it has a `Drop` impl that would access the
    // fields.
    let bytes = include_bytes!("miri.rs");

    let src = UnsafeCopyUncheckedBuilder::new()
        .bytes(Box::new(*bytes))
        .ranges(Box::new([0..1, 1..bytes.len()]));

    // SAFETY: unchecked builder must be safe to copy no matter what
    // however only one final version of it must be `build`/`assume_init`-ed.
    let copy1 = unsafe { (&raw const src).read() };
    let copy2 = unsafe { (&raw const copy1).read() };
    let copy3 = unsafe { (&raw const src).read() };

    // SAFETY: all fields initialized
    let result = unsafe { copy2.build() };
    drop(src);
    drop(copy1);
    drop(copy3);

    assert_eq!(
        result,
        UnsafeCopy {
            bytes: Box::new(*bytes),
            ranges: Box::new([0..1, 1..bytes.len()])
        }
    );
}
