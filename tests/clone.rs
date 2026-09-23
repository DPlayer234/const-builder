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

use const_builder::ConstBuilder;

#[derive(Debug, Clone, PartialEq)]
struct CloneOnly<T>(T);

#[derive(Debug, PartialEq)]
struct NotClone<T>(T);

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone = "like_derive")]
struct LikeDerive {
    a: u32,
    b: CloneOnly<u32>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct PackedLikeDerive {
    a: u32,
    b: u32,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
struct LikeDeriveGenerics<A, B> {
    a: A,
    b: CloneOnly<B>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct PackedLikeDeriveGenerics<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone = "precise")]
struct Precise<A, B> {
    a: A,
    b: CloneOnly<B>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone = "precise")]
#[repr(Rust, packed)]
struct PackedPrecise<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(clone)]
struct SkipLikeDerive {
    a: u32,
    b: CloneOnly<u32>,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(clone = "precise")]
struct SkipPrecise {
    a: u32,
    b: CloneOnly<u32>,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct SkipPackedLikeDerive {
    a: u32,
    b: u32,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(ConstBuilder)]
#[builder(clone = "precise")]
#[repr(Rust, packed)]
struct SkipPackedPrecise {
    a: u32,
    b: u32,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[test]
fn like_derive() {
    fn check(value: LikeDerive, a: u32, b: u32) {
        assert_eq!({ value }, LikeDerive { a, b: CloneOnly(b) });
    }

    let empty = LikeDerive::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn packed_like_derive() {
    fn check(value: PackedLikeDerive, a: u32, b: u32) {
        assert_eq!({ value }, PackedLikeDerive { a, b });
    }

    let empty = PackedLikeDerive::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(2);

    check(empty.a(3).b(4).build(), 3, 4);
    check(only_a.clone().b(5).build(), 1, 5);
    check(only_a.b(6).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn like_derive_generics() {
    fn check(value: LikeDeriveGenerics<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, LikeDeriveGenerics { a, b: CloneOnly(b) });
    }

    let empty = LikeDeriveGenerics::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn packed_like_derive_generics() {
    fn check(value: PackedLikeDeriveGenerics<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, PackedLikeDeriveGenerics { a, b });
    }

    let empty = PackedLikeDeriveGenerics::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(2);

    check(empty.a(3).b(4).build(), 3, 4);
    check(only_a.clone().b(5).build(), 1, 5);
    check(only_a.b(6).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn precise_simple() {
    fn check(value: Precise<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, Precise { a, b: CloneOnly(b) });
    }

    let empty = Precise::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
#[expect(clippy::redundant_clone)]
fn precise_strict() {
    _ = <Precise<NotClone<()>, NotClone<()>>>::builder().clone();
    _ = <Precise<CloneOnly<u32>, NotClone<()>>>::builder()
        .a(CloneOnly(0))
        .clone();
    _ = <Precise<NotClone<()>, u32>>::builder()
        .b(CloneOnly(0))
        .clone();
}

#[test]
fn packed_precise_simple() {
    fn check(value: PackedPrecise<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, PackedPrecise { a, b });
    }

    let empty = PackedPrecise::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(2);

    check(empty.a(3).b(4).build(), 3, 4);
    check(only_a.clone().b(5).build(), 1, 5);
    check(only_a.b(6).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
#[expect(clippy::redundant_clone)]
fn packed_precise_strict() {
    _ = <PackedPrecise<NotClone<()>, NotClone<()>>>::builder().clone();
    _ = <PackedPrecise<u32, NotClone<()>>>::builder().a(0).clone();
    _ = <PackedPrecise<NotClone<()>, u32>>::builder().b(0).clone();
}

#[test]
#[expect(clippy::redundant_clone)]
fn skipped() {
    assert_eq!(
        SkipLikeDerive::builder()
            .a(1)
            .b(CloneOnly(2))
            .clone()
            .build(),
        SkipLikeDerive {
            a: 1,
            b: CloneOnly(2),
            skip: NotClone(0),
        }
    );
    assert_eq!(
        SkipPrecise::builder().a(1).b(CloneOnly(2)).clone().build(),
        SkipPrecise {
            a: 1,
            b: CloneOnly(2),
            skip: NotClone(0),
        }
    );
    _ = SkipPackedLikeDerive::builder().a(1).b(2).clone().build();
    _ = SkipPackedPrecise::builder().a(1).b(2).clone().build();
}
