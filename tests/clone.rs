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

#[derive(Debug, PartialEq)]
struct CloneIfCopy<T>(T);
impl<T: Copy> Clone for CloneIfCopy<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone(simple))]
struct Simple {
    a: u32,
    b: CloneOnly<u32>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct PackedSimple {
    a: u32,
    b: u32,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
struct SimpleGenerics<A, B> {
    a: A,
    b: CloneOnly<B>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct PackedSimpleGenerics<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone(precise))]
struct Precise<A, B> {
    a: A,
    b: CloneOnly<B>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone(precise))]
#[repr(Rust, packed)]
struct PackedPrecise<A, B> {
    a: A,
    b: B,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(clone)]
struct SkipSimple {
    a: u32,
    b: CloneOnly<u32>,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(clone(precise))]
struct SkipPrecise {
    a: u32,
    b: CloneOnly<u32>,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(ConstBuilder)]
#[builder(clone)]
#[repr(Rust, packed)]
struct SkipPackedSimple {
    a: u32,
    b: u32,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(ConstBuilder)]
#[builder(clone(precise))]
#[repr(Rust, packed)]
struct SkipPackedPrecise {
    a: u32,
    b: u32,
    #[builder(skip, default = NotClone(0))]
    skip: NotClone<u32>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
struct SimpleAll<'a, A, B, const N: usize> {
    a: A,
    b: CloneOnly<B>,
    #[builder(default = None)]
    c: Option<&'a u32>,
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone(precise))]
struct PreciseAll<'a, A, B, const N: usize> {
    a: A,
    b: CloneOnly<B>,
    #[builder(default = None)]
    c: Option<&'a u32>,
}

trait WithAssoc {
    type Assoc<A>;
}
impl<T> WithAssoc for T {
    type Assoc<A> = CloneIfCopy<T>;
}

#[derive(Debug, Clone, PartialEq, ConstBuilder)]
#[builder(clone)]
struct SimpleAssocNightmare<A: WithAssoc, B: WithAssoc> {
    value: CloneOnly<B::Assoc<A::Assoc<()>>>,
}

#[test]
fn simple() {
    fn check(value: Simple, a: u32, b: u32) {
        assert_eq!({ value }, Simple { a, b: CloneOnly(b) });
    }

    let empty = Simple::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn packed_simple() {
    fn check(value: PackedSimple, a: u32, b: u32) {
        assert_eq!({ value }, PackedSimple { a, b });
    }

    let empty = PackedSimple::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(2);

    check(empty.a(3).b(4).build(), 3, 4);
    check(only_a.clone().b(5).build(), 1, 5);
    check(only_a.b(6).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn simple_generics() {
    fn check(value: SimpleGenerics<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, SimpleGenerics { a, b: CloneOnly(b) });
    }

    let empty = SimpleGenerics::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn packed_simple_generics() {
    fn check(value: PackedSimpleGenerics<u32, u32>, a: u32, b: u32) {
        assert_eq!({ value }, PackedSimpleGenerics { a, b });
    }

    let empty = PackedSimpleGenerics::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(2);

    check(empty.a(3).b(4).build(), 3, 4);
    check(only_a.clone().b(5).build(), 1, 5);
    check(only_a.b(6).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn precise_basic() {
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
fn packed_precise_basic() {
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
        SkipSimple::builder().a(1).b(CloneOnly(2)).clone().build(),
        SkipSimple {
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
    _ = SkipPackedSimple::builder().a(1).b(2).clone().build();
    _ = SkipPackedPrecise::builder().a(1).b(2).clone().build();
}

#[test]
fn simple_all() {
    fn check(value: SimpleAll<'_, u32, u32, 0>, a: u32, b: u32) {
        assert_eq!(
            { value },
            SimpleAll {
                a,
                b: CloneOnly(b),
                c: None
            }
        );
    }

    let empty = SimpleAll::builder();

    let only_a = empty.clone().a(1);
    let only_b = empty.clone().b(CloneOnly(2));

    check(empty.a(3).b(CloneOnly(4)).build(), 3, 4);
    check(only_a.clone().b(CloneOnly(5)).build(), 1, 5);
    check(only_a.b(CloneOnly(6)).build(), 1, 6);
    check(only_b.clone().a(7).build(), 7, 2);
    check(only_b.a(8).build(), 8, 2);
}

#[test]
fn precise_all() {
    fn check(value: PreciseAll<'_, u32, u32, 0>, a: u32, b: u32) {
        assert_eq!(
            { value },
            PreciseAll {
                a,
                b: CloneOnly(b),
                c: None
            }
        );
    }

    let empty = PreciseAll::builder();

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
fn simple_assoc_nightmare() {
    let value = SimpleAssocNightmare::<(), ()>::builder()
        .clone()
        .value(CloneOnly(CloneIfCopy(())))
        .clone()
        .build();

    assert_eq!(
        value,
        SimpleAssocNightmare {
            value: CloneOnly(CloneIfCopy(()))
        }
    );
}
