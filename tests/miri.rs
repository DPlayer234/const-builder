//! Additional tests intended for MIRI's UB checks that won't do a lot in other
//! environments.
//!
//! MIRI should run the other tests as well. These ones are just for cases where
//! we are primarily concerned about soundness of the generated code (i.e. not
//! emitting aligned reads & writes for unaligned/packed fields).
//!
//! These are enabled unconditionally because there isn't a real reason not to
//! run them anyways.

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

#[test]
fn unconditional_not_packed() {
    let not_packed = UnconditionalNotPacked::builder()
        .field(HugeAlign(16))
        .build();

    assert_eq!(align_of_val(&not_packed), 512);
    assert_eq!(
        not_packed,
        UnconditionalNotPacked {
            field: HugeAlign(16)
        }
    );
}

#[test]
fn unconditional_packed() {
    let packed = UnconditionalPacked::builder().field(HugeAlign(16)).build();

    assert_eq!(align_of_val(&packed), 1);
    assert_eq!(
        packed,
        UnconditionalPacked {
            field: HugeAlign(16)
        }
    );
}
