use std::borrow::Cow;
use std::marker::PhantomData;

use const_builder::ConstBuilder;

//#[derive(Debug, PartialEq)]
#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(
    vis = "",
    rename = "CreatePerson",
    unchecked(vis = "pub(crate)", rename = "UncheckedCreatePerson")
)]
pub struct Person<'a, T: ?Sized + PartialEq, const VERSION: usize> {
    #[builder(rename = "set_first_name")]
    pub first_name: Cow<'a, str>,
    pub last_name: Cow<'a, str>,
    #[builder(default = "0")]
    pub age: u32,
    #[builder(default = "None")]
    pub awake_since: Option<u32>,
    #[builder(vis = "" /* priv */)]
    pub unique: T,
}

#[derive(Debug, PartialEq, ConstBuilder)]
#[builder(default)]
struct Defaultable {
    #[builder(default = "0")]
    key: u32,
    #[builder(default = "Some(0)")]
    value: Option<u32>,
    #[builder(default = "Cow::Borrowed(\"unlabelled\")")]
    label: Cow<'static, str>,
}

#[test]
fn person() {
    let person = const {
        Person::<'_, _, 2>::builder()
            .set_first_name(Cow::Borrowed("steve"))
            .last_name(Cow::Borrowed("smith"))
            .age(32)
            .unique(PhantomData::<()>)
            .build()
    };

    assert_eq!(
        person,
        Person {
            first_name: Cow::Borrowed("steve"),
            last_name: Cow::Borrowed("smith"),
            age: 32,
            awake_since: None,
            unique: PhantomData::<()>,
        }
    );
}

#[test]
fn defaultable() {
    let default = const { Defaultable::default() };
    assert_eq!(default, <Defaultable as Default>::default());
}
