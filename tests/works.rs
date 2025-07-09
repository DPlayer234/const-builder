use std::borrow::Cow;

use const_builder::ConstBuilder;

#[derive(Debug, PartialEq, ConstBuilder)]
struct Person<'a, T: PartialEq, const VERSION: usize> {
    pub first_name: Cow<'a, str>,
    pub last_name: Cow<'a, str>,
    #[const_default = "0"]
    pub age: u32,
    #[const_default = "None"]
    pub awake_since: Option<u32>,
    pub unique: T,
}

#[test]
fn _person() {
    let person = const {
        Person::<'_, _, 2>::builder()
            .first_name(Cow::Borrowed("steve"))
            .last_name(Cow::Borrowed("smith"))
            .age(32)
            .unique(())
            .build()
    };

    assert_eq!(
        person,
        Person {
            first_name: Cow::Borrowed("steve"),
            last_name: Cow::Borrowed("smith"),
            age: 32,
            awake_since: None,
            unique: (),
        }
    );
}
