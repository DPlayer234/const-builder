#![no_std]

use const_builder::ConstBuilder;

#[derive(Debug, PartialEq, ConstBuilder)]
pub struct Person<'a> {
    pub name: &'a str,
    #[builder(default = 0)]
    pub age: u32,
}

const STEVE: Person<'_> = const {
    Person::builder()
        .name("steve smith")
        // keep the default for age
        .build()
};

#[test]
fn no_std_person() {
    assert_eq!(
        STEVE,
        Person {
            name: "steve smith",
            age: 0
        }
    );
}
