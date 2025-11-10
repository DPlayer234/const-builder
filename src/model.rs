use std::borrow::Cow;

use darling::util::Flag;
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use proc_macro2::TokenStream;
use syn::{Attribute, Expr, Ident, PatType, Type, Visibility};

use crate::util::{AnyItem, BoolOr};

#[derive(Default, Debug, FromDeriveInput)]
#[darling(attributes(builder))]
pub struct BuilderAttrs {
    // darling recognizes `vis` as matching the struct visibility which i don't want
    #[darling(rename = "vis")]
    pub m_vis: Option<Visibility>,
    pub rename: Option<Ident>,
    pub rename_fn: Option<BoolOr<Ident>>,
    #[darling(default)]
    pub unchecked: BuilderUncheckedAttrs,
    pub default: Flag,
}

#[derive(Default, Debug, FromDeriveInput)]
#[darling(attributes(repr), allow_unknown_fields)]
pub struct ReprAttrs {
    // only check for presence of `packed` or `packed(N)`, no need to know the content.
    // there are basically no sane situations in which the value here matters to this code.
    pub packed: Option<AnyItem>,
    // the remaining `repr` fields don't matter either, since, while they impact layout order, for
    // soundness--in regards to this crate--only field alignment matters and `packed` is the only
    // option that may reduce field alignment.
}

#[derive(Default, Debug, FromMeta)]
pub struct BuilderUncheckedAttrs {
    pub vis: Option<Visibility>,
    pub rename: Option<Ident>,
}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder))]
pub struct FieldAttrs {
    pub rename: Option<Ident>,
    pub rename_generic: Option<Ident>,
    pub default: Option<Box<Expr>>,
    pub vis: Option<Visibility>,
    pub leak_on_drop: Flag,
    pub unsized_tail: Flag,
    #[darling(default)]
    pub setter: FieldSetterRaw,
}

pub struct FieldInfo<'a> {
    pub ident: &'a Ident,
    pub name: Ident,
    pub gen_name: Ident,
    pub drop_flag: Ident,
    pub ty: &'a Type,
    pub default: Option<Box<Expr>>,
    pub vis: Visibility,
    pub doc: Vec<Cow<'a, Attribute>>,
    pub deprecated: Option<&'a Attribute>,
    pub leak_on_drop: bool,
    pub unsized_tail: bool,
    pub setter: FieldSetter,
}

pub trait FieldInfoSliceExt {
    fn gen_names(&self) -> impl Iterator<Item = &Ident>;
}

impl FieldInfoSliceExt for [FieldInfo<'_>] {
    fn gen_names(&self) -> impl Iterator<Item = &Ident> {
        self.iter().map(|t| &t.gen_name)
    }
}

#[derive(Default, Debug, FromMeta)]
pub struct FieldSetterRaw {
    pub strip_option: Flag,
    pub transform: Option<Expr>,
}

#[derive(Default, Debug)]
pub enum FieldSetter {
    #[default]
    Default,
    StripOption,
    Transform(FieldTransform),
}

#[derive(Debug)]
pub struct FieldTransform {
    pub lifetimes: Option<TokenStream>,
    pub inputs: Vec<PatType>,
    pub body: Box<Expr>,
}
