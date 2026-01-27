use std::borrow::Cow;

use darling::util::{Flag, Override};
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use proc_macro2::TokenStream;
use syn::{Attribute, Expr, Ident, PatType, Type, Visibility};

use crate::util::BoolOr;

#[derive(Default, Debug, FromDeriveInput)]
#[darling(attributes(builder))]
pub struct BuilderAttrs {
    // darling recognizes `vis` as matching the struct visibility which i don't want
    #[darling(rename = "vis")]
    pub m_vis: Option<Visibility>,
    pub rename: Option<Ident>,
    pub rename_fn: Option<BoolOr<Ident>>,
    pub default: Flag,
}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder))]
pub struct FieldAttrs {
    pub rename: Option<Ident>,
    pub rename_generic: Option<Ident>,
    pub default: Option<Override<Box<Expr>>>,
    pub non_const: Flag,
    pub vis: Option<Visibility>,
    #[darling(default)]
    pub setter: FieldSetterRaw,
}

pub struct FieldInfo<'a> {
    pub ident: &'a Ident,
    pub name: Ident,
    pub gen_name: Ident,
    pub ty: &'a Type,
    pub default: Option<Override<Box<Expr>>>,
    pub vis: Visibility,
    pub doc: Vec<Cow<'a, Attribute>>,
    pub deprecated: Option<&'a Attribute>,
    pub non_const: bool,
    pub setter: FieldSetter,
}

pub trait FieldInfoSliceExt {
    fn idents(&self) -> impl Iterator<Item = &Ident>;
    fn gen_names(&self) -> impl Iterator<Item = &Ident>;
}

impl FieldInfoSliceExt for [FieldInfo<'_>] {
    fn idents(&self) -> impl Iterator<Item = &Ident> {
        self.iter().map(|t| t.ident)
    }
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
    pub is_const: bool,
    pub lifetimes: Option<TokenStream>,
    pub inputs: Vec<PatType>,
    pub body: Box<Expr>,
}
