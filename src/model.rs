#![expect(
    clippy::duplicated_attributes,
    reason = "darling is stricter in what it filters on than what rustc enables, \
              so we specify the raw-identifier version on the top-level"
)]

use std::borrow::Cow;

use darling::util::{Flag, Override};
use darling::{FromAttributes, FromMeta};
use syn::punctuated::Punctuated;
use syn::{Attribute, Expr, Ident, PatType, Token, Type, Visibility};

use crate::util::*;

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder, r#builder))]
pub struct EmptyAttrs {}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder, r#builder))]
pub struct BuilderAttrs {
    #[darling(rename = "pub", with = "visibility_meta")]
    pub r#pub: Option<Visibility>,
    pub rename: Option<Ident>,
    pub rename_fn: MaybeIdent,
    pub default: Flag,
    pub clone: Option<Override<CloneMode>>,
    #[darling(default)]
    pub unchecked: BuilderUncheckedAttrs,
}

#[derive(Default, Debug, FromMeta)]
pub struct BuilderUncheckedAttrs {
    #[darling(rename = "pub", with = "visibility_meta")]
    pub r#pub: Option<Visibility>,
    pub rename: Option<Ident>,
}

#[derive(Default, Clone, Copy, Debug, FromMeta)]
pub enum CloneMode {
    #[default]
    Simple,
    Precise,
}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder, r#builder))]
pub struct FieldAttrs {
    #[darling(rename = "pub", with = "visibility_meta")]
    pub r#pub: Option<Visibility>,
    #[darling(with = "option_box_expr_without_reparse")]
    pub default: Option<Box<Expr>>,
    pub rename: Option<Ident>,
    pub rename_generic: Option<Ident>,
    pub skip: Flag,
    pub setter: Option<FieldSetterRaw>,
    pub forget_on_drop: Flag,
    pub unsized_tail: Flag,
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
    pub forget_on_drop: bool,
    pub unsized_tail: bool,
    pub skip: bool,
    pub setter: FieldSetter<'a>,
}

pub trait FieldInfoSliceExt {
    /// Fields that are part of the public API.
    ///
    /// Any `!skip` field will at least show up as a generic parameter.
    fn pub_api(&self) -> impl Iterator<Item = &FieldInfo<'_>> + Clone;

    /// `gen_name` for pub API fields.
    fn gen_names(&self) -> impl Iterator<Item = &Ident> + Clone;
}

impl FieldInfoSliceExt for [FieldInfo<'_>] {
    fn pub_api(&self) -> impl Iterator<Item = &FieldInfo<'_>> + Clone {
        self.iter().filter(|t| !t.skip)
    }
    fn gen_names(&self) -> impl Iterator<Item = &Ident> + Clone {
        self.pub_api().map(|t| &t.gen_name)
    }
}

#[derive(Default, Debug, FromMeta)]
pub struct FieldSetterRaw {
    pub strip_option: Flag,
    #[darling(with = "option_box_expr_without_reparse")]
    pub transform: Option<Box<Expr>>,
}

#[derive(Default, Debug)]
pub enum FieldSetter<'a> {
    #[default]
    Default,
    StripOption(&'a Type),
    // boxed to reduce size of `FieldInfo`
    Transform(Box<FieldTransform>),
}

#[derive(Debug)]
pub struct FieldTransform {
    pub lifetimes: Option<AngleBracketedGenerics>,
    pub inputs: Punctuated<PatType, Token![,]>,
    pub body: Box<Expr>,
}
