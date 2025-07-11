use darling::util::Flag;
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use syn::{Expr, Ident, Type, Visibility};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(builder))]
pub struct BuilderAttrs {
    // darling recognizes `vis` as matching the struct visibility which i don't want
    #[darling(rename = "vis")]
    pub m_vis: Option<Visibility>,
    pub rename: Option<Ident>,
    #[darling(default)]
    pub unchecked: BuilderUncheckedAttrs,
    pub default: Flag,
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
    pub default: Option<Expr>,
    pub vis: Option<Visibility>,
}

pub struct FieldInfo {
    pub ident: Ident,
    pub name: Ident,
    pub gen_name: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub vis: Visibility,
    pub doc: String,
}

pub trait FieldInfoSliceExt {
    fn gen_names(&self) -> impl Iterator<Item = &Ident>;
    fn idents(&self) -> impl Iterator<Item = &Ident>;
}

impl FieldInfoSliceExt for [FieldInfo] {
    fn gen_names(&self) -> impl Iterator<Item = &Ident> {
        self.iter().map(|t| &t.gen_name)
    }

    fn idents(&self) -> impl Iterator<Item = &Ident> {
        self.iter().map(|t| &t.ident)
    }
}
